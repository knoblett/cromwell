package cromwell.binding

import cromwell.binding.WdlExpression.ScopedLookupFunction
import cromwell.binding.formatter.{NullSyntaxHighlighter, SyntaxHighlighter}
import cromwell.binding.types._
import cromwell.binding.values._
import cromwell.parser.WdlParser
import cromwell.parser.WdlParser.{Ast, AstList, AstNode, Terminal}
import cromwell.binding.AstTools.EnhancedAstNode

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

class WdlExpressionException(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause)

object WdlExpression {
  val parser = new WdlParser()

  /** Maps from a locally qualified name to a WdlValue. */
  type ScopedLookupFunction = String => WdlValue

  def binaryOperators = Set(
    "Add", "Subtract", "Multiply", "Divide", "Remainder",
    "GreaterThan", "LessThan", "GreaterThanOrEqual", "LessThanOrEqual",
    "Equals", "NotEquals", "LogicalAnd", "LogicalOr"
  )

  val unaryOperators = Set("LogicalNot", "UnaryPlus", "UnaryNegation")

  val preEvaluableFunctions_SingleParameter: Seq[String] = Seq("read_int", "read_string")

  def functionCall(a: Ast): Boolean = a.getName == "FunctionCall"

  def binaryOperator(a: Ast): Boolean = binaryOperators.contains(a.getName)
  
  def unaryOperator(a: Ast): Boolean = unaryOperators.contains(a.getName)
  
  def functionCallWithOneFileParameter(a: Ast): Boolean = (
    functionCall(a)
    && a.getAttribute("params").astListAsVector.size == 1
    && preEvaluableFunctions_SingleParameter.contains(a.getAttribute("name").asInstanceOf[Terminal].getSourceString))
  
  /**
   * Look within the expression for filenames which aren't explicitly listed as outputs. 
   */
  def preevaluateExpressionForFilenames(ast: AstNode, lookup: ScopedLookupFunction, functions: WdlFunctions): Try[Seq[WdlFile]] = {
    ast match {
      // This is the only case which actually pre-evaluates anything. The other cases are just buck-passers:
      case a: Ast if functionCallWithOneFileParameter(a) => {
        // Test the pre-evaluation would be valid by using dummy functions:
        val innerExpression = a.getAttribute("params").astListAsVector.head
        val dummyEvaluation = evaluate(innerExpression, lookup, new DummyPreEvaluationFunctions())
        // If dummyEvaluation succeeded, run the real evaluation instead and match against it:
        dummyEvaluation.flatMap( _ => evaluate(innerExpression, lookup, functions)) match {
          case Success(value) => Success(Seq(WdlFile(value.valueString)))
          case Failure(error) => Failure(error)
        }
      }
      // Binary operators - find filenames in sub-expressions and merge the lists:
      case a: Ast if binaryOperator(a) => {
        val lhs = preevaluateExpressionForFilenames(a.getAttribute("lhs"), lookup, functions)
        val rhs = preevaluateExpressionForFilenames(a.getAttribute("rhs"), lookup, functions)
        // Recurse both sides and add the lists together:
        lhs match {
          case Success(lhsValue) => {
            rhs match {
              case Success(rhsValue) => {
                Success(Seq(lhsValue,rhsValue).flatten)
              }
              case Failure(error) => Failure(error)
            }
          }
          case Failure(error) => Failure(error)
        }
      }
      case a: Ast if unaryOperator(a) => preevaluateExpressionForFilenames(a.getAttribute("expression"), lookup, functions)

      case _ => Success(Seq())
    }
  }

  private def replaceInterpolationTag(string: String, tag: String, lookup: ScopedLookupFunction) =
    string.replace(tag, lookup(tag.substring(2, tag.length - 1)).valueString)

  def interpolate(str: String, lookup: ScopedLookupFunction): String =
    "\\$\\{([a-zA-Z]([a-zA-Z0-9_])*)\\}".r.findAllIn(str).foldLeft(str) {replaceInterpolationTag(_, _, lookup)}

  def evaluate(ast: AstNode, lookup: ScopedLookupFunction, functions: WdlFunctions, interpolateStrings: Boolean = false): Try[WdlValue] = {
    ast match {
      case t: Terminal if t.getTerminalStr == "identifier" => Try(lookup(t.getSourceString))
      case t: Terminal if t.getTerminalStr == "integer" => Success(WdlInteger(t.getSourceString.toInt))
      case t: Terminal if t.getTerminalStr == "float" => Success(WdlFloat(t.getSourceString.toDouble))
      case t: Terminal if t.getTerminalStr == "boolean" => Success(WdlBoolean(t.getSourceString == "true"))
      case t: Terminal if t.getTerminalStr == "string" =>
        val strValue = if (interpolateStrings) interpolate(t.getSourceString, lookup) else t.getSourceString
        Success(WdlString(strValue))
      case a: Ast if binaryOperators.contains(a.getName) =>
        val lhs = evaluate(a.getAttribute("lhs"), lookup, functions, interpolateStrings)
        val rhs = evaluate(a.getAttribute("rhs"), lookup, functions, interpolateStrings)
        a.getName match {
          case "Add" => for(l <- lhs; r <- rhs) yield l.add(r).get
          case "Subtract" => for(l <- lhs; r <- rhs) yield l.subtract(r).get
          case "Multiply" => for(l <- lhs; r <- rhs) yield l.multiply(r).get
          case "Divide" => for(l <- lhs; r <- rhs) yield l.divide(r).get
          case "Remainder" => for(l <- lhs; r <- rhs) yield l.mod(r).get
          case "Equals" => for(l <- lhs; r <- rhs) yield l.equals(r).get
          case "NotEquals" => for(l <- lhs; r <- rhs) yield l.notEquals(r).get
          case "LessThan" => for(l <- lhs; r <- rhs) yield l.lessThan(r).get
          case "LessThanOrEqual" => for(l <- lhs; r <- rhs) yield l.lessThanOrEqual(r).get
          case "GreaterThan" => for(l <- lhs; r <- rhs) yield l.greaterThan(r).get
          case "GreaterThanOrEqual" => for(l <- lhs; r <- rhs) yield l.greaterThanOrEqual(r).get
          case "LogicalOr" => for(l <- lhs; r <- rhs) yield l.or(r).get
          case "LogicalAnd" => for(l <- lhs; r <- rhs) yield l.and(r).get
          case _ => Failure(new WdlExpressionException(s"Invalid operator: ${a.getName}"))
        }
      case a: Ast if unaryOperators.contains(a.getName) =>
        val expression = evaluate(a.getAttribute("expression"), lookup, functions, interpolateStrings)
        a.getName match {
          case "LogicalNot" => for(e <- expression) yield e.not.get
          case "UnaryPlus" => for(e <- expression) yield e.unaryPlus.get
          case "UnaryNegation" => for(e <- expression) yield e.unaryMinus.get
          case _ => Failure(new WdlExpressionException(s"Invalid operator: ${a.getName}"))
        }
      case a: Ast if a.getName == "ArrayLiteral" =>
        val evaluatedElements = a.getAttribute("values").astListAsVector map {x =>
          evaluate(x, lookup, functions, interpolateStrings)
        }
        evaluatedElements.partition {_.isSuccess} match {
          case (_, failures) if failures.nonEmpty =>
            val message = failures.collect {case f: Failure[_] => f.exception.getMessage}.mkString("\n")
            Failure(new WdlExpressionException(s"Could not evaluate expression:\n$message"))
          case (successes, _) =>
            for (subtype <- WdlType.areTypesHomogeneous(successes.map(_.get)))
              yield WdlArray(WdlArrayType(subtype), successes.map(_.get))
        }
      case a: Ast if a.getName == "MapLiteral" =>
        val evaluatedMap = a.getAttribute("map").astListAsVector map {kv =>
          val key = evaluate(kv.asInstanceOf[Ast].getAttribute("key"), lookup, functions, interpolateStrings)
          val value = evaluate(kv.asInstanceOf[Ast].getAttribute("value"), lookup, functions, interpolateStrings)
          key -> value
        }
        val flattenedTries = evaluatedMap flatMap { case (k,v) => Seq(k,v) }

        flattenedTries partition {_.isSuccess} match {
          case (_, failures) if failures.nonEmpty =>
            val message = failures.collect { case f: Failure[_] => f.exception.getMessage }.mkString("\n")
            Failure(new WdlExpressionException(s"Could not evaluate expression:\n$message"))
          case (successes, _) =>
            WdlMapType(WdlAnyType, WdlAnyType).coerceRawValue(evaluatedMap.map({ case (k, v) => k.get -> v.get }).toMap)
        }
      case a: Ast if a.getName == "MemberAccess" =>
        a.getAttribute("rhs") match {
          case rhs:Terminal if rhs.getTerminalStr == "identifier" =>
            evaluate(a.getAttribute("lhs"), lookup, functions, interpolateStrings).flatMap {
              case o: WdlObject =>
                o.value.get(rhs.getSourceString) match {
                  case Some(v:WdlValue) => Success(v)
                  case None => Failure(new WdlExpressionException(s"Could not find key ${rhs.getSourceString}"))
                }
              case ns: WdlNamespace => Success(lookup(ns.importedAs.map {n => s"$n.${rhs.getSourceString}"}.getOrElse(rhs.getSourceString)))
              case _ => Failure(new WdlExpressionException("Left-hand side of expression must be a WdlObject or Namespace"))
            }
          case _ => Failure(new WdlExpressionException("Right-hand side of expression must be identifier"))
        }
      case a: Ast if functionCall(a) =>
        val name = a.getAttribute("name").asInstanceOf[Terminal].getSourceString
        val params = a.getAttribute("params").astListAsVector map {
          evaluate(_, lookup, functions, interpolateStrings)
        }
        functions.getFunction(name)(params)
    }
  }

  def fromString(expression: WdlSource): WdlExpression = {
    val tokens = parser.lex(expression, "string")
    val terminalMap = (tokens.asScala.toVector map {(_, expression)}).toMap
    val parseTree = parser.parse_e(tokens, new WdlSyntaxErrorFormatter(terminalMap))
    new WdlExpression(parseTree.toAst)
  }

  def toString(ast: AstNode, highlighter: SyntaxHighlighter = NullSyntaxHighlighter): String = {
    ast match {
      case t: Terminal if t.getTerminalStr == "identifier" => t.getSourceString
      case t: Terminal if t.getTerminalStr == "integer" => t.getSourceString
      case t: Terminal if t.getTerminalStr == "float" => t.getSourceString
      case t: Terminal if t.getTerminalStr == "string" => s""""${t.getSourceString}""""
      case a:Ast if binaryOperators.contains(a.getName) => {
        val lhs = toString(a.getAttribute("lhs"), highlighter)
        val rhs = toString(a.getAttribute("rhs"), highlighter)
        a.getName match {
          case "Add" => s"$lhs + $rhs"
          case "Subtract" => s"$lhs - $rhs"
          case "Multiply" => s"$lhs * $rhs"
          case "Divide" => s"$lhs / $rhs"
          case "Remainder" => s"$lhs % $rhs"
          case "Equals" => s"$lhs == $rhs"
          case "NotEquals" => s"$lhs != $rhs"
          case "LessThan" => s"$lhs < $rhs"
          case "LessThanOrEqual" => s"$lhs <= $rhs"
          case "GreaterThan" => s"$lhs > $rhs"
          case "GreaterThanOrEqual" => s"$lhs >= $rhs"
          case "LogicalOr" => s"$lhs || $rhs"
          case "LogicalAnd" => s"$lhs && $rhs"
        }
      }
      case a:Ast if a.getName == "FunctionCall" => {
        val name = a.getAttribute("name").asInstanceOf[Terminal].getSourceString
        val params = a.getAttribute("params").astListAsVector map {a => toString(a, highlighter)}
        s"${highlighter.function(name)}(${params.mkString(", ")})"
      }
      case a:Ast if a.getName == "MemberAccess" => {
        val lhs = toString(a.getAttribute("lhs"), highlighter)
        val rhs = toString(a.getAttribute("rhs"), highlighter)
        s"$lhs.$rhs"
      }
    }
  }
}

case class WdlExpression(ast: AstNode) extends WdlValue {
  override val wdlType = WdlExpressionType
  def evaluate(lookup: ScopedLookupFunction, functions: WdlFunctions, interpolateStrings: Boolean = false): Try[WdlValue] =
    WdlExpression.evaluate(ast, lookup, functions, interpolateStrings)
  def preevaluateExpressionForFilenames(lookup: ScopedLookupFunction, functions: WdlFunctions): Try[Seq[WdlFile]] =
    WdlExpression.preevaluateExpressionForFilenames(ast, lookup: ScopedLookupFunction, functions: WdlFunctions)
  def toString(highlighter: SyntaxHighlighter): String = {
    WdlExpression.toString(ast, highlighter)
  }
  override def toWdlString: String = toString(NullSyntaxHighlighter)
}

trait WdlFunctions {
  type WdlFunction = Seq[Try[WdlValue]] => Try[WdlValue]

  def getFunction(name: String): WdlFunction
}

class NoFunctions extends WdlStandardLibraryFunctions

class DummyPreEvaluationFunctions extends WdlStandardLibraryFunctions {
  override protected def stdout(params: Seq[Try[WdlValue]]): Try[WdlFile] = Success(WdlFile("/test/value"))
  override protected def stderr(params: Seq[Try[WdlValue]]): Try[WdlFile] = Success(WdlFile("/test/value"))
}