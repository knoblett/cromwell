package cromwell.binding.formatter

import cromwell.binding.AstTools.EnhancedAstNode
import cromwell.binding._
import cromwell.binding.command.StringCommandPart
import cromwell.binding.types.WdlType
import cromwell.parser.WdlParser.{Ast, AstList, AstNode}
import cromwell.util.TerminalUtil

import scala.collection.JavaConverters._

trait SyntaxHighlighter {
  def keyword(s: String): String = s
  def name(s: String): String = s
  def section(s: String): String = s
  def wdlType(t: WdlType): String = t.toWdlString
  def variable(s: String): String = s
  def alias(s: String): String = s
  def command(s: String): String = s
  def function(s: String): String = s
}

object NullSyntaxHighlighter extends SyntaxHighlighter

object AnsiSyntaxHighlighter extends SyntaxHighlighter {
  override def keyword(s: String): String = TerminalUtil.highlight(214, s)
  override def name(s: String): String = TerminalUtil.highlight(253, s)
  override def section(s: String): String = keyword(s)
  override def wdlType(t: WdlType): String = TerminalUtil.highlight(33, t.toWdlString)
  override def variable(s: String): String = TerminalUtil.highlight(112, s)
  override def alias(s: String): String = s
  override def command(s: String): String = s
  override def function(s: String): String = TerminalUtil.highlight(13, s)
}

object HtmlSyntaxHighlighter extends SyntaxHighlighter {
  def wrap(s: String, cls: String) = s"""<span class="$cls">$s</span>"""
  override def keyword(s: String): String = wrap(s, "keyword")
  override def name(s: String): String = wrap(s, "name")
  override def section(s: String): String = wrap(s, "section")
  override def wdlType(t: WdlType): String = wrap(t.toWdlString, "type")
  override def variable(s: String): String = wrap(s, "variable")
  override def alias(s: String): String = wrap(s, "alias")
  override def command(s: String): String = wrap(s, "command")
  override def function(s: String): String = wrap(s, "function")
}

class SyntaxFormatter(highlighter: SyntaxHighlighter = NullSyntaxHighlighter) {
  val indentLevel = 2

  private def indent(s: String, i: Int): String = {
    s.split("\n").map {" " * (i * indentLevel) + _}.mkString("\n")
  }

  def format(namespace: WdlNamespace): String = {
    val imports = namespace.imports.map(formatImport) match {
      case v if v.nonEmpty => v.mkString("\n") + "\n\n"
      case v => ""
    }

    /*
     TODO/FIXME: If 'definitions' is really a function of `namespace` then `WdlNamespace should have a func which
     does the first part, and `NamespaceWithWorkflow` override it, call super and then adds on the second part
    */

    val namespaceDefinitions = namespace.ast.getAttribute("definitions").asInstanceOf[AstList].asScala.toVector

    val taskDefinitions = namespaceDefinitions collect { case a: Ast if a.getName == "Task" =>
      formatTask(namespace.findTask(a.getAttribute("name").sourceString()).getOrElse(throw new UnsupportedOperationException("Shouldn't happen")))
    }

    val workflowDefinitions = namespace match {
      case n: NamespaceWithWorkflow => namespaceDefinitions collect {case a: Ast if a.getName == "Workflow" => formatWorkflow(n.workflow)}
      case _ => Vector.empty[AstNode]
    }
    val definitions = taskDefinitions ++ workflowDefinitions

    s"$imports${definitions.mkString("\n\n")}"
  }

  private def formatImport(imp: Import): String = {
    val namespace = imp.namespace.map{ns => s" as $ns"}.getOrElse("")
    s"${highlighter.keyword("import")} '${imp.uri}'$namespace"
  }

  private def formatTask(task: Task): String = {
    val outputs = if (task.outputs.nonEmpty) formatOutputs(task.outputs, 1) else ""
    val command = formatCommandSection(task, 1)
    val declarations = task.declarations.map(formatDeclaration(_, 1)) match {
      case x: Seq[String] if x.nonEmpty => x.mkString("\n")
      case _ => ""
    }
    val sections = List(declarations, command, outputs).filter(_.nonEmpty)
    val header = s"""${highlighter.keyword("task")} ${highlighter.name(task.name)} {
       |${sections.mkString("\n")}
       |}"""
     .stripMargin
    header
  }

  private def formatCommandSection(task: Task, level:Int): String = {
    val (sdelim: String, edelim: String) =
      if (task.commandTemplate.collect({case s:StringCommandPart => s.literal}).mkString.contains("}")) ("<<<", ">>>")
      else ("{", "}")

    val section = s"""${highlighter.section("command")} $sdelim
        |${indent(highlighter.command(task.commandTemplateString), 1)}
        |$edelim"""
    indent(section.stripMargin, level)
  }

  private def formatOutputs(outputs: Seq[TaskOutput], level:Int): String = {
    val section = s"""${highlighter.section("output")} {
        |${outputs.map(formatOutput(_, 1)).mkString("\n")}
        |}"""
    indent(section.stripMargin, level)
  }

  private def formatOutput(output: TaskOutput, level:Int): String = {
    indent(s"${highlighter.wdlType(output.wdlType)} ${highlighter.variable(output.name)} = ${output.expression.toString(highlighter)}", level)
  }

  private def formatWorkflow(workflow: Workflow): String = {
    val declarations = workflow.declarations.map(formatDeclaration(_, 1))
    val calls = workflow.calls.map(formatCall(_, 1))
    val sections = (declarations ++ calls).filter(_.nonEmpty)
    s"""${highlighter.keyword("workflow")} ${highlighter.name(workflow.name)} {
        |${sections.mkString("\n")}
        |}""".stripMargin
  }

  private def formatDeclaration(decl: Declaration, level: Int): String = {
    val expression = decl.expression.map({e => s" = ${e.toWdlString}"}).getOrElse("")
    indent(s"${highlighter.wdlType(decl.wdlType)} ${highlighter.variable(decl.name)}$expression", level)
  }

  private def formatCall(call: Call, level:Int): String = {
    val header = s"${highlighter.keyword("call")} ${highlighter.name(call.task.name)}${formatCallAlias(call)}"
    if (call.inputMappings.isEmpty) {
      indent(header, level)
    } else {
      val inputString = call.inputMappings.map {case (k, v) =>
        s"$k=${v.toString(highlighter)}"
      }.mkString(", ")
      indent(s"""$header {
         |  input: $inputString
         |}""".stripMargin, level)
    }
  }

  private def formatCallAlias(call: Call): String = {
    call.alias.map {a => s" as ${highlighter.alias(a)}"}.getOrElse("")
  }
}
