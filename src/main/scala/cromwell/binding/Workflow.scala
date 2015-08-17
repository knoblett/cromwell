package cromwell.binding

import cromwell.binding.AstTools.{AstNodeName, EnhancedAstNode}
import cromwell.binding.types.WdlType
import cromwell.parser.WdlParser.{Ast, SyntaxError, Terminal}

object Workflow {
  def apply(ast: Ast, wdlSyntaxErrorFormatter: WdlSyntaxErrorFormatter, calls: Seq[Call]): Workflow = {
    val name = ast.getAttribute("name").asInstanceOf[Terminal].getSourceString
    val declarations = ast.findAsts(AstNodeName.Declaration).map(Declaration(_, name, wdlSyntaxErrorFormatter))
    val callNames = ast.findAsts(AstNodeName.Call).map {call =>
      Option(call.getAttribute("alias")).getOrElse(call.getAttribute("task"))
    }

    val workflowOutputsDecls = ast.findAsts(AstNodeName.WorkflowOutput) map { wfOutput =>
      val wildcard = Option(wfOutput.getAttribute("wildcard")).map(_.sourceString).getOrElse("").nonEmpty
      WorkflowOutputDeclaration(wfOutput.getAttribute("fqn").sourceString, wildcard)
    }
    callNames.groupBy(_.sourceString) foreach {
      case (name, terminals) if terminals.size > 1 =>
        throw new SyntaxError(wdlSyntaxErrorFormatter.multipleCallsAndHaveSameName(terminals.asInstanceOf[Seq[Terminal]]))
      case _ =>
    }
    new Workflow(name, declarations, calls, workflowOutputsDecls)
  }
}

/**
 * Represents a `workflow` definition in WDL which currently
 * only supports a set of `call` declarations and a name for
 * the workflow
 *
 * @param name The name of the workflow
 * @param calls The set of `call` declarations
 */
case class Workflow(name: String, declarations: Seq[Declaration], calls: Seq[Call], workflowOutputDecls: Seq[WorkflowOutputDeclaration]) extends Executable with Scope {
  calls foreach {c => c.setParent(this)}

  /** Parent node for this workflow.  Since we do not support nested
    * workflows currently, this is always `None`
    */
  val parent: Option[Scope] = None

  /**
   * All inputs for this workflow and their associated types.
   *
   * @return a Seq[WorkflowInput] representing the
   *         inputs that the user needs to provide to this workflow
   */
  def inputs: Seq[WorkflowInput] = {
    val callInputs = for {call <- calls; input <- call.unsatisfiedInputs} yield input
    val declarationInputs = for(declaration <- declarations; input <- declaration.asWorkflowInput) yield input
    callInputs ++ declarationInputs
  }

  /**
   * All outputs for this workflow and their associated types
   *
   * @return a Map[FullyQualifiedName, WdlType] representing the union
   *         of all outputs from all `call`s within this workflow
   */
  def outputs: Map[FullyQualifiedName, WdlType] = {
    val outputs = for (call <- calls; output <- call.task.outputs) yield (s"${call.fullyQualifiedName}.${output.name}", output.wdlType)
    outputs.toMap
  }
}
