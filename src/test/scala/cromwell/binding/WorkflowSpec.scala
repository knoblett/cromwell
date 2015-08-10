package cromwell.binding

import cromwell.parser.BackendType
import cromwell.util.SampleWdl
import org.scalatest.{FlatSpec, Matchers}

class WorkflowSpec extends FlatSpec with Matchers {
  val namespace = NamespaceWithWorkflow.load(
    """workflow w {
      |  output {
      |    a.b,
      |    c.d.*,
      |    foo.bar.baz
      |  }
      |}""".stripMargin, BackendType.LOCAL)

  val workflowOutputDecls = namespace.workflow.workflowOutputDecls
  "Workflow" should "have proper number of workflow outputs defined" in {
    workflowOutputDecls.size shouldEqual 3
  }
  it should "should have correct output section" in {
    workflowOutputDecls(0) shouldEqual WorkflowOutputDeclaration("a.b", false)
    workflowOutputDecls(1) shouldEqual WorkflowOutputDeclaration("c.d", true)
    workflowOutputDecls(2) shouldEqual WorkflowOutputDeclaration("foo.bar.baz", false)
  }
}
