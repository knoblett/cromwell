package cromwell.core

import org.scalatest.{FlatSpec, Matchers}
import spray.json.DefaultJsonProtocol._
import spray.json._

import scalaz._

class WorkflowSourceFilesSpec extends FlatSpec with Matchers {

  behavior of "WorkflowSourceFiles"

  it should "successfully merge and override multiple input files" in {
    val input1 = Map("wf.a1" -> "hello", "wf.a2" -> "world").toJson.toString
    val input2 = Map.empty[String, String].toJson.toString
    val overrideInput1 = Map("wf.a2" -> "universe").toJson.toString
    val allInputs = WorkflowSourceFiles.mergeInputs(NonEmptyList(input1, input2, overrideInput1)).parseJson.asJsObject

    allInputs.fields.keys should contain allOf("wf.a1", "wf.a2")
    allInputs.fields("wf.a2") should be(JsString("universe"))
  }
}
