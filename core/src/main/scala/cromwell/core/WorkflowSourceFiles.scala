package cromwell.core

import spray.json._
import DefaultJsonProtocol._
import wdl4s.{WdlJson, WdlSource}

import scalaz.NonEmptyList

/**
  * Represents the collection of source files that a user submits to run a workflow
  */
final case class WorkflowSourceFiles(wdlSource: WdlSource, inputsJsons: NonEmptyList[WdlJson],
                                     workflowOptionsJson: WorkflowOptionsJson) {
  lazy val inputsJson: WdlJson = WorkflowSourceFiles.mergeInputs(inputsJsons)
}

object WorkflowSourceFiles {
  def apply(wdlSource: WdlSource, inputsJson: WdlJson,
            workflowOptionsJson: WorkflowOptionsJson): WorkflowSourceFiles = {
    WorkflowSourceFiles(wdlSource, NonEmptyList(inputsJson), workflowOptionsJson)
  }

  def mergeInputs(inputsJsons: NonEmptyList[WdlJson]): WdlJson = {
    import spray.json._
    if (inputsJsons.size == 1) {
      inputsJsons.head
    } else {
      val inputsMap = inputsJsons.map(WorkflowSourceFiles.parseInputs).list.toList.reduce(_ ++ _)
      inputsMap.toJson.compactPrint
    }
  }

  def parseInputs(inputs: WdlJson): Map[String, JsValue] = {
    import spray.json._
    inputs.parseJson match {
      case JsObject(inputMap) => inputMap
      case _ => throw new RuntimeException(s"Inputs is not a json object: $inputs")
    }
  }
}
