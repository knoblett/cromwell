package cromwell.engine.backend.jes

import java.util.Date

import com.google.api.services.genomics.model.{Logging, RunPipelineRequest, ServiceAccount, CancelOperationRequest}
import cromwell.engine.backend.jes.JesBackend.JesParameter
import cromwell.engine.backend.jes.Run.{Failed, Running, Success, _}
import cromwell.util.google.GoogleScopes
import org.joda.time.DateTime
import org.slf4j.LoggerFactory

import scala.annotation.tailrec
import scala.collection.JavaConverters._

object Run  {
  val JesServiceAccount = new ServiceAccount().setEmail("default").setScopes(GoogleScopes.Scopes.asJava)
  lazy val Log = LoggerFactory.getLogger("main")

  def apply(pipeline: Pipeline): Run = {
    val rpr = new RunPipelineRequest().setPipelineId(pipeline.id).setProjectId(pipeline.projectId).setServiceAccount(JesServiceAccount)
    val tag = s"JES Run [UUID(${pipeline.workflow.shortId}):${pipeline.call.name}]"

    rpr.setInputs(pipeline.jesParameters.filter(_.isInput).toRunMap)
    Log.info(s"$tag inputs are ${rpr.getInputs}")
    rpr.setOutputs(pipeline.jesParameters.filter(_.isOutput).toRunMap)
    Log.info(s"$tag outputs are ${rpr.getOutputs}")

    val logging = new Logging()
    logging.setGcsPath(pipeline.gcsPath)
    rpr.setLogging(logging)

    val id = pipeline.genomicsService.pipelines().run(rpr).execute().getName
    Log.info(s"$tag ID is $id")
    new Run(id, pipeline)
  }

  implicit class RunJesParameters(val params: Seq[JesParameter]) extends AnyVal {
    def toRunMap = params.map(p => p.name -> p.gcs).toMap.asJava
  }

  sealed trait RunStatus
  trait TerminalRunStatus extends RunStatus
  final case class Initializing(created: DateTime) extends RunStatus
  final case class Running(created: DateTime, started: DateTime) extends RunStatus
  final case class Success(created: DateTime, started: DateTime, finished: DateTime) extends TerminalRunStatus
  final case class Failed(created: DateTime, started: DateTime, finished: DateTime, errorCode: Int, errorMessage: String) extends TerminalRunStatus
}

case class Run(name: String, pipeline: Pipeline) {
  def status(): RunStatus = {
    val op = pipeline.genomicsService.operations().get(name).execute
    val metadata = OperationMetadata(op)

    if (op.getDone) {
      val error = Option(op.getError)
      error match {
        case Some(x) => Failed(
          metadata.created,
          metadata.started.get,
          metadata.finished.get,
          x.getCode,
          x.getMessage)
        case None => Success(metadata.created, metadata.started.get, metadata.finished.get)
      }
    } else metadata.started map {s => Running(metadata.created, s)} getOrElse Initializing(metadata.created)
  }

  @tailrec
  final def waitUntilComplete(): TerminalRunStatus = {
    val currentStatus = status()
    println(s"Current status is $currentStatus")
    currentStatus match {
      case x: TerminalRunStatus => x
      case _ =>
        Thread.sleep(5000)
        waitUntilComplete()
    }
  }

  @tailrec
  final def waitUntilRunningOrComplete(): Unit = {
    val currentStatus = status()
    println(s"Current status is $currentStatus")
    currentStatus match {
      case x: Running =>
      case x: TerminalRunStatus =>
      case _ =>
        Thread.sleep(5000)
        waitUntilRunningOrComplete()
    }
  }

  def abort(): Unit = {
    val cancellationRequest: CancelOperationRequest = new CancelOperationRequest()
    val executed = pipeline.genomicsService.operations().cancel(name, cancellationRequest).execute
  }
}
