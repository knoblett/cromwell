package cromwell.engine

import akka.actor.ActorSystem
import cats.data.Validated.{Invalid, Valid}
import com.typesafe.config.ConfigFactory
import cromwell.core.WorkflowOptions
import cromwell.core.path.{DefaultPathBuilder, PathBuilder}
import cromwell.filesystems.gcs.{GoogleConfiguration, RetryableGcsPathBuilderFactory}
import lenthall.config.ScalaConfig._
import lenthall.exception.MessageAggregation

case class EngineFilesystems(actorSystem: ActorSystem) {

  private val config = ConfigFactory.load
  private val googleConf: GoogleConfiguration = GoogleConfiguration(config)
  private val googleAuthMode = config.getStringOption("engine.filesystems.gcs.auth") map { confMode =>
    googleConf.auth(confMode) match {
      case Valid(mode) => mode
      case Invalid(errors) => throw new RuntimeException() with MessageAggregation {
        override def exceptionContext: String = s"Failed to create authentication mode for $confMode"
        override def errorMessages: Traversable[String] = errors.toList
      }
    }
  }

  private val gcsPathBuilderFactory = googleAuthMode map { RetryableGcsPathBuilderFactory(_) }

  def pathBuildersForWorkflow(workflowOptions: WorkflowOptions): List[PathBuilder] = {
    List(gcsPathBuilderFactory map { _.withOptions(workflowOptions) }, Option(DefaultPathBuilder)).flatten
  }
}
