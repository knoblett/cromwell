package cromwell.backend.sfs

import akka.actor.{Actor, ActorRef}
import cromwell.backend.BackendInitializationData
import cromwell.backend.callcaching.JobCachingActorHelper
import cromwell.backend.io.JobPaths
import cromwell.backend.validation.{RuntimeAttributesValidation, ValidatedRuntimeAttributes}
import cromwell.core.logging.JobLogging
import lenthall.config.ScalaConfig._

trait SharedFileSystemJobCachingActorHelper extends JobCachingActorHelper {
  this: Actor with JobLogging =>

  def backendInitializationDataOption: Option[BackendInitializationData]

  def serviceRegistryActor: ActorRef

  lazy val jobPaths =
    new JobPaths(jobDescriptor.workflowDescriptor, configurationDescriptor.backendConfig, jobDescriptor.key)

  lazy val initializationData = BackendInitializationData.
    as[SharedFileSystemBackendInitializationData](backendInitializationDataOption)

  lazy val validatedRuntimeAttributes: ValidatedRuntimeAttributes = {
    val builder = initializationData.runtimeAttributesBuilder
    builder.build(jobDescriptor.runtimeAttributes, jobLogger)
  }

  lazy val metadataKeyValues: Map[String, Any] = {
    val runtimeAttributesMetadata = RuntimeAttributesValidation.extract(validatedRuntimeAttributes) map {
      case (key, value) => (s"runtimeAttributes:$key", value)
    }
    val fileMetadata = jobPaths.metadataPaths
    val otherMetadata = Map("cache:allowResultReuse" -> true)
    runtimeAttributesMetadata ++ fileMetadata ++ otherMetadata
  }

  lazy val sharedFileSystem = new SharedFileSystem {
    override lazy val sharedFileSystemConfig = {
      configurationDescriptor.backendConfig.getConfigOr("filesystems.local")
    }
  }
}
