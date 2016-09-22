package cromwell.backend.wfs

import com.typesafe.config.Config
import cromwell.backend.io.WorkflowPaths
import cromwell.backend.{BackendConfigurationDescriptor, BackendWorkflowDescriptor}
import cromwell.core.WorkflowOptions
import cromwell.core.path.PathBuilder
import lenthall.config.ScalaConfig._

import scala.concurrent.ExecutionContext

object WorkflowPathBuilder {
  def workflowPaths(configurationDescriptor: BackendConfigurationDescriptor,
                    workflowDescriptor: BackendWorkflowDescriptor,
                    pathBuilders: List[PathBuilder],
                    fileSystemExecutionContext: ExecutionContext): WorkflowPaths = {
    val backendConfig = configurationDescriptor.backendConfig
    val fileSystemConfig = backendConfig.getConfigOr("filesystems")
    val globalConfig = configurationDescriptor.globalConfig
    val params = WorkflowFileSystemProviderParams(fileSystemConfig, globalConfig, workflowDescriptor.workflowOptions,
      fileSystemExecutionContext)
    new WorkflowPaths(workflowDescriptor, configurationDescriptor.backendConfig, pathBuilders)
  }
}

final case class WorkflowFileSystemProviderParams(fileSystemConfig: Config, globalConfig: Config,
                                                  workflowOptions: WorkflowOptions,
                                                  fileSystemExecutionContext: ExecutionContext)

trait WorkflowPathBuilder {
  def pathBuilderOption(params: WorkflowFileSystemProviderParams): Option[PathBuilder]
}
