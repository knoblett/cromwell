package cromwell.backend.io

import java.nio.file.{Path, Paths}

import com.typesafe.config.Config
import cromwell.backend.{BackendJobDescriptorKey, BackendWorkflowDescriptor}
import cromwell.core.path.{DefaultPathBuilder, PathBuilder, PathFactory}
import lenthall.config.ScalaConfig._

object WorkflowPaths{
  val DockerRoot = Paths.get("/root")
  val DefaultPathBuilders = List(DefaultPathBuilder)
}

class WorkflowPaths(workflowDescriptor: BackendWorkflowDescriptor, config: Config, val pathBuilders: List[PathBuilder] = WorkflowPaths.DefaultPathBuilders) extends PathFactory {
  val executionRoot = Paths.get(config.getStringOr("root", "cromwell-executions")).toAbsolutePath

  private def workflowPathBuilder(root: Path) = {
    root.resolve(workflowDescriptor.workflowNamespace.workflow.unqualifiedName)
        .resolve(workflowDescriptor.id.toString)
  }

  lazy val workflowRoot = workflowPathBuilder(executionRoot)
  lazy val dockerWorkflowRoot = workflowPathBuilder(WorkflowPaths.DockerRoot)

  def toJobPaths(jobKey: BackendJobDescriptorKey) = new JobPaths(workflowDescriptor, config, jobKey)
}
