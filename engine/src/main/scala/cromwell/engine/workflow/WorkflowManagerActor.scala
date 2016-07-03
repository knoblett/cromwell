package cromwell.engine.workflow

import akka.actor.FSM.{CurrentState, SubscribeTransitionCallBack, Transition}
import akka.actor._
import akka.event.Logging
import akka.routing.FromConfig
import com.typesafe.config.{Config, ConfigFactory}
import cromwell.core.WorkflowId
import cromwell.engine._
import cromwell.engine.workflow.WorkflowActor._
import cromwell.engine.workflow.WorkflowManagerActor._
import cromwell.engine.workflow.lifecycle.CopyWorkflowLogsActor
import cromwell.services.MetadataServiceActor._
import cromwell.services.ServiceRegistryClient
import cromwell.webservice.CromwellApiHandler._
import lenthall.config.ScalaConfig.EnhancedScalaConfig

import scala.concurrent.duration._
import scala.concurrent.{Await, Promise}
import scala.language.postfixOps

object WorkflowManagerActor {
  val DefaultMaxWorkflowsToLaunch = 50
  val DefaultNewWorkflowPollRate = 20

  case class WorkflowIdToActorRef(workflowId: WorkflowId, workflowActor: ActorRef)
  class WorkflowNotFoundException(s: String) extends Exception(s)

  sealed trait WorkflowManagerActorMessage
  /**
    * Commands
    */
  sealed trait WorkflowManagerActorCommand extends WorkflowManagerActorMessage
  case object RetrieveNewWorkflows extends WorkflowManagerActorCommand
  case class AbortWorkflowCommand(id: WorkflowId) extends WorkflowManagerActorCommand
  case object AbortAllWorkflowsCommand extends WorkflowManagerActorCommand
  case class SubscribeToWorkflowCommand(id: WorkflowId) extends WorkflowManagerActorCommand

  /**
    * Responses
    */
  sealed trait WorkflowManagerActorResponse extends WorkflowManagerActorMessage

  def props(workflowStore: ActorRef): Props = Props(new WorkflowManagerActor(workflowStore))

  /**
    * States
    */
  sealed trait WorkflowManagerState
  case object Running extends WorkflowManagerState
  case object Aborting extends WorkflowManagerState
  case object Done extends WorkflowManagerState

  /**
    * Data
    */
  case class WorkflowManagerData(workflows: Map[WorkflowId, ActorRef]) {
    def idFromActor(actor: ActorRef): Option[WorkflowId] = workflows.collectFirst { case (id, a) if a == actor => id }
    def withAddition(entry: WorkflowIdToActorRef): WorkflowManagerData =
      this.copy(workflows = workflows + (entry.workflowId -> entry.workflowActor))

    def withAddition(entries: List[WorkflowIdToActorRef]): WorkflowManagerData = {
      val entryTuples = entries map { e => e.workflowId -> e.workflowActor }
      this.copy(workflows = workflows ++ entryTuples)
    }

    def without(id: WorkflowId): WorkflowManagerData = this.copy(workflows = workflows - id)
    def without(actor: ActorRef): WorkflowManagerData = {
      // If the ID was found in the lookup return a modified copy of the state data, otherwise just return
      // the same state data.
      idFromActor(actor) map without getOrElse this
    }
  }
}

class WorkflowManagerActor(config: Config, workflowStore: ActorRef)
  extends LoggingFSM[WorkflowManagerState, WorkflowManagerData] with CromwellActor with ServiceRegistryClient {

  def this(workflowStore: ActorRef) = this(ConfigFactory.load, workflowStore)
  implicit val actorSystem = context.system

  private val maxWorkflowsToLaunch = config.getConfig("system").getIntOr("max-workflows-to-launch", default=DefaultMaxWorkflowsToLaunch)
  private val newWorkflowPollRate = config.getConfig("system").getIntOr("new-workflow-poll-rate", default=DefaultNewWorkflowPollRate).seconds

  private val restartDelay: FiniteDuration = 200 milliseconds
  private val logger = Logging(context.system, this)
  private val tag = self.path.name

  private val donePromise = Promise[Unit]()

  private val workflowLogCopyRouter: ActorRef = {
    context.actorOf(FromConfig.withSupervisorStrategy(CopyWorkflowLogsActor.strategy).props(Props[CopyWorkflowLogsActor].withDispatcher("akka.dispatchers.slow-actor-dispatcher")), "WorkflowLogCopyRouter")
  }

  override def preStart() {
    addShutdownHook()
    // Starts the workflow polling cycle
    self ! RetrieveNewWorkflows
  }

  private def addShutdownHook(): Unit = {
    // Only abort jobs on SIGINT if the config explicitly sets backend.abortJobsOnTerminate = true.
    val abortJobsOnTerminate =
      config.getConfig("system").getBooleanOr("abort-jobs-on-terminate", default = false)

    if (abortJobsOnTerminate) {
      sys.addShutdownHook {
        logger.info(s"$tag: Received shutdown signal. Aborting all running workflows...")
        self ! AbortAllWorkflowsCommand
        Await.ready(donePromise.future, Duration.Inf)
      }
    }
  }

  startWith(Running, WorkflowManagerData(workflows = Map.empty))

  when (Running) {
    /*
     Commands from clients
     */
    case Event(RetrieveNewWorkflows, stateData) =>
      workflowStore ! WorkflowStoreActor.FetchRunnableWorkflows(maxWorkflowsToLaunch)
      stay()
    case Event(WorkflowStoreActor.NewWorkflows(newWorkflows), stateData) =>
      val newSubmissions = newWorkflows map submitWorkflow
      log.debug(s"Retrieved ${newSubmissions.size} workflows from the WorkflowStoreActor")
      context.system.scheduler.scheduleOnce(newWorkflowPollRate, self, RetrieveNewWorkflows)(context.dispatcher)
      stay() using stateData.withAddition(newSubmissions)
    case Event(SubscribeToWorkflowCommand(id), data) =>
      data.workflows.get(id) foreach {_ ! SubscribeTransitionCallBack(sender())}
      stay()
    case Event(WorkflowManagerActor.AbortWorkflowCommand(id), stateData) =>
      val workflowActor = stateData.workflows.get(id)
      workflowActor match {
        case Some(actor) =>
          actor ! WorkflowActor.AbortWorkflowCommand
          stay()
        case None =>
          sender ! WorkflowManagerAbortFailure(id, new WorkflowNotFoundException(s"Couldn't abort $id because no workflow with that ID is in progress"))
          stay()
      }
    case Event(AbortAllWorkflowsCommand, data) if data.workflows.isEmpty =>
      goto(Done)
    case Event(AbortAllWorkflowsCommand, data) =>
      log.info(s"$tag Aborting all workflows")
      data.workflows.values.foreach { _ ! WorkflowActor.AbortWorkflowCommand }
      goto(Aborting)
    /*
     Responses from services
     */
    case Event(WorkflowSucceededResponse(workflowId), data) =>
      log.info(s"$tag Workflow $workflowId succeeded!")
      stay()
    case Event(WorkflowFailedResponse(workflowId, inState, reasons), data) =>
      log.error(s"$tag Workflow $workflowId failed (during $inState): ${reasons.mkString("\n")}")
      stay()
    /*
     Watched transitions
     */
    case Event(Transition(workflowActor, _, toState: WorkflowActorTerminalState), data) =>
      log.info(s"$tag ${workflowActor.path.name} is in a terminal state: $toState")
      // This silently fails if idFromActor is None, but data.without call right below will as well
      data.idFromActor(workflowActor) foreach { workflowStore ! WorkflowStoreActor.RemoveWorkflow(_) }
      stay using data.without(workflowActor)
  }

  when (Aborting) {
    case Event(Transition(workflowActor, _, toState: WorkflowActorState), data) if toState.terminal =>
      // Remove this terminal actor from the workflowStore and log a progress message.
      val updatedData = data.without(workflowActor)
      // If there are no more workflows to abort we're done, otherwise just stay in the current state.
      val resultAction = if (updatedData.workflows.isEmpty) {
        logger.info(s"$tag All workflows are aborted")
        goto(Done)
      } else {
        logger.info(s"$tag Waiting for all workflows to abort (${updatedData.workflows.size} remaining).")
        stay()
      }
      // Whatever the result action, use the updated data:
      resultAction using updatedData
    case Event(_, _) => stay()
  }

  when (Done) { FSM.NullFunction }

  whenUnhandled {
    case Event(MetadataPutFailed(action, error), _) =>
      log.warning(s"$tag Put failed for Metadata action $action : ${error.getMessage}")
      stay()
    case Event(MetadataPutAcknowledgement(_), _) => stay()
    // Uninteresting transition and current state notifications.
    case Event((Transition(_, _, _) | CurrentState(_, _)), _) => stay()
    // Anything else certainly IS interesting:
    case Event(unhandled, data) =>
      log.warning(s"$tag Unhandled message: $unhandled")
      stay()
  }

  onTransition {
    case _ -> Done =>
      logger.info(s"$tag All workflows finished. Shutting down.")
      donePromise.trySuccess(())
    case fromState -> toState =>
      logger.info(s"$tag transitioning from $fromState to $toState")
  }

  /**
    * Submit the workflow and return an updated copy of the state data reflecting the addition of a
    * Workflow ID -> WorkflowActorRef entry.
    */
  private def submitWorkflow(workflow: WorkflowStore.WorkflowToStart): WorkflowIdToActorRef = {
    val workflowId = workflow.id

    val startMode = if (workflow.state == WorkflowStore.Restartable) {
      logger.info(s"$tag Restarting workflow UUID($workflowId)")
      RestartExistingWorkflow
    } else {
      logger.info(s"$tag Starting workflow UUID($workflowId)")
      StartNewWorkflow
    }

    val wfProps = WorkflowActor.props(workflowId, startMode, workflow.sources, config, serviceRegistryActor, workflowLogCopyRouter)
    val wfActor = context.actorOf(wfProps, name = s"WorkflowActor-$workflowId")

    wfActor ! SubscribeTransitionCallBack(self)
    wfActor ! StartWorkflowCommand
    logger.info(s"$tag Successfully started ${wfActor.path.name}")
    WorkflowIdToActorRef(workflowId, wfActor)
  }
}

