package edu.torax.scheduler
import scala.collection.mutable.Queue
import scala.actors.Actor
import scala.actors.Actor._

abstract sealed class SchedulerMessage
case class WorkerReady[T, TR](worker: Worker[T, TR], taskResult: TR) extends SchedulerMessage
case class NewTask[T](task: T) extends SchedulerMessage
case class ProcessTask[T, W <: Worker[T, TR], TR](task: T, scheduler: Scheduler[T, W, TR]) extends SchedulerMessage
case class TaskDone[TR](taskResult: TR) extends SchedulerMessage

abstract class Scheduler[T, W <: Worker[T, TR], TR] (val maxWorkers: Int, val listener: Listener[TR]) extends Actor {
	private val tasks = new Queue[T]()
	private val workers = new Queue[W]()
 
	protected def makeWorker: W
 
	for (i <- 1 to maxWorkers) {
	  workers.enqueue(makeWorker)
	}
 
	start()
 
	def act() = loop {
	  react {
	  	case WorkerReady(worker, taskResult) => 
	    	workers.enqueue(worker.asInstanceOf[W])
	    	listener ! TaskDone(taskResult.asInstanceOf[TR])
	    	checkState()
	  	case NewTask(task) =>
	  	  tasks.enqueue(task.asInstanceOf[T])
	  	  checkState()
	  }
	}
 
	private def checkState() {
	  if (!tasks.isEmpty && !workers.isEmpty) {
	    workers.dequeue() ! ProcessTask(tasks.dequeue(), this)
	  }
	}
}
