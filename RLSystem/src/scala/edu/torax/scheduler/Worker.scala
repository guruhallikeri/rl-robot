package edu.torax.scheduler
import scala.actors.Actor
import scala.actors.Actor._

trait Worker[T, TR] extends Actor {
  private case class Done(Scheduler: Actor, taskResult: TR)
  def processTask(task: T): TR
  
	def act() = loop {
	  react {
	    case ProcessTask(task, scheduler) =>
	      scheduler ! WorkerReady(this, processTask(task.asInstanceOf[T]))
//	      val who = this
//	      val tempActor = actor {
//	        who ! Done(scheduler, processTask(task.asInstanceOf[T]))
//	      }
	    case Done(scheduler, taskResult) =>
	      scheduler ! WorkerReady(this, taskResult)
	  }
	}
 
	start()
}
