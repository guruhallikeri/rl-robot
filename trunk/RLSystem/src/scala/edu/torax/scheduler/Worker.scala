package edu.torax.scheduler
import scala.actors.Actor
import scala.actors.Actor._

trait Worker[T, TR] extends Actor {
  def processTask(task: T): TR
  
	def act() = loop {
	  react {
	    case ProcessTask(task, scheduler) =>
	      scheduler ! WorkerReady(this, processTask(task.asInstanceOf[T]))
	  }
	}
 
	start()
}
