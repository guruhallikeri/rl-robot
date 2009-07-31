package edu.torax.scheduler
import scala.actors.Actor
import scala.actors.Actor._
  
trait Listener[TR] extends Actor {
  def taskDone(taskResult: TR)
  
  def act() = loop {
    react {
      case TaskDone(task) => taskDone(task.asInstanceOf[TR])
    }
  }
  
  start()
}
