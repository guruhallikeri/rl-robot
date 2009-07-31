package edu.torax.scheduler

case class Task(val id: Int, val pause: Int)

class MyWorker (val id: Int) extends Worker[Task, Task] {
  def processTask(task: Task): Task = {
    println("Processing task #" + task.id + "(" + task.pause + ") on Worker #" + id + "...")
    //Thread.sleep(task.pause)
    var sum = 0.0
    for (i <- 1 to task.pause) {
      sum += Math.sqrt(i) * i * i
    }
    task
  }
}

class MyListener(val allJobsCount: Int) extends Listener[Task] {
  val startTime = System.currentTimeMillis
  private var jobsDone = 0
  
  def taskDone(task: Task) {
    println("Task #" + task.id + " done.")
    jobsDone += 1
    if (jobsDone == allJobsCount) {
    	println("All tasks done in " + (System.currentTimeMillis - startTime)/1000.0 + " seconds.")
    }
  } 
}
