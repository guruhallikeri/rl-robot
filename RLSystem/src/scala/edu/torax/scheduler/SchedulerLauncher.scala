package edu.torax.scheduler

object SchedulerLauncher {
  def main(args : Array[String]) : Unit = {
    val maxW = 10
    val maxT = 1000000
    val sch = new Scheduler[Task, MyWorker, Task](maxW, new MyListener(maxT)) {
      private var workerCount = 0
      protected def makeWorker = {
        workerCount += 1
        new MyWorker(workerCount)
      }  
    }
    
    for (i <- 1 to maxT) {
      sch ! NewTask(Task(i, 200 + (Math.random*500).toInt))
    }
  }
}
