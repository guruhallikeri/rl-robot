package edu.torax.reinforcement.robot
import java.io.File

case class RobotTestDumpReport(testCaseNum: Int, attempt: Int, iterationsPassed: Int,
                               quantiles: Array[Int], 
                               reached: Int, crashed: Int, timedOut: Int)

object RobotTestCase {
  val NaN = 1000000000
  val quantiles = Array(10, 20, 30, 40, 50, 60, 70, 80, 85, 90, 93, 95, 97)
  val quantCount = quantiles.size
}

class RobotTestCase(val num: Int, val attemptsToDo: Int, val iterationsToDo: Int, val reportEach: Int, 
                    settings: RobotSessionSettings, listener: RobotTestDumpReport => Unit,
                    path: String, suit: String, set: String) {
  import RobotTestCase._
  def execute(): Long = {
    println("    --- Test case <" + suit + "." + set + "-" + num + "> started:")
    val globalStartTime = (new java.util.Date).getTime
    
    def dummyProcessor(e: framework.Actor.Event) {}
    
    for (attempt <- 1 to attemptsToDo) {
      print("      --= Attempt #" + attempt + " started")
			val startTime = (new java.util.Date).getTime
			val session = new RobotSession(settings.clone, dummyProcessor _)
			val quants = Array.make(quantCount, NaN)
			
			var i = 1
			var cnt = 1
			while (i <= iterationsToDo) {
			  session.actor.doStep()
			  
			  if ((i & 255) == 0 && i >= 50000) { // every 256th iteration check quantiles
					val q = 100*session.reachedCount/(session.reachedCount + session.crashedCount + session.timedOutCount)
					for (j <- 0 until quantCount) {
					  if (q >= quantiles(j) && i < quants(j))
					  	quants(j) = i
					}
			  }
			  
			  if (cnt == reportEach) {
					cnt = 0
					// report out
					listener(RobotTestDumpReport(num, attempt, i, quants.toArray, 
												 session.reachedCount, session.crashedCount, session.timedOutCount))
					
					val fileName: String = String.format("%s.%s-%d.att%02d.i%06d", suit, set, 
                                  num.asInstanceOf[Object], attempt.asInstanceOf[Object], i.asInstanceOf[Object])
					val file = new File(new File(path), fileName + ".xml")
					RobotXML.save(file.getPath, session.toXML)
					
					val all = 0.0 + session.reachedCount + session.crashedCount + session.timedOutCount
					val summaryFile = new File(new File(path), fileName + ".summary.xml")
					val summaryXML: xml.Elem = 
						<testCaseSummary>
							<suit>{suit}</suit>
							<set>{set}</set>
							<caseNumber>{num}</caseNumber>
							<reachedCount>{session.reachedCount}</reachedCount>
							<crashedCount>{session.crashedCount}</crashedCount>
							<timedOutCount>{session.timedOutCount}</timedOutCount>
							<reachedPercentage>{String.format("%.2f", 
                (100.0 * session.reachedCount / all).asInstanceOf[Object])}</reachedPercentage>
							<crashedPercentage>{String.format("%.2f", 
                (100.0 * session.crashedCount / all).asInstanceOf[Object])}</crashedPercentage>
							<timedOutPercentage>{String.format("%.2f", 
                (100.0 * session.timedOutCount / all).asInstanceOf[Object])}</timedOutPercentage>
							<quants>{quants map (x => <int>{x}</int>)}</quants>
							<secsPassed>{((new java.util.Date).getTime - startTime)/1000.0}</secsPassed>
						</testCaseSummary>
					RobotXML.save(summaryFile.getPath, summaryXML)
			  }
			  cnt += 1
			  i += 1
			}
      println(" and completed in " + ((new java.util.Date).getTime - startTime)/1000.0 + " secs.")
		}
    val time = (new java.util.Date).getTime - globalStartTime
    println("    --- Test case <" + suit + "." + set + "-" + num + " completed in " + time/1000.0 + " secs.")
    time 
  }
}
