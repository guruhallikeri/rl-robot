package edu.torax.reinforcement.robot
import edu.torax.scheduler._
import java.io.File

case class RobotTestSuiteTask(testSet: RobotTestSet, testSetId: Int, 
                            fromTest: Int, toTest: Int, path: String)
case class RobotTestSuiteResult(testSet: RobotTestSet, testSetId: Int, testsDone: Int, timePassed: Long)

class RobotTestSuiteWorker extends Worker[RobotTestSuiteTask, RobotTestSuiteResult] {
  def processTask(task: RobotTestSuiteTask): RobotTestSuiteResult = {
    val (time, tests) = task.testSet.execute(task.fromTest, task.toTest, task.path)
    RobotTestSuiteResult(task.testSet, task.testSetId, tests, time)
  }
}

class RobotTestSuiteScheduler (maxWorkers: Int, listener: Listener[RobotTestSuiteResult])
 extends Scheduler[RobotTestSuiteTask, RobotTestSuiteWorker, RobotTestSuiteResult](maxWorkers, listener) {
   protected def makeWorker = new RobotTestSuiteWorker()
 }

class RobotTestSuite(elem: xml.NodeSeq, path: String) extends Listener[RobotTestSuiteResult] {
	val name = (elem \ "@name").text
	val description = (elem \ "@descr").text
	val repeatEachTest = (elem \ "@repeatEachTest").text.toInt
	val iterationsToDo = (elem \ "@iterationsToDo").text.toInt
	val dumpEvery = (elem \ "@dumpEvery").text.toInt
	val fromTest = if ((elem \ "@fromTest").length == 0) 1 else (elem \ "@fromTest").text.toInt
	val toTest = if ((elem \ "@toTest").length == 0) 1000000000 else (elem \ "@toTest").text.toInt
	
	private def loadTestSet(elem: xml.NodeSeq) = {
		val r = new RobotTestSet(elem, name, repeatEachTest, iterationsToDo, dumpEvery)
		println(name + ": Test Set <" + r.name + "> loaded with " + r.testsCount + " tests in it.")
		r
	}
	val testSets = ((elem \ "testSet") map loadTestSet).toArray
	val allTestsCount = (0 /: testSets) ((x,y) => x + y.testsCount)
 
	println(name + " -- " + testSets.size + " test sets loaded with total " + allTestsCount + " tests.")
	println("Tests in range [" + fromTest + "; " + toTest + "] will be run.")
	
	private val startTime = System.currentTimeMillis
	private var testsDone = 0
 	private var testSetsDone = 0
	private val testSetsReports: Array[xml.Elem] = Array.make(testSets.size, null)

	val workDir = new File(new File(path), name)
	workDir.mkdirs

	def execute() { 
		println("Starting executing test suit <" + name + ">, tests in range [" + fromTest +"; " + toTest + "].")

  
		val scheduler = new RobotTestSuiteScheduler(2, this)
		for (val (testSet, index) <- testSets.zipWithIndex) {
		  scheduler ! NewTask(RobotTestSuiteTask(testSet, index, fromTest-testsDone, toTest-testsDone, workDir.getPath))
	  }
	}
 
 	def taskDone(taskResult: RobotTestSuiteResult) {
 	  val testSet = taskResult.testSet
		println(name + ": Test set <" + testSet.name + "> done in " + (taskResult.timePassed/1000.0) + 
			"s. Tests done/passed: [" + taskResult.testsDone + "/" + testSet.testsCount + "].")
    testSetsDone += 1
    testsDone += taskResult.testsDone
		val testSetReport =
			<testSetDone>
				<name>{testSet.name}</name>
				<time>{taskResult.timePassed/1000.0}</time>
				<testsDone>{taskResult.testsDone}</testsDone>
				<testsPassed>{testSet.testsCount}</testsPassed>
			</testSetDone>
    testSetsReports(taskResult.testSetId) = testSetReport
    
    if (testSetsDone == testSets.size)
    {
			val time = System.currentTimeMillis - startTime
			println(" -- Test suit <" + name + "> done in " + time/1000.0 +
				" secs with total amount of tests actually performed: " + testsDone + ".")
			val testSuiteReport = 
				<testSuiteDone>
					<name>{name}</name>
					<time>{time/1000.0}</time>
					<testsDone>{testsDone}</testsDone>
					<testsPassed>{allTestsCount}</testsPassed>
					{testSetsReports}
				</testSuiteDone>
			val file = new File(workDir, name + ".summary.xml")
			RobotXML.save(file.getPath, testSuiteReport)
			exit()
    }
 	}

//	def execute(): Long = {
//	  println("Starting executing test suit <" + name + ">, tests in range [" + fromTest +"; " + toTest + "].")
//		val startTime = System.currentTimeMillis
//
//		import java.io.File
//		val workDir = new File(new File(path), name)
//		workDir.mkdirs
//
//		var testsPassed = 0
//		var testsDone = 0
//		var data: Array[xml.Elem] = Array.make(testSets.size, null)
//  
//		var i = 0
//		for (val testSet <- testSets) {
//			val res: (Long, Int) = testSet.execute(fromTest-testsPassed, toTest-testsPassed, workDir.getPath)
//			testsPassed += testSet.testsCount
//			testsDone += res._2
//			println(name + ": Test set <" + testSet.name + "> done in " + (res._1/1000.0) + 
//				"s. Tests done/passed: [" + res._2 + "/" + testSet.testsCount +
//				"]. Overall progress: [" + testsPassed + "/" + allTestsCount + "]")
//   
//			val tmp =
//				<testSetDone>
//					<name>{testSet.name}</name>
//					<time>{res._1/1000.0}</time>
//					<testsDone>{res._2}</testsDone>
//					<testsPassed>{testSet.testsCount}</testsPassed>
//				</testSetDone>
//     
//			data(i) = tmp
//			i += 1
//		}
//  
//		val time = (new java.util.Date).getTime - startTime
//		println(" -- Test suit <" + name + "> done in " + time/1000.0 +
//			" secs with total amount of tests actually performed: " + testsDone + ".")
//			
//		val tmp = 
//			<testSuiteDone>
//				<name>{name}</name>
//				<time>{time/1000.0}</time>
//				<testsDone>{testsDone}</testsDone>
//				<testsPassed>{testsPassed}</testsPassed>
//				{data}
//			</testSuiteDone>
//		
//		val file = new File(workDir, name + ".summary.xml")
//		RobotXML.save(file.getPath, tmp)
//		
//		time
//	}
}
