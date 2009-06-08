package edu.torax.reinforcement.robot

class RobotTestSuite(elem: xml.NodeSeq, path: String) {
	val name = (elem \ "@name").text
	val description = (elem \ "@descr").text
	val repeatEachTest = (elem \ "@repeatEachTest").text.toInt
	val iterationsToDo = (elem \ "@iterationsToDo").text.toInt
	val dumpEvery = (elem \ "@dumpEvery").text.toInt
	val fromTest = if ((elem \ "@fromTest").length == 0) 1 else (elem \ "@fromTest").text.toInt
	val toTest = if ((elem \ "@toTest").length == 0) 1000000000 else (elem \ "@toTest").text.toInt
	
	private var allTestsCount = 0
	val testSets = (elem \ "testSet") map loadTestSet
	println(name + " -- " + testSets.size + " test sets loaded with total " + allTestsCount + " tests.")
	println("Tests in range [" + fromTest + "; " + toTest + "] will be run.")
	
	private def loadTestSet(elem: xml.NodeSeq) = {
		val r = new RobotTestSet(elem, name, repeatEachTest, iterationsToDo, dumpEvery)
		println(name + ": Test Set <" + r.name + "> loaded with " + r.testsCount + " tests in it.")
		allTestsCount += r.testsCount
		r
	}
  
	def execute(): Long = {
	  println("Starting executing test suit <" + name + ">, tests in range [" + fromTest +"; " + toTest + "].")
		val startTime = (new java.util.Date).getTime

		import java.io.File
		val workDir = new File(new File(path), name)
		workDir.mkdirs

		var testsPassed = 0
		var testsDone = 0
		var data: Array[xml.Elem] = Array.make(testSets.size, null)
  
		var i = 0
		for (val testSet <- testSets) {
			val res: (Long, Int) = testSet.execute(fromTest-testsPassed, toTest-testsPassed, workDir.getPath)
			testsPassed += testSet.testsCount
			testsDone += res._2
			println(name + ": Test set <" + testSet.name + "> done in " + (res._1/1000.0) + 
				"s. Tests done/passed: [" + res._2 + "/" + testSet.testsCount +
				"]. Overall progress: [" + testsPassed + "/" + allTestsCount + "]")
   
			val tmp =
				<testSetDone>
					<name>{testSet.name}</name>
					<time>{res._1/1000.0}</time>
					<testsDone>{res._2}</testsDone>
					<testsPassed>{testSet.testsCount}</testsPassed>
				</testSetDone>
     
			data(i) = tmp
			i += 1
		}
  
		val time = (new java.util.Date).getTime - startTime
		println(" -- Test suit <" + name + "> done in " + time/1000.0 +
			" secs with total amount of tests actually performed: " + testsDone + ".")
			
		val tmp = 
			<testSuiteDone>
				<name>{name}</name>
				<time>{time/1000.0}</time>
				<testsDone>{testsDone}</testsDone>
				<testsPassed>{testsPassed}</testsPassed>
				{data}
			</testSuiteDone>
		
		val file = new File(workDir, name + ".summary.xml")
		RobotXML.save(file.getPath, tmp)
		
		time
	}
}
