package edu.torax.reinforcement.robot
import collection.mutable._
import java.io.File

class RobotTestSet(elem: xml.NodeSeq, val suitName: String, 
                   val attemptsToDo: Int, val iterationsToDo: Int, dumpEvery: Int) {
  val name = (elem \ "@name").text

  val (params, testsCount) = processParams
  
  private def processParams: (List[List[RobotTestParam]], Int) = {
	  val tmpParams = new HashMap[String, Set[RobotTestParam]]  
	  
	  def processParam(node: xml.NodeSeq) {
	    val tmp = RobotTestParam.fromXML(node)
	    if (tmpParams contains tmp.name) {
	    	tmpParams(tmp.name) += tmp
	    } else {
	      tmpParams += tmp.name -> HashSet(tmp)
	    }
	  }
   
	  (elem \ "param") foreach processParam
	  
	  val pars = (tmpParams map (x => x._2.toList)).toList
	  val cnt = (1 /: pars) ((x,y) => x * (0 /: y){_ + _.variantsCount} )
	  (pars, cnt)
  }
  
	def execute(fromTest: Int, toTest: Int, path: String): (Long, Int) = {
	  println("  -- Starting executing <" + suitName + "." + name + "> test set.")
	  val startTime = System.currentTimeMillis
   
	  val testSetDir = new File(new File(path), "test-set." + name)
	  testSetDir.mkdir

	  def process(pars: List[List[RobotTestParam]], curTest: Int, 
                curSetts: RobotSessionSettings): (Int, Int) = pars match {
      case x :: xs =>
        var testsPassed = 0
        var testsDone = 0
        for (param <- x) {
          param.reset()
          do {
            param.modify(curSetts)
            val t = process(xs, curTest+testsPassed, curSetts)
            testsPassed += t._1
            testsDone += t._2
          } while (param.nextValue())
        }
        (testsPassed, testsDone)
      case Nil => 
        if (curTest >= fromTest && curTest <= toTest) {
          val wDir = new File(testSetDir, String.format("test-case.%03d", curTest.asInstanceOf[Object]))
          wDir.mkdir

          val stats: Array[List[RobotTestDumpReport]] = Array.make(attemptsToDo, Nil)
					def listener(report: RobotTestDumpReport) {
          	stats(report.attempt-1) = report :: stats(report.attempt-1) 
          }
          
          val test = new RobotTestCase(curTest, attemptsToDo, iterationsToDo, dumpEvery, 
                                       curSetts, listener, 
                                       wDir.getPath, suitName, name)
          val execTime = test.execute()
          println("  -- " + suitName + "." + name + ": test case #" + curTest + 
                    " completed in " + execTime/1000.0 + " secs.")
          
          var xmlElems: List[xml.Elem] = Nil
          val sz = stats(0).size
          for (i <- 0 until sz) {
            var reached = 0.0
            var crashed = 0.0
            var timedOut = 0.0
            val q = Array.make(RobotTestCase.quantCount, 0)
            val qC = Array.make(RobotTestCase.quantCount, 0)
            val itersDone = stats(0).head.iterationsPassed
            for (j <- 0 until attemptsToDo) {
              val r = stats(j).head.reached
              val c = stats(j).head.crashed
              val t = stats(j).head.timedOut
              val all = 0.0 +  r + c + t
              reached += r / all
              crashed += c / all
              timedOut += t / all
              
              for (k <- 0 until q.size) {
                if (stats(j).head.quantiles(k) != RobotTestCase.NaN) {
                  q(k) += stats(j).head.quantiles(k)
                  qC(k) += 1
                } 
              }
              stats(j) = stats(j).tail
            }
            for (k <- 0 until q.size) {
              if (qC(k) != 0) {
                q(k) /= qC(k)
              }
            }
            val x = 
              <testCaseAverages itersDone={itersDone.toString}>
              	<reachedPercentage>{100.0 * reached / attemptsToDo}</reachedPercentage>
              	<crashedPercentage>{100.0 * crashed / attemptsToDo}</crashedPercentage>
              	<timedOutPercentage>{100.0 * timedOut / attemptsToDo}</timedOutPercentage>
              	{(q zip RobotTestCase.quantiles) map (x => 
              	  <quant val={x._2.toString}>{x._1}</quant>
              	)}
              </testCaseAverages>
            xmlElems = x :: xmlElems
          }
          val statistics = 
            <testCaseSummary suit={suitName} set={name} caseNum={curTest.toString}>
            	{xmlElems}
            </testCaseSummary>
          val summaryFile = new File(wDir, suitName + "." + name + "-" + curTest + ".summary.xml") 
          RobotXML.save(summaryFile.getPath, statistics)
          (1, 1)
        } else {
          (1, 0)
        }
	  }
	  val (passed, done) = process(params, 1, new RobotSessionSettings)
  	println("  -- Test set <" + suitName + "." + name + "> completed: [" + done + "/" + passed + "].")
	  (System.currentTimeMillis - startTime, done)
	}
}
