package edu.torax.reinforcement.robot

import framework._
import gutils.Vector

class RobotSession (val settings: RobotSessionSettings, messageProcessor: (framework.Actor.Event) => Unit) {
	private var env: RobotEnvironment = null
  
  var inScreenshotMode = false
  private var scrPositions = Array.make(settings.envTimeOut + 5, Vector(Double.NaN, Double.NaN))
  private var scrMoves = 0
  
  def screenPositions = scrPositions
  def screenMoves = scrMoves
  
  private def addModelPosition(pos: Vector) {
	  scrPositions(scrMoves) = pos
	  scrMoves += 1
  }
  
  private var vfunc: ValueFunction[RobotAction, RobotState] = new UsualNNValueFunction[RobotAction,RobotState](
    RobotEnvironment.actionsCount, 
    RobotState.dimensionality, 
    settings.stepSizeFunction,
  	settings.nnInitializer,
  	settings.nnStructure,
  	settings.hiddenActFunc,
  	settings.outputActFunc)
  // ------ Actor creation --------
  recreateEnvironment()
  private var actr: Actor[RobotAction, RobotState] = createActor
  
  def createActor = {
    //recreateEnvironment()
//    val a = new SarsaActor(vfunc, settings.gamma, (x => ())) 
//  														with EpsGreedyPolicy[RobotAction,RobotState] { val eps = settings.epsFunction }
    val a = new SarsaActor(vfunc, settings.gamma, (x => ())) 
  														with EpsGreedyPolicy[RobotAction,RobotState] { val eps = settings.epsFunction }
    a.beginEpisode(env, true)
    a.listener = processMessage
    a
  }
  // ------------------------------
  
  def recreateEnvironment() = environment = new RobotEnvironment(settings)
  def environment = env
  def environment_=(value: RobotEnvironment) {
    env = value
    scrMoves = 0
    addModelPosition(env.model.position)

    if (actr != null) {
    	actr.beginEpisode(env, true)
    }
  }
  
  def valueFunction = vfunc
  def actor = actr

  private var episodesDone = 0
  private var stepsDone = 0
  private var reachedCnt = 0
  private var crashedCnt = 0
  private var timedoutCnt = 0
  def timedOutCount = timedoutCnt
  def crashedCount = crashedCnt
  def reachedCount = reachedCnt
  def stepsCount = stepsDone
  def episodesCount = episodesDone
  
  def formatRCT = " (R: " + reachedCnt + "; C: " + crashedCnt + "; T: " + timedoutCnt + ")"
  def formatRCTPercentage = {
  	val sum = Math.max(1, reachedCnt + crashedCnt + timedoutCnt)
    "{" + 100*reachedCnt/sum + "-" + 100*crashedCnt/sum + "-" + 100*timedoutCnt/sum + "}"
  }
  def formatES = "[E: " + episodesDone + ", S: " + stepsDone + "]"
  
  private var lastMax = settings.lastMax
  private var lasts = Array.make(lastMax, -1)
  private var lastB = 0
  private var lastE = 0
  private var lastCnt = 0
  
  private def processMessage(event: Actor.Event): Unit = {
    event match {
	    case ev: Actor.EpisodeStarted[_,_] =>
	      //println("-Episode " + episodesCount + " Started")
       
	    case ev: Actor.EpisodeFinished[_,_] =>
	      //println("-Episode Finished")
	      episodesDone += 1
       
	      if (lastCnt == lastMax) {
		      lasts(lastB) match {
		        case 0 => timedoutCnt -= 1
		        case 1 => crashedCnt -= 1
		        case 2 => reachedCnt -= 1
		      }
		      lastB = if (lastB == lastMax-1 ) 0 else lastB + 1
		      lastCnt -= 1
		    }
		    if (env.timedOut) {
		      timedoutCnt += 1
		      lasts(lastE) = 0
		    } else if (env.isModelCrashed) {
		      crashedCnt += 1
		      lasts(lastE) = 1
		    } else if (env.isGoalReached) {
		      reachedCnt += 1
		      lasts(lastE) = 2
		    }
		    lastE = if (lastE == lastMax-1 ) 0 else lastE + 1
		    lastCnt += 1
		    
		    recreateEnvironment()
	      
	    case ev: Actor.StepFinished[_,_] =>
	      //println("-Step Finished")
	      stepsDone += 1
	      addModelPosition(env.model.position)
    }
    messageProcessor(event)
  } 

  def toXML = 
    <RobotSession>
  		<settings>{settings.toXML}</settings>
    	<environment>{env.toXML}</environment>
    	<valueFunction>{vfunc.toXML}</valueFunction>
      <episodesDone>{episodesDone}</episodesDone>
      <stepsDone>{stepsDone}</stepsDone>
      <reachedCnt>{reachedCnt}</reachedCnt>
      <crashedCnt>{crashedCnt}</crashedCnt>
      <timedoutCnt>{timedoutCnt}</timedoutCnt>
      <lastMax>{lastMax}</lastMax>
      <lastB>{lastB}</lastB>
      <lastE>{lastE}</lastE>
      <lastCnt>{lastCnt}</lastCnt>
      <lasts>{lasts map (x => <int>{x}</int>)}</lasts>
      <inScreenshotMode>{inScreenshotMode}</inScreenshotMode>
      <scrMoves>{scrMoves}</scrMoves>
      <scrPositions>{scrPositions map (_.toXML)}</scrPositions>
  	</RobotSession>
     
  def this(node: xml.NodeSeq, messageProcessor: (framework.Actor.Event) => Unit) = {
    this(new RobotSessionSettings(node \ "settings" \ "RobotSessionSettings"), messageProcessor)
    env = new RobotEnvironment(node \ "environment" \ "RobotEnvironment", settings)
    vfunc = new UsualNNValueFunction(node \ "valueFunction" \ "UsualNNValueFunction")
     
    episodesDone = (node \ "episodesDone").text.toInt
    stepsDone = (node \ "stepsDone").text.toInt
    reachedCnt = (node \ "reachedCnt").text.toInt
    crashedCnt = (node \ "crashedCnt").text.toInt
    timedoutCnt = (node \ "timedoutCnt").text.toInt
    
    lastMax = (node \ "lastMax").text.toInt
    lastB = (node \ "lastB").text.toInt
    lastE = (node \ "lastE").text.toInt
    lastCnt = (node \ "lastCnt").text.toInt
    lasts = ((node \ "lasts" \ "int") map (_.text.toInt)).toArray
    
    inScreenshotMode = (node \ "inScreenshotMode").text.toBoolean
    scrMoves = (node \ "scrMoves").text.toInt
    scrPositions = ((node \ "scrPositions" \ "vector") map (Vector.fromXML(_))).toArray
    
    actr = createActor
  }
}