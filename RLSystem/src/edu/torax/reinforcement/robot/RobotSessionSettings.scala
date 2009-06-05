package edu.torax.reinforcement.robot
import framework._

object RobotSessionSettings {
  object Defaults {
	  // RL Settings
	  val gamma: Double = 0.99
	
	  // Environment Settings
	  val envWidth: Double = 40.0
	  val envHeight: Double = 40.0
	  val envTurnAngle: Double = 15.0
	  val envTimeOut: Int = 200
	  val envMoveDistance: Double = 0.9
	  val envVisionAngle: Double = 75.0
	  val modelWidth: Double = 1.0
	  val modelHeight: Double = 1.0
	
		// Neural Network Settings
		val stepSizeFunction: GeneralFunction = LinearDecreasingFunction(0.01, 0.3, 1000000)
		val nnStructure = Array(13,8)
	  val hiddenActFunc = NeuralNetwork.ActFunc.logistic
	  val outputActFunc = NeuralNetwork.ActFunc.identity
	  val nnInitializer: NeuralNetwork.Initializer = NeuralNetwork.ConstantInitializer(0.0)
	
	  // Actor Settings
	  val epsFunction: GeneralFunction = LinearDecreasingFunction(0.00, 0.5, 300000)
	 
	  // Statistics Settings
	  val lastMax: Int = 5000			// how much last episodes to use for statistics
	
	  // Robot Environment Settings
	  val maxDistanceToGoal: Double = 1.0
	  val maxDistanceToObs: Double = 0.01
	  val obsMinNumber: Int = 4
	  val obsMaxNumber: Int = 10
	  val obsMinRadius: Double = 1.5
	  val obsMaxRadius: Double = 4.0
	  val obsGap: Double = 1.3
  }
}

class RobotSessionSettings {
  import RobotSessionSettings.Defaults
  // RL Settings
  var gamma = Defaults.gamma

  // Environment Settings
  var envWidth = Defaults.envWidth
  var envHeight = Defaults.envHeight
  var envTurnAngle = Defaults.envTurnAngle
  var envTimeOut = Defaults.envTimeOut
  var envMoveDistance = Defaults.envMoveDistance
  var envVisionAngle = Defaults.envVisionAngle
  var modelWidth = Defaults.modelWidth
  var modelHeight = Defaults.modelHeight

	// Neural Network Settings
	var stepSizeFunction = Defaults.stepSizeFunction
	var nnStructure = Defaults.nnStructure
  var hiddenActFunc = Defaults.hiddenActFunc
  var outputActFunc = Defaults.outputActFunc
  var nnInitializer = Defaults.nnInitializer

  // Actor Settings
  var epsFunction = Defaults.epsFunction

  // Statistics Settings
  var lastMax = Defaults.lastMax			// how much last episodes to use for statistics
  
  // Robot Environment Settings
  var maxDistanceToGoal = Defaults.maxDistanceToGoal
  var maxDistanceToObs = Defaults.maxDistanceToObs
  var obsMinNumber = Defaults.obsMinNumber
  var obsMaxNumber = Defaults.obsMaxNumber
  var obsMinRadius = Defaults.obsMinRadius
  var obsMaxRadius = Defaults.obsMaxRadius
  var obsGap = Defaults.obsGap

  override def clone: RobotSessionSettings = {
    val r = new RobotSessionSettings

    r.gamma = this.gamma
	  r.envWidth = this.envWidth
	  r.envHeight = this.envHeight
	  r.envTurnAngle = this.envTurnAngle
	  r.envTimeOut = this.envTimeOut
	  r.envMoveDistance = this.envMoveDistance
	  r.envVisionAngle = this.envVisionAngle
	  r.modelWidth = this.modelWidth
	  r.modelHeight = this.modelHeight
		r.stepSizeFunction = this.stepSizeFunction
		r.nnStructure = this.nnStructure
	  r.hiddenActFunc = this.hiddenActFunc
	  r.outputActFunc = this.outputActFunc
	  r.nnInitializer = this.nnInitializer
	  r.epsFunction = this.epsFunction
	  r.lastMax = this.lastMax			// how much last episodes to use for statistics
	  r.maxDistanceToGoal = this.maxDistanceToGoal
	  r.maxDistanceToObs = this.maxDistanceToObs
	  r.obsMinNumber = this.obsMinNumber
	  r.obsMaxNumber = this.obsMaxNumber
	  r.obsMinRadius = this.obsMinRadius
	  r.obsMaxRadius = this.obsMaxRadius
	  r.obsGap = this.obsGap
	  r
  }
  
  def toXML: xml.Elem = 
    <RobotSessionSettings>
		  <gamma>{gamma}</gamma>

		  <envWidth>{envWidth}</envWidth>
		  <envHeight>{envHeight}</envHeight>
		  <envTurnAngle>{envTurnAngle}</envTurnAngle>
		  <envTimeOut>{envTimeOut}</envTimeOut>
		  <envMoveDistance>{envMoveDistance}</envMoveDistance>
		  <envVisionAngle>{envVisionAngle}</envVisionAngle>
		  <modelWidth>{modelWidth}</modelWidth>
		  <modelHeight>{modelHeight}</modelHeight>

			<stepSizeFunction>{stepSizeFunction.toXML}</stepSizeFunction>
			<nnStructure>{nnStructure map (x => <int>{x}</int>)}</nnStructure>
		  <hiddenActFunc>{hiddenActFunc.toXML}</hiddenActFunc>
		  <outputActFunc>{outputActFunc.toXML}</outputActFunc>
		  <nnInitializer>{nnInitializer.toXML}</nnInitializer>

		  <epsFunction>{epsFunction.toXML}</epsFunction>

		  <lastMax>{lastMax}</lastMax>

	  	<maxDistanceToGoal>{maxDistanceToGoal}</maxDistanceToGoal>
	  	<maxDistanceToObs>{maxDistanceToObs}</maxDistanceToObs>
      <obsMinNumber>{obsMinNumber}</obsMinNumber>
      <obsMaxNumber>{obsMaxNumber}</obsMaxNumber>
      <obsMinRadius>{obsMinRadius}</obsMinRadius>
      <obsMaxRadius>{obsMaxRadius}</obsMaxRadius>
      <obsGap>{obsGap}</obsGap>
  	</RobotSessionSettings>
    
    def this(node: xml.NodeSeq) = {
      this()
      (node \ "_") foreach (x => x.label match {
        case "gamma" => gamma = x.text.toDouble
        case "envWidth" => envWidth = x.text.toDouble
        case "envHeight" => envHeight = x.text.toDouble
        case "envTurnAngle" => envTurnAngle = x.text.toDouble
        case "envTimeOut" => envTimeOut = x.text.toInt
        case "envVisionAngle" => envVisionAngle = x.text.toDouble
        case "envMoveDistance" => envMoveDistance = x.text.toDouble
        case "modelWidth" => modelWidth = x.text.toDouble
        case "modelHeight" => modelHeight = x.text.toDouble
        case "stepSizeFunction" => stepSizeFunction = GeneralFunction.fromXML(x \ "generalFunction")
        case "nnStructure" => nnStructure = ((x \ "int") map (_.text.toInt)).toArray
        case "hiddenActFunc" => hiddenActFunc = NeuralNetwork.ActFunc.fromXML(x \ "actFunc")
        case "outputActFunc" => outputActFunc = NeuralNetwork.ActFunc.fromXML(x \ "actFunc")
        case "nnInitializer" => nnInitializer = NeuralNetwork.Initializer.fromXML(x \ "initializer")
        case "epsFunction" => epsFunction = GeneralFunction.fromXML(x \ "generalFunction")
        case "lastMax" => lastMax = x.text.toInt
        case "maxDistanceToGoal" => maxDistanceToGoal = x.text.toDouble
        case "maxDistanceToObs" => maxDistanceToObs = x.text.toDouble
        case "obsMinNumber" => obsMinNumber = x.text.toInt
        case "obsMaxNumber" => obsMaxNumber = x.text.toInt
        case "obsMinRadius" => obsMinRadius = x.text.toDouble
        case "obsMaxRadius" => obsMaxRadius = x.text.toDouble
        case "obsGap" => obsGap = x.text.toDouble 
        case unknown => // ignore unknown parameters
      })
    }
}
