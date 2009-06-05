package edu.torax.reinforcement.robot
import framework._

abstract class RobotTestParam(val name: String) {
  def modify(settings: RobotSessionSettings): Unit
  def nextValue(): Boolean
  val variantsCount: Int
  def reset(): Unit
}

object RobotTestParam {
  def fromXML(elem: xml.NodeSeq): RobotTestParam = (elem \ "@type").text match {
  	case "int" => 
		  if ((elem \ "@value").length != 0) 
			new RobotTestIntParam((elem \ "@name").text, (elem \ "@value").text.toInt, (elem \ "@value").text.toInt, 1)
		  else
			new RobotTestIntParam((elem \ "@name").text, (elem \ "@min").text.toInt, (elem \ "@max").text.toInt, (elem \ "@step").text.toInt)
	  
		case "double" =>
		  if ((elem \ "@value").length != 0) 
			new RobotTestDoubleParam((elem \ "@name").text, (elem \ "@value").text.toDouble, (elem \ "@value").text.toDouble, 1)
		  else
			new RobotTestDoubleParam((elem \ "@name").text, (elem \ "@min").text.toDouble, (elem \ "@max").text.toDouble, (elem \ "@step").text.toDouble)

		case "array.int" => 
		  new RobotTestArrayIntParam((elem \ "@name").text, ((elem \ "int") map (_.text.toInt)).toArray)

		case "function" => 
		  new RobotTestFuncParam((elem \ "@name").text, GeneralFunction.fromXML(elem \ "generalFunction"))
		
		case "initializer" => 
		  new RobotTestInitializerParam((elem \ "@name").text, NeuralNetwork.Initializer.fromXML(elem \ "initializer"))
	
		case "actFunc" => 
		  new RobotTestActFuncParam((elem \ "@name").text, NeuralNetwork.ActFunc.fromXML(elem \ "actFunc"))
	
		case unknown => throw new Exception("Unknown type of test parameter: " + unknown)
  }
}

class RobotTestIntParam(name: String, val min: Int, val max: Int, val step: Int) extends RobotTestParam(name) {
  private var cur = min
  
  def modify(settings: RobotSessionSettings){
    if (cur > max) throw new Exception("Test param out of range: " + toString)
    name match {
      case "envTimeOut" => settings.envTimeOut = cur
      case "lastMax" => settings.lastMax = cur
      case "obsMinNumber" => settings.obsMinNumber = cur
      case "obsMaxNumber" => settings.obsMaxNumber = cur
      case unallowed => throw new Exception("Unallowed integer parameter: " + unallowed)
    }
  }
  
  def nextValue() = {
    cur += step
    (cur <= max)
  }

  def reset() { cur = min }
  val variantsCount = 1 + (max-min)/step

  override def toString = name + " = " + "(" + min + ", " + max + "; " + step + ": " + cur + ")"
}

class RobotTestDoubleParam(name: String, val min: Double, val max: Double, val step: Double) extends RobotTestParam(name) {
  private var cur = min
  
  def modify(settings: RobotSessionSettings){
    if (cur > max + 1e-7) throw new Exception("Test param out of range: " + toString)
    name match {
		  case "gamma" => settings.gamma = cur
		  case "envWidth" => settings.envWidth = cur
		  case "envHeight" => settings.envHeight = cur
		  case "envTurnAngle" => settings.envTurnAngle = cur
		  case "envMoveDistance" => settings.envMoveDistance = cur
		  case "envVisionAngle" => settings.envVisionAngle = cur
		  case "modelWidth" => settings.modelWidth = cur
		  case "modelHeight" => settings.modelHeight = cur
		  case "maxDistanceToGoal" => settings.maxDistanceToGoal = cur
		  case "maxDistanceToObs" => settings.maxDistanceToObs = cur
		  case "obsMinRadius" => settings.obsMinRadius = cur
		  case "obsMaxRadius" => settings.obsMaxRadius = cur
		  case "obsGap" => settings.obsGap = cur
      case unallowed => throw new Exception("Unallowed floating-point parameter: " + unallowed)
    }
  }
  
  def nextValue() = {
    cur += step
    (cur < max + 1e-7)
  }

  def reset() { cur = min }
  val variantsCount = 1 + ((max-min)/step + 1e-7).toInt

  override def toString = name + " = " + "(" + min + ", " + max + "; " + step + ": " + cur + ")"
}

class RobotTestFuncParam(name: String, func: GeneralFunction) extends RobotTestParam(name) {
  def nextValue() = false
  
  def modify(settings: RobotSessionSettings) {
    name match {
      case "stepSizeFunction" => settings.stepSizeFunction = func
      case "epsFunction" => settings.epsFunction = func
      case unallowed => throw new Exception("Unallowed general function parameter: " + unallowed)
    }
  }

  def reset() {}
  val variantsCount = 1
  
  override def toString = name + " = " + func.toString
}

class RobotTestActFuncParam(name: String, func: NeuralNetwork.ActFunc) extends RobotTestParam(name) {
  def nextValue() = false
  
  def modify(settings: RobotSessionSettings) {
    name match {
      case "hiddenActFunc" => settings.hiddenActFunc = func
      case "outputActFunc" => settings.outputActFunc = func
      case unallowed => throw new Exception("Unallowed activation function parameter: " + unallowed)
    }
  }

  def reset() {}
  val variantsCount = 1

  override def toString = name + " = " + func.toString
}

class RobotTestInitializerParam(name: String, init: NeuralNetwork.Initializer) extends RobotTestParam(name) {
  def nextValue() = false
  
  def modify(settings: RobotSessionSettings) {
    name match {
      case "nnInitializer" => settings.nnInitializer = init
      case unallowed => throw new Exception("Unallowed initializer parameter: " + unallowed)
    }
  }

  def reset() {}
  val variantsCount = 1
  
  override def toString = name + " = " + init.toString
}

class RobotTestArrayIntParam(name: String, array: Array[Int]) extends RobotTestParam(name) {
  def nextValue() = false
  
  def modify(settings: RobotSessionSettings) {
    name match {
      case "nnStructure" => settings.nnStructure = array.toArray
      case unallowed => throw new Exception("Unallowed Array[Int] parameter: " + unallowed)
    }
  }

  def reset() {}
  val variantsCount = 1

  override def toString = name + " = " + array.toString
}