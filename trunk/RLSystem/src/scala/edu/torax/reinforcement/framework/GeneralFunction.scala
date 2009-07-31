package edu.torax.reinforcement.framework

abstract class GeneralFunction(val name: String) {
  def toXML: xml.Elem
  def apply(): Double
  def makeCopy: GeneralFunction
}

object GeneralFunction {
  def fromXML(elem: xml.NodeSeq): GeneralFunction = (elem \ "@name").text match {
    case "linearDecreasing" => new LinearDecreasingFunction(elem)
    case unknown => throw new Exception("Unknown type of step-size function: " + unknown)
  }
}

case class LinearDecreasingFunction(min: Double, max: Double, timeRange: Int) extends GeneralFunction("linearDecreasing") {
  def this(elem: xml.NodeSeq) = {
    this((elem \ "@min").text.toDouble, (elem \ "@max").text.toDouble, (elem \ "@timeRange").text.toInt)
    itersDone = if ((elem \ "@itersDone").length != 0) (elem \ "@itersDone").text.toInt else 0
  }
  
  private var itersDone = 0
  
  def apply() = {
    itersDone += 1
    Math.max(min, min + (max-min)*(timeRange-itersDone)/timeRange)
  }
  
  def makeCopy = {
    val r = LinearDecreasingFunction(min, max, timeRange)
    r.itersDone = this.itersDone
    r
  }
  
  def toXML: xml.Elem = <generalFunction name={name} min={min.toString} max={max.toString} 
    												timeRange={timeRange.toString} itersDone={itersDone.toString}/>
}
