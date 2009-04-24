package edu.torax.reinforcement.gutils
import Math._

case class Vector(x: Double, y: Double) {
  // turn angle degrees counter-clockwise
  def rotate(angle: Double): Vector = rotateRadians(Math.toRadians(angle))
  // turn theta radians counter-clockwise
  def rotateRadians(theta: Double) = Vector(x*cos(theta) - y*sin(theta), x*sin(theta) + y*cos(theta))
  
  def +(v: Vector) = Vector(x + v.x, y + v.y)
  def -(v: Vector) = Vector(x - v.x, y - v.y)		
  def *(k: Double) = Vector(k*x, k*y)
  def /(k: Double) = Vector(x/k, y/k)
  def *(v: Vector) = x*v.x + y*v.y				// dot product
  def ^(v: Vector) = x*v.y - y*v.x				// cross product
  
  def length = sqrt(x*x + y*y)
  
  def normalize = {
    val len = length
    Vector(x / len, y / len)
  }
  
  override def toString = "(" + x + "; " + y + ")"
  
  override def equals(other: Any) = other match {
  	case that: Vector => Math.abs(this.x - that.x) < Vector.eps && Math.abs(this.y - that.y) < Vector.eps
  	case _ => false
  }
}  

object Vector {
  val eps = 1e-6
  implicit def toTuple(v: Vector) = (v.x, v.y)
  implicit def toVector(v: (Double, Double)) = Vector(v._1, v._2)
  
  def angleBetween(v1: Vector, v2: Vector): Double = {
  	def correct(a: Double) = if (a < 0) a + 2.0*Math.Pi else if (a >= 2.0*Math.Pi) a-2.0*Math.Pi else a
  	correct(Math.atan2(v1.y, v1.x) - Math.atan2(v2.y, v2.x)) 
  }
}