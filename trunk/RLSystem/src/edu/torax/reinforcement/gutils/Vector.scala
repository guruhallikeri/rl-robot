package edu.torax.reinforcement.gutils
import Math._

case class Vector(x: Double, y: Double) {
  def rotate(theta: Double): Vector =				// turn theta radians left
    Vector(x*cos(theta) - y*sin(theta), x*sin(theta) + y*cos(theta))
  
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
}  

object Vector {
  val eps = 1e-6
  implicit def toTuple(v: Vector) = (v.x, v.y)
  implicit def toVector(v: (Double, Double)) = Vector(v._1, v._2)
}