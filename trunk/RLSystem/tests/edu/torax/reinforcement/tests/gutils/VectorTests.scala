package edu.torax.reinforcement.tests.gutils
import org.scalatest.FunSuite
import reinforcement.gutils.Vector
import reinforcement.gutils.Vector._
  
class VectorTests extends FunSuite {

  test ("wheter Vector constructs correctly") {
	  val v = Vector(7.7, 5.4)
	  expect (7.7) { v.x }
	  expect (5.4) { v.y } 
  }
  
  test ("Vector arithemetic works correctly") {
    val v1 = Vector(3, 2)
    val v2 = Vector(7, 9)
    
    expect (Vector(1.5, 1.0)) { v1/2 }
    expect (Vector(1.5, 1.0)) { v1*0.5 }
    expect (Vector(13, 13)) { v1*2 + v2 }
    expect (Vector(-4, -7)) { v1 - v2 }
  }
  
  test ("Vector dot product") {
    val v1 = Vector(3, 5)
    val v2 = Vector(2, 7)
    expect (41.0) { v1*v2 }
    
    val v3 = Vector(1, 0)
    val v4 = Vector(0, 1)
    expect (0.0) { v3*v4 }
  }
  
  test ("Vector cross product") {
    val v1 = Vector(1, 1)
    val v2 = Vector(1, 0)
    val v3 = Vector(-1, 0)
    expect (-1.0) { v1 ^ v2 }
    expect (1.0) { v2 ^ v1 }
    expect (0.0) { v2 ^ v3 }
    expect (0.0) { v3 ^ v2 }
    expect (1.0) { v1 ^ v3 }
    expect (-1.0) { v3 ^ v1 }
  }
  
  test ("Vector length calculation") {
    val v1 = Vector(2.0, 0.0)
    val v2 = Vector(1.0, 1.0)
    val v3 = Vector(-1.0, -1.0)
    val v4 = Vector(0.0, 3.0)
    expect (2.0) { v1.length }
    expect (Math.sqrt(2.0)) { v2.length }
    expect (v2.length) { v3.length }
    expect (3.0) { v4.length }
  }
  
  test ("Vector normalization") {
    val v1 = Vector(-1.0, 0.0)
    expect (v1) { v1.normalize }
    val v2 = Vector(1.0, -1.0)
    expect (Vector(1.0/Math.sqrt(2.0), -1.0/Math.sqrt(2.0))) { v2.normalize }
    val v3 = Vector(7.0, 89.0) 
    expect (1.0) { v3.normalize.length}
  }
  
  test ("Vector rotation") {
    val v1 = Vector(1.0, 0.0)
    expect (Vector(0, 1)) { v1 rotate 90 }
    expect (Vector(0, 1)) { v1 rotate -270 }
    expect (Vector(0, -1)) { v1 rotate -90 }
    expect (Vector(0, -1)) { v1 rotate 270 }
    expect (Vector(-1, 0)) { v1 rotate 180 }
    expect (Vector(-1, 0)) { v1 rotate -180 }
    
    val v2 = Vector(1.0, 1.0).normalize
    expect (Vector(1.0, 0.0)) { v2 rotate -45 }
    expect (Vector(0.0, 1.0)) { v2 rotate 45 }
  }
  
  test ("Vector and tuple implicit conversions") {
    val v1 = Vector(1, 2)
    val t1 = (1.0, 2.0)
    expect (v1) { val v2: Vector = t1; v2 }
    expect (t1) { val t2: (Double, Double) = v1; t2 }
  }
}
