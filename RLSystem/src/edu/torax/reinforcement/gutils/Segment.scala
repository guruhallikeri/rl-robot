package edu.torax.reinforcement.gutils
import Math._
import Vector._

case class Segment(p1: Vector, p2: Vector) {
  // returns intersection point of two segments or Nothing if they do not intersects
  def intersection(seg: Segment): Option[Vector] = {
    val ax = p1.x
    val ay = p1.y
    val bx = p2.x
    val by = p2.y
    val cx = seg.p1.x
    val cy = seg.p1.y
    val dx = seg.p2.x
    val dy = seg.p2.y
    val det = (bx - ax) * (cy - dy) - (by - ay) * (cx - dx)
    if (abs(det) < eps) {
      None
    } else {
      val det1 = (cx - ax) * (cy - dy) - (cy - ay) * (cx - dx)
      val det2 = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
      val t1 = det1 / det;
      val t2 = det2 / det;
      if (t1 > -eps && t1 < 1.0 + eps && t2 > -eps && t2 < 1.0 + eps) {
        Some(Vector(ax + t1 * (bx - ax), ay + t1 * (by - ay)))
      } else {
        None
      }	
    }	
  }	
  
  def distanceTo(p: Vector): Double = {
    if ((p-p1)*(p2-p1) > -eps && (p-p2)*(p1-p2) > -eps) {
      Math abs (((p-p1)^(p2-p1)) / (p2-p1).length)
    } else {
      min((p-p1).length, (p-p2).length)
    }
  }
  
  def distanceTo(s: Segment): Double = {
    var res = Double.PositiveInfinity
    res = res min (this distanceTo s.p1)
    res = res min (this distanceTo s.p2)
    res = res min (s distanceTo p1)
    res = res min (s distanceTo p2)
    res
  }
}

object Segment {
  def apply(x1: Double, y1: Double, x2: Double, y2: Double): Segment = Segment(Vector(x1,y1), Vector(x2,y2))
}
