package edu.torax.reinforcement.gutils
import Math._
import Vector._

case class Sector(origin: Vector, direction: Vector, distance: Double, angle: Double) {
  val dirNorm = direction.normalize
  // tells wheter the given point is within the area of the sector and returns distance to the point
  def overlapsWithPoint(p: Vector): (Boolean, Double) = {
    val v = p - origin
    val vLeft = dirNorm rotate angle/2.0
    val vRight = dirNorm rotate -angle/2.0
    ((v.length < distance) && ((v ^ vLeft)*(v ^ vRight) < eps) && (v * direction > -eps), v.length)
  }
  
  // tells wheter the given segment overlaps with current sector and returns smallest distance to the segment
  def overlapsWithSegment(s: Segment): (Boolean, Double) = {
    var res = (false, Double.PositiveInfinity)
    val r1 = this overlapsWithPoint s.p1
    val r2 = this overlapsWithPoint s.p2
    res = if (r1._1) (true, min(res._2, r1._2)) else res
    res = if (r2._1) (true, min(res._2, r2._2)) else res
    (s intersection Segment(origin, origin + (dirNorm rotate angle/2.0)*distance)) match {
      case Some(p) => res = (true, min(res._2, (p-origin).length))
      case None =>
    }
    (s intersection Segment(origin, origin + (dirNorm rotate -angle/2.0)*distance)) match {
      case Some(p) => res = (true, min(res._2, (p-origin).length))
      case None =>
    }
    res
  }
  
  // tells wheter the given polygon represented as List of succesive points 
  // overlaps with current sector and returns smallest distance to the polygon
  def overlapsWithPolygon(points: List[Vector]): (Boolean, Double) = {
    var res = (false, Double.PositiveInfinity)
    var last = points.last
    for (p <- points) {
      val tmp = this overlapsWithSegment Segment(last, p)
      res = if (tmp._1) (true, min(res._2, tmp._2)) else res
      last = p
    }
    res
  }
}
