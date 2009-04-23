package edu.torax.reinforcement.gutils

object Area {
  def of(polygon: List[Vector]): Double = {
    var last = polygon.tail.head
    var area = 0.0
    val point = polygon.head 
    for (p <- polygon.drop(2)) {
      area += (p - point) ^ (last - point)
      last = p
    }
    Math.abs(0.5*area)
  }
}
