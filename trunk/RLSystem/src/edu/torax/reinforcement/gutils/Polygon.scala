package edu.torax.reinforcement.gutils

object Polygon {
	def distanceBetween(polygon1: List[Vector], polygon2: List[Vector]): Double = {
	  if (containsPoint(polygon1, polygon2.head) || containsPoint(polygon2, polygon1.head)) return 0.0
   
		var last1 = polygon1.last
		var last2 = polygon2.last
		var res = Double.PositiveInfinity
		for (p1 <- polygon1) {
			for (p2 <- polygon2) {
				res = res min (Segment(last1, p1) distanceTo Segment(last2, p2))
				last2 = p2
			}
			last1 = p1
		}
		res
	}
 
	def distanceBetween(polygon: List[Vector], point: Vector): Double = {
		if (containsPoint(polygon, point)) return 0.0

		var last = polygon.last
		var res = Double.PositiveInfinity
		for (p <- polygon) {
			res = res min (Segment(last, p) distanceTo point)
			last = p
		}
		res
	}
 
	def distanceBetween(polygon: List[Vector], segment: Segment): Double = {
	  if (containsPoint(polygon, segment.p1)) return 0.0
   
	  var last = polygon.last
	  var res = Double.PositiveInfinity
	  for (p <- polygon) {
	    res = res min (Segment(last, p) distanceTo segment)
	    last = p
	  }
	  res
	}
 
  def area(polygon: List[Vector]): Double = {
    var last = polygon.tail.head
    var res = 0.0
    val point = polygon.head 
    for (p <- polygon.drop(2)) {
      res += (p - point) ^ (last - point)
      last = p
    }
    Math.abs(0.5*res)
  }
  
	def containsPoint(polygon: List[Vector], point: Vector): Boolean = {
		var tmp = 0.0
		var last = polygon.last
		for (p <- polygon) {
			tmp += Math.abs((last - point)^(p - point))
			last = p
		}
		Math.abs(2*area(polygon) - tmp) < Vector.eps
	}
}
