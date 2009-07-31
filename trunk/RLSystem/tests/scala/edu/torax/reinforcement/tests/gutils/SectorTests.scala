package edu.torax.reinforcement.tests.gutils
import org.scalatest.FunSuite
import reinforcement.gutils._
import reinforcement.gutils.Vector._

class SectorTests extends FunSuite {
	test ("Sector creation and direction normalization") {
		val s1 = Sector((0.0,0.0), (1.0,1.0), 2.0, 90)

		expect (Vector(1.0/Math.sqrt(2.0), 1.0/Math.sqrt(2.0))) { s1.dirNorm }
	}

	test ("Sector overlapping with point") {
		val s1 = Sector((0.0,0.0), (1.0,1.0), 2.0, 90)

		expect ((true, 1.0)) { s1 overlapsWithPoint(Vector(1.0, 0.0)) }
		expect ((true, 1.0)) { s1 overlapsWithPoint(Vector(0.0, 1.0)) }
		expect ((true, 2.0)) { s1 overlapsWithPoint(Vector(2.0, 0.0)) }
		expect ((true, 2.0)) { s1 overlapsWithPoint(Vector(0.0, 2.0)) }
		expect ((true, Math.sqrt(2.0))) { s1 overlapsWithPoint(Vector(1.0, 1.0)) }

		expect (true) { (s1 overlapsWithPoint(Vector(1.00001, 1.0)))._1 }
		expect (true) { (s1 overlapsWithPoint(Vector(1.00001, 1.00001)))._1 }
		expect (true) { (s1 overlapsWithPoint(Vector(1.0, 1.00001)))._1 }

		val sqrt2 = Math.sqrt(2.0) 
		expect (true) { (s1 overlapsWithPoint(Vector(sqrt2, sqrt2)))._1 }
  
		expect (false) { (s1 overlapsWithPoint(Vector(sqrt2+0.0001, sqrt2)))._1 }
		expect (false) { (s1 overlapsWithPoint(Vector(sqrt2+0.0001, sqrt2+0.0001)))._1 }
		expect (true) { (s1 overlapsWithPoint(Vector(sqrt2-0.0001, sqrt2+0.0001)))._1 }
		expect (false) { (s1 overlapsWithPoint(Vector(sqrt2-0.0001,sqrt2+0.0002)))._1 }
		expect (false) { (s1 overlapsWithPoint(Vector(sqrt2, sqrt2+0.0001)))._1 }

		expect (false) { (s1 overlapsWithPoint(Vector(-1.0, 1.0)))._1 }
		expect (false) { (s1 overlapsWithPoint(Vector(1.0, -1.0)))._1 }
		expect (false) { (s1 overlapsWithPoint(Vector(-1.0, -1.0)))._1 }

		expect (true) { (s1 overlapsWithPoint(Vector(0.3, 0.2)))._1 }
	}

	test ("Sector overlapping with segment") {
		val s1 = Sector((0.0,0.0), (1.0,1.0), 2.0, 90)

		expect (false) { (s1 overlapsWithSegment Segment(Vector(1.0,-0.5), Vector(1.5,-0.5)))._1}
		expect ((true, 1.0)) { s1 overlapsWithSegment Segment(1.0, 0.0, 2.0, 2.0) }
		expect ((true, 1.0)) { s1 overlapsWithSegment Segment(10.0, 10.0, 0.0, 1.0) }
		expect ((true, Math.sqrt(2.0))) { s1 overlapsWithSegment Segment(1.0, 1.0, 2.0, 2.0) }
		expect ((true, Math.sqrt(2.0))) { s1 overlapsWithSegment Segment(2.0, 2.0, 1.0, 1.0) }

//		expect ((true, Math.sqrt(0.5))) { s1 overlapsWithSegment Segment(-3.0, 4.0, 4.0, -3.0) }
		val r1 = s1 overlapsWithSegment Segment(-3.0, 4.0, 4.0, -3.0)
		assert (r1._1 === true)
		assert (Math.abs(r1._2 - Math.sqrt(0.5)) < eps)
  
		expect ((true, 1.0)) { s1 overlapsWithSegment Segment(1.0, 40.0, 1.0, -30.0) }
		expect ((true, 0.5)) { s1 overlapsWithSegment Segment(-40.0, 0.5, 1.0, 0.5) }

		expect ((true, 1.0)) { s1 overlapsWithSegment Segment(0.0, 1.0, -10.0, -30.0) }
		expect ((true, 1.0)) { s1 overlapsWithSegment Segment(1.0, 0.0, 10.0, 30.0) }

		expect ((true, Math.sqrt(2.0))) { s1 overlapsWithSegment Segment(2.0, 0.0, 0.0, 2.0) }
		expect (false) { (s1 overlapsWithSegment Segment(4.0, 0.0, 0.0, 4.0))._1 }
  
		expect ((true, 0.0)) { s1 overlapsWithSegment Segment(-0.1, 0.1, 0.1, -0.1) }
		expect (false) { (s1 overlapsWithSegment Segment(-0.1, 0.09, 0.1, -0.11))._1 }

		expect ((true, 1.000000000000001)) { s1 overlapsWithSegment Segment(0.0, -0.1, 2.0, 0.1) }
	}
}
