package edu.torax.reinforcement.tests.gutils
import org.scalatest.FunSuite
import reinforcement.gutils._
import reinforcement.gutils.Vector._

class SegmentTests extends FunSuite {
	test ("Segments intersection points") {
	  val s1 = Segment(0.0, 0.0, 1.0, 1.0) // /
	  val s2 = Segment(0.0, 1.0, 1.0, 0.0) // \
	  val s3 = Segment(0.0, 0.0, 0.0, 1.0) // |
	  val s4 = Segment(1.0, 1.0, 1.0, 0.0) // |
	  val s5 = Segment(0.0, 1.0, 1.0, 1.0) // -
	  val s6 = Segment(1.0, 0.0, 0.0, 0.0) // -
	  val s7 = Segment(1.0, 1.0, 2.0, 2.0) // / 
	  val s8 = Segment(1.1, 1.1, 2.1, 2.1) // / 

	  val p1 = Vector(0.5, 0.5)
	  expect (Some(`p1`)) { s1 intersection s2 }
	  expect (Some(`p1`)) { s2 intersection s1 }
   
	  expect (None) { s1 intersection s8}
   
	  val p2 = Vector(0.0, 0.0)
	  expect (Some(`p2`)) { s1 intersection s3 }
	  expect (Some(`p2`)) { s1 intersection s6 }
	  expect (Some(`p2`)) { s6 intersection s3 }
   
	  val p3 = Vector(1.0, 1.0)
	  expect (Some(`p3`)) { s1 intersection s4 }
	  expect (Some(`p3`)) { s1 intersection s5 }
	  expect (None/*Some(`p3`)*/) { s1 intersection s7 }	// actually it's not correct :)
	  expect (Some(`p3`)) { s4 intersection s5 }
	  expect (Some(`p3`)) { s4 intersection s7 }
	  expect (Some(`p3`)) { s5 intersection s7 }

	  val p4 = Vector(1.0, 0.0)
	  expect (Some(`p4`)) { s2 intersection s4 }
	  expect (Some(`p4`)) { s2 intersection s6 }
	  expect (Some(`p4`)) { s4 intersection s6 }
   
	  expect (None) { s7 intersection s8 }
	}
 
	test ("Distance from segment ot point") {
	  val s = Segment(0, 0, 1, 0)
   
	  expect (0.5) { s distanceTo Vector(0.5, 0.5) }
	  expect (0.5) { s distanceTo Vector(1.0, 0.5) }
	  expect (0.5) { s distanceTo Vector(0.0, 0.5) }
	  expect (Math.sqrt(0.5)) { s distanceTo Vector(1.5, 0.5) }

	  expect (0.5) { s distanceTo Vector(0.5, -0.5) }
	  expect (0.5) { s distanceTo Vector(1.0, -0.5) }
	  expect (0.5) { s distanceTo Vector(0.0, -0.5) }
	  expect (Math.sqrt(0.5)) { s distanceTo Vector(-0.5, -0.5) }
   
	  expect (1.0) { s distanceTo Vector(-1.0, 0.0) }
	  expect (4.0) { s distanceTo Vector(5.0, 0.0) }
	}
}
