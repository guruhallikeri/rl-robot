package edu.torax.reinforcement.acrobot
import framework.State

class AcrobotState(model: AcrobotModel) extends State {
	import AcrobotModel._
	def encode: List[Double] = 
		List(
			(model.theta1 - minTheta1)/(maxTheta1 - minTheta1),
			(model.dotTheta1 - minDotTheta1)/(maxDotTheta1 - minDotTheta1),
			(model.theta2 - minTheta2)/(maxTheta2 - minTheta2),
			(model.dotTheta2 - minDotTheta2)/(maxDotTheta2 - minDotTheta2)
		)
//	encoder(model.theta1 - minTheta1, 4, maxTheta1 - minTheta1) :::
//	encoder(model.dotTheta1 - minDotTheta1, 4, maxDotTheta1 - minDotTheta1) :::
//	encoder(model.theta2 - minTheta2, 4, maxTheta2 - minTheta2) :::
//	encoder(model.dotTheta2 - minDotTheta2, 4, maxDotTheta2 - minDotTheta2)
//   
//	private def encoder(x: Double, N: Int, r: Double): List[Double] = {
//		val w = 4*N/r
//		val res = for (i <- 1 to N) yield 1.0 / (1.0 + Math.exp(w*((2*i - 1)/2.0*r/N - x)))
//		res.toList
//	}
}

object AcrobotState
{
	val dimensionality = 4
}