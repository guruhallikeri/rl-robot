package edu.torax.reinforcement.acrobot

class AcrobotModel {
  private var th1 = 0.0
  private var dth1 = 0.0
  private var th2 = 0.0
  private var dth2 = 0.0
  
  def theta1 = th1
  def theta2 = th2
  def dotTheta1 = dth1
  def dotTheta2 = dth2
  
  def torque(tau: Double) {
    import AcrobotModel._
    import Math._
    def bound(v: Double, mn: Double, mx: Double) = max(mn, min(mx, v))
//    val q2 = th2
//    val dq2 = dth2
//    val q1 = th1 - Pi / 2.0
//    val dq1 = dth1
//    val force = maxForce * tau
//    val cosQ2 = cos(q2)
//    val sinQ2 = sin(q2)
//    val cosQ1Q2 = cos(q1 + q2)
//    val d11 = m1*lc1*lc1 + m2*(l1*l1 + lc2*lc2 + 2*l1*lc2*cosQ2) + I1 + I2
//    val d22 = m2*lc2*lc2 + I2
//    val d12 = m2*(lc2*lc2 + l1*lc2*cosQ2) + I2
//    val h1 = -m2*l1*lc2*sinQ2*dq2*dq2 - 2*m2*l1*lc2*sinQ2*dq2*dq1
//    val h2 = m2*l1*lc2*sinQ2*dq1*dq1
//    val phi1 = (m1*lc1 + m2*l1)*G*cos(q1) + m2*lc2*G*cosQ1Q2
//    val phi2 = m2*lc2*G*cosQ1Q2
//    
//    val q2acc = (force + d12/d11*(h1+phi1) - h2 - phi2) / (d22 - d12*d12/d11)
//    val q1acc = ((d12*q2acc)+h1+phi1) / (-d11)
//
//    val dq1new = bound(dq1 + deltaT*q1acc, minDotTheta1, maxDotTheta1)
//    val q1new = bound(q1 + deltaT*dq1new + Pi/2.0, minTheta1, maxTheta1)
//    val dq2new = bound(dq2 + deltaT*q2acc, minDotTheta2, maxDotTheta2)
//    val q2new = bound(q2 + deltaT*dq2new, minTheta2, maxTheta2)
//    
//    th1 = q1new
//    dth1 = dq1new
//    th2 = q2new
//    dth2 = dq2new
    for (i <- 1 to 4) {
	    val phi2 = m2 * lc2 * G * cos(th1 + th2 - Pi/2.0)
	    val phi1 = -m2*l1*lc2*dth2*dth2*sin(th2) - 2*m2*l1*lc2*dth1*dth2*sin(th2) + (m1*lc1 + m2*l1)*G*cos(th1-Pi/2.0) + phi2
	    val d2 = m2*(lc2*lc2 + l1*lc2*cos(th2)) + I2
	    val d1 = m1*lc1*lc1 + m2*(l1*l1 + lc2*lc2 + 2*l1*lc2*cos(th2)) + I1 + I2
	    val ddth2 = (maxForce*tau + d2*phi1/d1 - m2*l1*lc2*dth1*dth1*sin(th2) - phi2)/(m2*lc2*lc2 + I2 - d2*d2/d1)
	    val ddth1 = -(d2*ddth2 + phi1)/d1
	    dth1 = bound(dth1 + i/4.0*deltaT*ddth1, minDotTheta1, maxDotTheta1)
	    th1 = bound(th1 + i/4.0*deltaT*dth1, minTheta1, maxTheta1)
	    dth2 = bound(dth2 + i/4.0*deltaT*ddth2, minDotTheta2, maxDotTheta2)
	    th2 = bound(th2 + i/4.0*deltaT*dth2, minTheta2, maxTheta2)
    }	
  }
}
object AcrobotModel {
  import Math._
  val minTheta1 = -Pi
  val maxTheta1 = Pi
  val minDotTheta1 = -4.0*Pi
  val maxDotTheta1 = 4.0*Pi
  val minTheta2 = -Pi
  val maxTheta2 = Pi
  val minDotTheta2 = -9.0*Pi
  val maxDotTheta2 = 9.0*Pi
  val m1 = 1.0
  val m2 = 1.0
  val l1 = 1.0
  val l2 = 1.0
  val lc1 = 0.5
  val lc2 = 0.5
  val I1 = 1.0
  val I2 = 1.0
  val G = 9.8
  
  val deltaT = 0.2 //0.05
  val maxForce = 1.0
}
