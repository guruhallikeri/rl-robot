package edu.torax.reinforcement.tests.framework
import org.scalatest.FunSuite
import reinforcement.framework._
//
//class UsualNeuralNetworkTests extends FunSuite {
//	test ("Check complicated usual neural network backprop") {
//		val w = Array(0.12, 0.7, 0.29, 0.22, 0.5, 0.8, 0.15, 0.07, 0.35, 0.3, 0.72, 0.8, 0.02, 0.25, 0.42, 0.13, 0.66)
//		var cur = 0
//		val eta = 0.1
//		val nn = new UsualNeuralNetwork(2, Array(3, 2), Array(NeuralNetwork.logistic, NeuralNetwork.logistic)) { 
//			val alpha = eta
//			def initializer() = {
//				val r = w(cur)
//				cur += 1
//				r
//			}
//		}
//
//		expect (w(0)) { nn.network(0)(0)(0) }
//		expect (w(1)) { nn.network(0)(0)(1) }
//		expect (w(2)) { nn.network(0)(0)(2) }
//		expect (w(3)) { nn.network(0)(1)(0) }
//		expect (w(4)) { nn.network(0)(1)(1) }
//		expect (w(5)) { nn.network(0)(1)(2) }
//		expect (w(6)) { nn.network(0)(2)(0) }
//		expect (w(7)) { nn.network(0)(2)(1) }
//		expect (w(8)) { nn.network(0)(2)(2) }
//
//		expect (w(9)) { nn.network(1)(0)(0) }
//		expect (w(10)) { nn.network(1)(0)(1) }
//		expect (w(11)) { nn.network(1)(0)(2) }
//		expect (w(12)) { nn.network(1)(0)(3) }
//
//		expect (w(13)) { nn.network(1)(1)(0) }
//		expect (w(14)) { nn.network(1)(1)(1) }
//		expect (w(15)) { nn.network(1)(1)(2) }
//		expect (w(16)) { nn.network(1)(1)(3) }
//		expect (17) { w.size }
//
//		import Math._	
//		def flog(x: Double) = 1.0 / (1.0 + exp(-x))
//		def fder(x: Double) = flog(x)*(1.0 - flog(x))
//		def calc(x1: Double, x2: Double): List[Double] = {
//			val v1 = w(0) + w(1)*x1 + w(2)*x2
//			val v2 = w(3) + w(4)*x1 + w(5)*x2
//			val v3 = w(6) + w(7)*x1 + w(8)*x2
//			val v4 = w(9) + w(10)*flog(v1) + w(11)*flog(v2) + w(12)*flog(v3)
//			val v5 = w(13) + w(14)*flog(v1) + w(15)*flog(v2) + w(16)*flog(v3)
//			List(flog(v4), flog(v5))
//		}
//		for (i <- 0 to 100) {
//			for (j <- 0 to 100) {
//				expect(calc(i/100.0, j/100.0)) { nn.calculate(List(i/100.0, j/100.0))}
//			}
//		}
//
//		val in = List(0.5, 0.7)
//		val out = nn.calculate(in)
//		val must = calc(in.head, in.tail.head)
//		expect (must) { out }
//
//		val want = List(0.2, 0.9)
//
//		val v1 = w(0) + w(1)*in(0) + w(2)*in(1)
//		val vv1 = flog(v1)
//		val v2 = w(3) + w(4)*in(0) + w(5)*in(1)
//		val vv2 = flog(v2)
//		val v3 = w(6) + w(7)*in(0) + w(8)*in(1)
//		val vv3 = flog(v3)
//		val v4 = w(9) + w(10)*flog(v1) + w(11)*flog(v2) + w(12)*flog(v3)
//		val v5 = w(13) + w(14)*flog(v1) + w(15)*flog(v2) + w(16)*flog(v3)
//
//		//	println("v1: " + flog(v1) + ", v2: " + flog(v2) + ", v3: " + flog(v3))
//		val grad10 = (want(0) - out(0))*fder(v4)
//		val grad11 = (want(1) - out(1))*fder(v5)
//		val dw100 = eta*1.0*grad10
//		val dw101 = eta*flog(v1)*grad10
//		val dw102 = eta*flog(v2)*grad10
//		val dw103 = eta*flog(v3)*grad10
//
//		val dw110 = eta*1.0*grad11
//		val dw111 = eta*flog(v1)*grad11
//		val dw112 = eta*flog(v2)*grad11
//		val dw113 = eta*flog(v3)*grad11
//
//		val grad00 = (w(10)*grad10 + w(14)*grad11)*fder(v1)
//		val grad01 = (w(11)*grad10 + w(15)*grad11)*fder(v2)
//		val grad02 = (w(12)*grad10 + w(16)*grad11)*fder(v3)
//
//		val dw000 = eta*grad00*1.0
//		val dw001 = eta*grad00*in(0)  
//		val dw002 = eta*grad00*in(1)  
//
//		val dw010 = eta*grad01*1.0
//		val dw011 = eta*grad01*in(0)  
//		val dw012 = eta*grad01*in(1)  
//
//		val dw020 = eta*grad02*1.0
//		val dw021 = eta*grad02*in(0)  
//		val dw022 = eta*grad02*in(1)  
//	
//		nn.tuneUp(in, want)
//
//		val ok = 1e-6
//		def check(x: Double, y: Double) = Math.max(Math.abs(x-y), ok)
//
//		expect (ok) { check(w(0) + dw000, nn.network(0)(0)(0)) }
//		expect (ok) { check(w(1) + dw001, nn.network(0)(0)(1)) }
//		expect (ok) { check(w(2) + dw002, nn.network(0)(0)(2)) }
//		expect (ok) { check(w(3) + dw010, nn.network(0)(1)(0)) }
//		expect (ok) { check(w(4) + dw011, nn.network(0)(1)(1)) }
//		expect (ok) { check(w(5) + dw012, nn.network(0)(1)(2)) }
//		expect (ok) { check(w(6) + dw020, nn.network(0)(2)(0)) }
//		expect (ok) { check(w(7) + dw021, nn.network(0)(2)(1)) }
//		expect (ok) { check(w(8) + dw022, nn.network(0)(2)(2)) }
//
//		expect (w(9) + dw100) { nn.network(1)(0)(0) }
//		expect (w(10) + dw101) { nn.network(1)(0)(1) }
//		expect (w(11) + dw102) { nn.network(1)(0)(2) }
//		expect (ok) { check(w(12) + dw103, nn.network(1)(0)(3)) }
//
//		expect (ok) { check(w(13) + dw110, nn.network(1)(1)(0)) }
//		expect (ok) { check(w(14) + dw111, nn.network(1)(1)(1)) }
//		expect (ok) { check(w(15) + dw112, nn.network(1)(1)(2)) }
//		expect (ok) { check(w(16) + dw113, nn.network(1)(1)(3)) }
//	}
//}