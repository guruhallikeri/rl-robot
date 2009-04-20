package edu.torax.reinforcement.framework.tests
import org.scalatest.Suite
import org.scalatest.PrivateMethodTester

class NeuralNetworkTests extends Suite with PrivateMethodTester {

  def testSimpleNetworkCreation {
    val net = new NeuralNetwork (
      List(2, 3, 2, 1), 
      List((x:Double) => Math.exp(x),(x:Double) => Math.exp(x), (x:Double) => Math.exp(x)),
      List((x:Double) => Math.exp(x),(x:Double) => Math.exp(x), (x:Double) => Math.exp(x))
    ) {
      val gamma = 1.0
      val lambda = 1.0
      val alpha = 1.0
      val initializer = 0.0
    }
    
    assert(net.inputDimension == 2)
    assert(net.outputDimension == 1)
    assert(net.layersCount == 3)
    
    val tmp = net.network
    assert(tmp(0).size == 3)
    assert(tmp(1).size == 2)
    assert(tmp(2).size == 1)
    assert(tmp(0)(0).size == 2+1, "Size is equal " + tmp(0)(0).size)
    assert(tmp(1)(0).size == 3+1, "Size is equal " + tmp(1)(0).size)
    assert(tmp(2)(0).size == 2+1, "Size is equal " + tmp(2)(0).size)

    val input = List(0.0,0.0)
    //assert(net.calculate(input).size == 1)
    val res = net.calculate(input)
    assert(Math.abs(res - 1.0) < 1e-6, "Network output: " + res)
  }

  def testSmallNetworkCalculation {
    val net = new NeuralNetwork (
      List(2, 3, 1), 
      List((x:Double) => Math.exp(x),(x:Double) => x),
      List((x:Double) => Math.exp(x),(x:Double) => 1.0)
    ) {
      val gamma = 1.0
      val lambda = 1.0
      val alpha = 1.0
      val initializer = 1.0
    }
    
    assert(net.inputDimension == 2)
    assert(net.outputDimension == 1)
    assert(net.layersCount == 2)

    val input = List(0.5, 0.5)
    //assert(net.calculate(input).size == 1)
    val res = net.calculate(input)
    val need = 3.0*Math.exp(2.0) + 1
    assert(Math.abs(res - need) < 1e-6, "Network output: " + res + " and must be " + need)
  }
  
  def testNetworkTuneUp {
    import Math.exp
    import Math.pow
    import Math.abs
    val net = new NeuralNetwork (
      List(2, 3, 1), 
      List((x:Double) => 1.0 / (1.0 + exp(-x)), (x:Double) => x),
      List((x:Double) => exp(-x) / pow(1.0 + exp(-x), 2.0), (x:Double) => 1.0)
    ) {
      val gamma = 0.9
      val lambda = 0.8
      val alpha = 0.3
      val initializer = 0.5
    }

    val eps = 1e-7
    val needed = 0.5 + 1.5 / (1.0 + exp(-0.95))
    val got = net.calculate(List(0.6, 0.3))
    assert(abs(needed - got) < eps, "Needed is: " + needed + ", but got: " + got)
    
    
    val e0 = 0.95
    val e1 = 1.0 / (1.0 + exp(-e0))
    val e2 = 0.5 + 1.5 * e1

    val grad1 = (0.5 - e2)
    val delta10 = grad1
    val delta11 = grad1 * e1
   
    net.tuneUp(List(0.6,0.3), 0.5)
    for (i <- 0 until 2; j <- 0 until net.network(i).size; k <- 0 until net.network(i)(j).size)
      assert(abs(net.network(i)(j)(k)-0.5) < eps, "Coefficients after tuneup is changed, but shouldn't!")

    assert(abs(net.eTraces(1)(0)(0)-delta10)<eps, "Should: " + delta10 + ", but is: " + net.eTraces(1)(0)(0))
    assert(abs(net.eTraces(1)(0)(1)-delta11)<eps, "Should: " + delta11 + ", but is: " + net.eTraces(1)(0)(1))
    assert(abs(net.eTraces(1)(0)(2)-delta11)<eps, "Should: " + delta11 + ", but is: " + net.eTraces(1)(0)(2))
    assert(abs(net.eTraces(1)(0)(3)-delta11)<eps, "Should: " + delta11 + ", but is: " + net.eTraces(1)(0)(3))
    
    val e3 = exp(-e0)/pow(1+exp(-e0), 2.0)
    val grad0 = 0.5 * grad1 * e3
    val delta00 = grad0
    val delta01 = grad0*0.6
    val delta02 = grad0*0.3

    for (i <- 0 until 3) {
      assert(abs(net.eTraces(0)(i)(0)-delta00)<eps, "Should: " + delta00 + ", but is: " + net.eTraces(0)(i)(0))
      assert(abs(net.eTraces(0)(i)(1)-delta01)<eps, "Should: " + delta01 + ", but is: " + net.eTraces(0)(i)(1))
      assert(abs(net.eTraces(0)(i)(2)-delta02)<eps, "Should: " + delta02 + ", but is: " + net.eTraces(0)(i)(2))
    }

    val f1 = (x:Double) => 1.0 / (1.0 + exp(-x))
    val f2 = (x:Double) => x
    val df1 = (x:Double) => exp(-x) / pow(1.0 + exp(-x), 2.0)
    val df2 = (x:Double) => 1.0
    
    val out2 = net.calculate(List(0.6, 0.3))
    assert(abs(out2 - needed)<eps, "Second output should be: " + needed + ", but is: " + out2)
    
    net.tuneUp(List(0.6, 0.3), 0.5)
    val e10 = delta10*net.lambda*net.gamma + delta10
    val e11 = delta11*net.lambda*net.gamma + delta11
    val w10 = 0.5 - net.alpha*(0.5-needed)*delta10
    val w11 = 0.5 - net.alpha*(0.5-needed)*delta11
    
    assert(abs(net.eTraces(1)(0)(0)-e10)<eps, "Should: " + e10 + ", but is: " + net.eTraces(1)(0)(0))
    assert(abs(net.eTraces(1)(0)(1)-e11)<eps, "Should: " + e11 + ", but is: " + net.eTraces(1)(0)(1))
    assert(abs(net.eTraces(1)(0)(2)-e11)<eps, "Should: " + e11 + ", but is: " + net.eTraces(1)(0)(2))
    assert(abs(net.eTraces(1)(0)(3)-e11)<eps, "Should: " + e11 + ", but is: " + net.eTraces(1)(0)(3))
    
    assert(abs(net.network(1)(0)(0)-w10)<eps, "Should: " + w10 + ", but is: " + net.network(1)(0)(0))
    assert(abs(net.network(1)(0)(1)-w11)<eps, "Should: " + w11 + ", but is: " + net.network(1)(0)(1))
    assert(abs(net.network(1)(0)(2)-w11)<eps, "Should: " + w11 + ", but is: " + net.network(1)(0)(2))
    assert(abs(net.network(1)(0)(3)-w11)<eps, "Should: " + w11 + ", but is: " + net.network(1)(0)(3))

    val e00 = net.lambda*net.gamma*delta00 + delta00
    val e01 = net.lambda*net.gamma*delta01 + delta01
    val e02 = net.lambda*net.gamma*delta02 + delta02

    val w00 = 0.5 - net.alpha*(0.5-needed)*delta00 
    val w01 = 0.5 - net.alpha*(0.5-needed)*delta01 
    val w02 = 0.5 - net.alpha*(0.5-needed)*delta02
    
    for (i <- 0 until 3) {
      assert(abs(net.eTraces(0)(i)(0)-e00)<eps, "Should: " + e00 + ", but is: " + net.eTraces(0)(i)(0))
      assert(abs(net.eTraces(0)(i)(1)-e01)<eps, "Should: " + e01 + ", but is: " + net.eTraces(0)(i)(1))
      assert(abs(net.eTraces(0)(i)(2)-e02)<eps, "Should: " + e02 + ", but is: " + net.eTraces(0)(i)(2))
    }
    for (i <- 0 until 3) {
      assert(abs(net.network(0)(i)(0)-w00)<eps, "Should: " + w00 + ", but is: " + net.network(0)(i)(0))
      assert(abs(net.network(0)(i)(1)-w01)<eps, "Should: " + w01 + ", but is: " + net.network(0)(i)(1))
      assert(abs(net.network(0)(i)(2)-w02)<eps, "Should: " + w02 + ", but is: " + net.network(0)(i)(2))
    }
  }
  def testNetworkNegativeTuneUp {
    import Math.exp
    import Math.pow
    import Math.abs
    val net = new NeuralNetwork (
      List(2, 3, 1), 
      List((x:Double) => 2.0 * (1.0 / (1.0 + exp(-x)) - 0.5), (x:Double) => x),
      List((x:Double) => 2.0 * exp(-x) / pow(1.0 + exp(-x), 2.0), (x:Double) => 1.0)
    ) {
      val gamma = 0.9
      val lambda = 0.8
      val alpha = 0.3
      val initializer = 0.5
    }

    val eps = 1e-7
    
    
    val e0 = 0.95
    val e1 = 2*(1.0 / (1.0 + exp(-e0)) - 0.5)
    val e2 = 0.5 + 1.5 * e1

    val grad1 = (-0.5 - e2)
    val delta10 = grad1
    val delta11 = grad1 * e1
   
    val got = net.calculate(List(0.6, 0.3))
    assert(abs(e2 - got) < eps, "Needed is: " + e2 + ", but got: " + got)

    net.tuneUp(List(0.6,0.3), -0.5)
    for (i <- 0 until 2; j <- 0 until net.network(i).size; k <- 0 until net.network(i)(j).size)
      assert(abs(net.network(i)(j)(k)-0.5) < eps, "Coefficients after tuneup is changed, but shouldn't!")

    assert(abs(net.eTraces(1)(0)(0)-delta10)<eps, "Should: " + delta10 + ", but is: " + net.eTraces(1)(0)(0))
    assert(abs(net.eTraces(1)(0)(1)-delta11)<eps, "Should: " + delta11 + ", but is: " + net.eTraces(1)(0)(1))
    assert(abs(net.eTraces(1)(0)(2)-delta11)<eps, "Should: " + delta11 + ", but is: " + net.eTraces(1)(0)(2))
    assert(abs(net.eTraces(1)(0)(3)-delta11)<eps, "Should: " + delta11 + ", but is: " + net.eTraces(1)(0)(3))
    
    val e3 = 2.0 * exp(-e0)/pow(1+exp(-e0), 2.0)
    val grad0 = 0.5 * grad1 * e3
    val delta00 = grad0
    val delta01 = grad0*0.6
    val delta02 = grad0*0.3

    for (i <- 0 until 3) {
      assert(abs(net.eTraces(0)(i)(0)-delta00)<eps, "Should: " + delta00 + ", but is: " + net.eTraces(0)(i)(0))
      assert(abs(net.eTraces(0)(i)(1)-delta01)<eps, "Should: " + delta01 + ", but is: " + net.eTraces(0)(i)(1))
      assert(abs(net.eTraces(0)(i)(2)-delta02)<eps, "Should: " + delta02 + ", but is: " + net.eTraces(0)(i)(2))
    }

    val f1 = (x:Double) => 2*(1.0 / (1.0 + exp(-x)) - 0.5)
    val f2 = (x:Double) => x
    val df1 = (x:Double) => 2.0 * exp(-x) / pow(1.0 + exp(-x), 2.0)
    val df2 = (x:Double) => 1.0
    
    val out2 = net.calculate(List(0.6, 0.3))
    assert(abs(out2 - e2)<eps, "Second output should be: " + e2 + ", but is: " + out2)
    
    net.tuneUp(List(0.6, 0.3), -0.5)
    val e10 = delta10*net.lambda*net.gamma + delta10
    val e11 = delta11*net.lambda*net.gamma + delta11
    val w10 = 0.5 - net.alpha*(-0.5-e2)*delta10
    val w11 = 0.5 - net.alpha*(-0.5-e2)*delta11
    
    assert(abs(net.eTraces(1)(0)(0)-e10)<eps, "Should: " + e10 + ", but is: " + net.eTraces(1)(0)(0))
    assert(abs(net.eTraces(1)(0)(1)-e11)<eps, "Should: " + e11 + ", but is: " + net.eTraces(1)(0)(1))
    assert(abs(net.eTraces(1)(0)(2)-e11)<eps, "Should: " + e11 + ", but is: " + net.eTraces(1)(0)(2))
    assert(abs(net.eTraces(1)(0)(3)-e11)<eps, "Should: " + e11 + ", but is: " + net.eTraces(1)(0)(3))
    
    assert(abs(net.network(1)(0)(0)-w10)<eps, "Should: " + w10 + ", but is: " + net.network(1)(0)(0))
    assert(abs(net.network(1)(0)(1)-w11)<eps, "Should: " + w11 + ", but is: " + net.network(1)(0)(1))
    assert(abs(net.network(1)(0)(2)-w11)<eps, "Should: " + w11 + ", but is: " + net.network(1)(0)(2))
    assert(abs(net.network(1)(0)(3)-w11)<eps, "Should: " + w11 + ", but is: " + net.network(1)(0)(3))

    val e00 = net.lambda*net.gamma*delta00 + delta00
    val e01 = net.lambda*net.gamma*delta01 + delta01
    val e02 = net.lambda*net.gamma*delta02 + delta02

    val w00 = 0.5 - net.alpha*(-0.5-e2)*delta00 
    val w01 = 0.5 - net.alpha*(-0.5-e2)*delta01 
    val w02 = 0.5 - net.alpha*(-0.5-e2)*delta02
    
    for (i <- 0 until 3) {
      assert(abs(net.eTraces(0)(i)(0)-e00)<eps, "Should: " + e00 + ", but is: " + net.eTraces(0)(i)(0))
      assert(abs(net.eTraces(0)(i)(1)-e01)<eps, "Should: " + e01 + ", but is: " + net.eTraces(0)(i)(1))
      assert(abs(net.eTraces(0)(i)(2)-e02)<eps, "Should: " + e02 + ", but is: " + net.eTraces(0)(i)(2))
    }
    for (i <- 0 until 3) {
      assert(abs(net.network(0)(i)(0)-w00)<eps, "Should: " + w00 + ", but is: " + net.network(0)(i)(0))
      assert(abs(net.network(0)(i)(1)-w01)<eps, "Should: " + w01 + ", but is: " + net.network(0)(i)(1))
      assert(abs(net.network(0)(i)(2)-w02)<eps, "Should: " + w02 + ", but is: " + net.network(0)(i)(2))
    }
    //println(e2)
    //println(net.calculate(List(0.6,0.3)))
  }
}
