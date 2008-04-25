package net.pragyah.scalgorithms.aima.logic.propositional


import junit.framework.TestCase
import junit.framework.Assert._

class TestModel extends TestCase{
  
  def testModel = {
    val m = Model()
    val s = Symbol("A")
    val m1 = m + s
    assert(m1 != m) //immutability


    
    null
  }

}