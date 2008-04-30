package net.pragyah.scalgorithms.aima.logic.propositional.inference

import junit.framework.TestCase
import junit.framework.Assert._

import scala.collection.mutable.Set

class TestForwardChaining extends TestCase{
 
  def testAlgo =  {

    val A = Symbol("A")
    val B = Symbol("B")
    val M = Symbol("M")
    val L = Symbol("L")
    val P = Symbol("P")
    val Q = Symbol("Q")
    
    val kb = KnowledgeBase[HornClause]()
    kb += new HornClause(A::B::Nil,L)
    kb += new HornClause(A::P::Nil,L)
    kb += new HornClause(L::B::Nil,M)
    kb += new HornClause(L::M::Nil,P)
    kb += new HornClause(M::P::Nil,Q)

    
    kb.tellTrue(A)
    kb.tellTrue(B)

    val infAlgo = new ForwardChaining()
    assertTrue(infAlgo.entails(kb,Q))

  }
  
  

}
