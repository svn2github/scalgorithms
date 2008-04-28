package net.pragyah.scalgorithms.aima.logic.propositional.inference

import junit.framework.TestCase
import junit.framework.Assert._

import scala.collection.mutable.Set

class TestResolution extends TestCase{
  
  
  
  
  def testPairs =  {
    
    val resolution = new Resolution();
    
    val l = 1::2::3::4::5::6::Nil
    var expected = Set[(int,int)]()
    expected += ((1,2))
    expected += ((1,3))
    expected += ((1,4))
    expected += ((1,5))
    expected += ((1,6))
    
    expected += ((2,3))
    expected += ((2,4))
    expected += ((2,5))
    expected += ((2,6))
    
    expected += ((3,4))
    expected += ((3,5))
    expected += ((3,6))
    
    expected += ((4,5))
    expected += ((4,6))
    
    expected += ((5,6))

    val set = resolution.pairs[int](l)

    assertEquals(set.size,expected.size)
    assert(expected.subsetOf(set))
    assert(set.subsetOf(expected))
  }
  
  

}
