package net.pragyah.scalgorithms

import net.pragyah.scalgorithms.heaps._
object AllTests {
  
  def suite : junit.framework.Test = {
    val suite = new junit.framework.TestSuite    
    suite.addTestSuite(classOf[TestBinomialHeap])
    suite.addTestSuite(classOf[TestFibonacciHeap])
    
    return suite
  }
}
