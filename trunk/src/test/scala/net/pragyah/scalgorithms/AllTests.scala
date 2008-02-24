package net.pragyah.scalgorithms

import net.pragyah.scalgorithms.heaps._
import net.pragyah.scalgorithms.graphs._
import net.pragyah.scalgorithms.graphs.mst._
import net.pragyah.scalgorithms.sorting._
object AllTests {
  
  def suite : junit.framework.Test = {
    val suite = new junit.framework.TestSuite
    //Heaps
    suite.addTestSuite(classOf[TestBinomialHeap])
    suite.addTestSuite(classOf[TestFibonacciHeap])
    //Graphs
    suite.addTestSuite(classOf[TestGraphBasicsDirected])
    suite.addTestSuite(classOf[TestGraphBasicsUndirected])
    //Graph-MST
    suite.addTestSuite(classOf[TestKruskalsAlgorithm])
    suite.addTestSuite(classOf[TestPrimsAlgorithm])
    //Sorting
    suite.addTestSuite(classOf[TestQuickSort])

    
    return suite
  }
}
