package net.pragyah.scalgorithms.aima.game

import junit.framework.TestCase
import junit.framework.Assert._

class TestAlphaBetaWithTicTacToe extends TestCase{
  
  

  def testAIMAExample = {
    
    val game = new TicTacToe
    val is = game.initialState.successors.tail.tail.head
    println(is)
    val a1:Action = game.minDecisionAlphaBeta(is)
    /*a1.from.*/
    println(a1.to)
    
    val a2:Action = game.maxDecisionAlphaBeta(a1.to)
    println(a2.to)
    
    val a3:Action = game.minDecisionAlphaBeta(a2.to)
    println(a3.to)
    
    val a4:Action = game.maxDecisionAlphaBeta(a3.to)
    println(a4.to)
    
    val a5:Action = game.minDecisionAlphaBeta(a4.to)
    println(a5.to)
    
    val a6:Action = game.maxDecisionAlphaBeta(a5.to)
    println(a6.to)
    
    val a7:Action = game.minDecisionAlphaBeta(a6.to)
    println(a7.to)
    
    val a8:Action = game.maxDecisionAlphaBeta(a7.to)
    println(a8.to)
    
    
  }
  


}
