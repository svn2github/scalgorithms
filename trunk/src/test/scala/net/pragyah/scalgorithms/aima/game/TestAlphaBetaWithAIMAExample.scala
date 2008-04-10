package net.pragyah.scalgorithms.aima.game

import junit.framework.TestCase
import junit.framework.Assert._

class TestAlphaBetaWithAIMAExample extends TestCase{
  
  
  def testInitialBoard = {
    
    val game = new AIMASampleGame
    game.maxDecisionAlphaBeta(game.A)
    
    
    

  }
  
  def testBoard = {
    
    val game = new AIMASampleGame
    val action1:Action = game.maxDecisionAlphaBeta(game.A)
    
    assert(action1.from == game.A)
    assert(action1.to == game.B)
    assert(action1.utility == 3)


    val action2:Action = game.minDecisionAlphaBeta(action1.to)
    
    assert(action2.from == game.B)
    assert(action2.to == game.B1)
    assert(action2.utility == 3)
    
    val action3:Action = game.minDecisionAlphaBeta(action2.to)
    assert(action3.from == game.B1)
    assert(action3.to == null)
    assert(action3.utility == 3)
    

  }
  


}

class AIMASampleGame extends Game{
  
  val A = new SampleGameState("A",false,-1)
	  val B = new SampleGameState("B",false,-1)
		  val B1 = new SampleGameState("B1",true,3)
		  val B2 = new SampleGameState("B2",true,12)
		  val B3 = new SampleGameState("B3",true,8)
	  val C = new SampleGameState("C",false,-1)
		  val C1 = new SampleGameState("C1",true,2)
		  val C2 = new SampleGameState("C2",true,4)
		  val C3 = new SampleGameState("C3",true,6)
	  val D = new SampleGameState("D",false,-1)
		  val D1 = new SampleGameState("D1",true,14)
		  val D2 = new SampleGameState("D2",true,5)
		  val D3 = new SampleGameState("D3",true,2)
    A.s = B::C::D::Nil
    B.s = B1::B2::B3::Nil
    C.s = C1::C2::C3::Nil
    D.s = D1::D2::D3::Nil
    
  
  
  def successors(state:State) = state.successors

  def utility(state:State) = state.asInstanceOf[SampleGameState].utility
  
  def terminalTest(state:State)= state.asInstanceOf[SampleGameState].terminal
  
  def initialState():State = A

  
  
  class SampleGameState(val name:String,val terminal:boolean, val utility:int) extends State{
    var s:List[State] = Nil
    def successors:List[State] = s
    override def toString = name+":"+utility
  }
  
}
