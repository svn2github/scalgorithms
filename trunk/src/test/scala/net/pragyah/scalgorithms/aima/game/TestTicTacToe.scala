package net.pragyah.scalgorithms.aima.game

import junit.framework.TestCase
import junit.framework.Assert._

import scala.collection.mutable.HashSet

class TestTicTacToe extends TestCase{
  
  def testInitialBoard = {
    val ttt = new TicTacToe()
    val is = ttt.initialState
    
    val isttt = is.asInstanceOf[TicTacToeState]
    isttt.board.foreach(row => {
                         row.foreach(
                           cell => assert(cell == None)
                         )
                       })

    val set = new HashSet[String]()
    set += "X--------"
    set += "-X-------"
    set += "--X------"
    set += "---X-----"
    set += "----X----"
    set += "-----X---"
    set += "------X--"
    set += "-------X-"
    set += "--------X"
    
    isttt.successors.foreach(state => {
                               set -= state.asInstanceOf[TicTacToeState].getAsString
                             })
    
    assert(set.size == 0)
    

  }
  
  def testRandomBoardX = {
    val game:TicTacToe = new TicTacToe()
    
    val arr = new Array[Array[Option[String]]](3,3)
    arr(0)(0) = Some("X")
    arr(0)(1) = Some("X")
    arr(0)(2) = Some("O")
    
    arr(1)(0) = None
    arr(1)(1) = Some("X")
    arr(1)(2) = None
    
    arr(2)(0) = None
    arr(2)(1) = Some("O")
    arr(2)(2) = None
    
    
    val tttState = new TicTacToeState(arr,TicTacToe.X)
    
    
    assert(tttState.getAsString == "XXO-X--O-")
    val utility = game.utility(tttState)
    assert(utility == -2)
    
    val successors = tttState.successors
    
    val set = new HashSet[String]()
    set += "XXOXX--O-"
    set += "XXO-XX-O-"
    set += "XXO-X-XO-"
    set += "XXO-X--OX"

    
	successors.foreach(successor => (successor.asInstanceOf[TicTacToeState]).whoIsToAct == TicTacToe.Y)

    successors.foreach(state => {
                               set -= state.asInstanceOf[TicTacToeState].getAsString
                             })
    assert(set.size == 0)
  }
  
  def testRandomBoardY = {
    val game:TicTacToe = new TicTacToe()
    
    val arr = new Array[Array[Option[String]]](3,3)
    arr(0)(0) = None
    arr(0)(1) = Some("X")
    arr(0)(2) = Some("O")
    
    arr(1)(0) = None
    arr(1)(1) = Some("X")
    arr(1)(2) = None
    
    arr(2)(0) = Some("O")
    arr(2)(1) = Some("O")
    arr(2)(2) = None
    
    
    val tttState = new TicTacToeState(arr,TicTacToe.Y)
    
    
    assert(tttState.getAsString == "-XO-X-OO-")
    val utility = game.utility(tttState)
    assert(utility == -2)
    
    val successors = tttState.successors
    
    val set = new HashSet[String]()
    set += "OXO-X-OO-"
    set += "-XOOX-OO-"
    set += "-XO-XOOO-"
    set += "-XO-X-OOO"

    
	successors.foreach(successor => (successor.asInstanceOf[TicTacToeState]).whoIsToAct == TicTacToe.X)

    successors.foreach(state => {
                               set -= state.asInstanceOf[TicTacToeState].getAsString
                             })
    assert(set.size == 0)
  }
  
  def testUtilityValuesX = {
    val game:TicTacToe = new TicTacToe()
    
    var arr = new Array[Array[Option[String]]](3,3)
    arr(0)(0) = Some("X")
    arr(0)(1) = Some("X")
    arr(0)(2) = Some("X")
    
    arr(1)(0) = None
    arr(1)(1) = Some("X")
    arr(1)(2) = None
    
    arr(2)(0) = Some("O")
    arr(2)(1) = Some("O")
    arr(2)(2) = None

    var tttState = new TicTacToeState(arr,TicTacToe.Y)
    game.utility(tttState)

    assert(tttState.getAsString == "XXX-X-OO-")

    var utility = game.utility(tttState)
    assert(utility == 1)


    arr = new Array[Array[Option[String]]](3,3)
    arr(0)(0) = Some("-")
    arr(0)(1) = Some("X")
    arr(0)(2) = Some("X")
    
    arr(1)(0) = Some("X")
    arr(1)(1) = Some("X")
    arr(1)(2) = Some("X")
    
    arr(2)(0) = Some("O")
    arr(2)(1) = Some("O")
    arr(2)(2) = None

    tttState = new TicTacToeState(arr,TicTacToe.Y)
    game.utility(tttState)

    assert(tttState.getAsString == "-XXXXXOO-")

    utility = game.utility(tttState)
    assert(utility == 1)

    arr = new Array[Array[Option[String]]](3,3)
    arr(0)(0) = Some("-")
    arr(0)(1) = Some("X")
    arr(0)(2) = Some("X")
    
    arr(1)(0) = Some("O")
    arr(1)(1) = Some("O")
    arr(1)(2) = Some("X")
    
    arr(2)(0) = Some("X")
    arr(2)(1) = Some("X")
    arr(2)(2) = Some("X")

    tttState = new TicTacToeState(arr,TicTacToe.Y)
    game.utility(tttState)

    assert(tttState.getAsString == "-XXOOXXXX")

    utility = game.utility(tttState)
    assert(utility == 1)

    
    arr = new Array[Array[Option[String]]](3,3)
    arr(0)(0) = Some("X")
    arr(0)(1) = Some("O")
    arr(0)(2) = Some("X")
    
    arr(1)(0) = Some("O")
    arr(1)(1) = Some("X")
    arr(1)(2) = Some("X")
    
    arr(2)(0) = None
    arr(2)(1) = Some("O")
    arr(2)(2) = Some("X")

    tttState = new TicTacToeState(arr,TicTacToe.Y)
    game.utility(tttState)

    assert(tttState.getAsString == "XOXOXX-OX")

    utility = game.utility(tttState)
    assert(utility == 1)

    
    
    arr = new Array[Array[Option[String]]](3,3)
    arr(0)(0) = Some("X")
    arr(0)(1) = Some("O")
    arr(0)(2) = Some("X")
    
    arr(1)(0) = Some("O")
    arr(1)(1) = Some("X")
    arr(1)(2) = Some("X")
    
    arr(2)(0) = Some("X")
    arr(2)(1) = Some("O")
    arr(2)(2) = Some("O")

    tttState = new TicTacToeState(arr,TicTacToe.Y)
    game.utility(tttState)

    assert(tttState.getAsString == "XOXOXXXOO")

    utility = game.utility(tttState)
    assert(utility == 1)
    
  }
  
  def testUtilityValuesY = {
    val game:TicTacToe = new TicTacToe()
    
    var arr = new Array[Array[Option[String]]](3,3)
    arr(0)(0) = Some("O")
    arr(0)(1) = Some("O")
    arr(0)(2) = Some("O")
    
    arr(1)(0) = None
    arr(1)(1) = Some("X")
    arr(1)(2) = None
    
    arr(2)(0) = Some("O")
    arr(2)(1) = Some("O")
    arr(2)(2) = None

    var tttState = new TicTacToeState(arr,TicTacToe.Y)
    game.utility(tttState)

    assert(tttState.getAsString == "OOO-X-OO-")

    var utility = game.utility(tttState)
    assert(utility == -1)


    arr = new Array[Array[Option[String]]](3,3)
    arr(0)(0) = Some("-")
    arr(0)(1) = Some("X")
    arr(0)(2) = Some("X")
    
    arr(1)(0) = Some("O")
    arr(1)(1) = Some("O")
    arr(1)(2) = Some("O")
    
    arr(2)(0) = Some("X")
    arr(2)(1) = Some("O")
    arr(2)(2) = None

    tttState = new TicTacToeState(arr,TicTacToe.Y)
    game.utility(tttState)

    assert(tttState.getAsString == "-XXOOOXO-")

    utility = game.utility(tttState)
    assert(utility == -1)

    arr = new Array[Array[Option[String]]](3,3)
    arr(0)(0) = Some("-")
    arr(0)(1) = Some("X")
    arr(0)(2) = Some("X")
    
    arr(1)(0) = Some("O")
    arr(1)(1) = Some("O")
    arr(1)(2) = Some("X")
    
    arr(2)(0) = Some("O")
    arr(2)(1) = Some("O")
    arr(2)(2) = Some("O")

    tttState = new TicTacToeState(arr,TicTacToe.Y)
    game.utility(tttState)

    assert(tttState.getAsString == "-XXOOXOOO")

    utility = game.utility(tttState)
    assert(utility == -1)

    
    arr = new Array[Array[Option[String]]](3,3)
    arr(0)(0) = Some("O")
    arr(0)(1) = Some("O")
    arr(0)(2) = Some("X")
    
    arr(1)(0) = Some("O")
    arr(1)(1) = Some("O")
    arr(1)(2) = Some("X")
    
    arr(2)(0) = None
    arr(2)(1) = Some("X")
    arr(2)(2) = Some("O")

    tttState = new TicTacToeState(arr,TicTacToe.Y)
    game.utility(tttState)

    assert(tttState.getAsString == "OOXOOX-XO")

    utility = game.utility(tttState)
    assert(utility == -1)

    
    
    arr = new Array[Array[Option[String]]](3,3)
    arr(0)(0) = Some("X")
    arr(0)(1) = Some("O")
    arr(0)(2) = Some("O")
    
    arr(1)(0) = Some("O")
    arr(1)(1) = Some("O")
    arr(1)(2) = Some("X")
    
    arr(2)(0) = Some("O")
    arr(2)(1) = Some("X")
    arr(2)(2) = Some("O")

    tttState = new TicTacToeState(arr,TicTacToe.Y)
    game.utility(tttState)

    assert(tttState.getAsString == "XOOOOXOXO")

    utility = game.utility(tttState)
    assert(utility == -1)
    
  }

  def testUtilityValuesNoWin = {
    val game:TicTacToe = new TicTacToe()
    
    var arr = new Array[Array[Option[String]]](3,3)
    arr(0)(0) = Some("O")
    arr(0)(1) = Some("X")
    arr(0)(2) = Some("O")
    
    arr(1)(0) = Some("X")
    arr(1)(1) = Some("O")
    arr(1)(2) = Some("X")
    
    arr(2)(0) = Some("X")
    arr(2)(1) = Some("O")
    arr(2)(2) = Some("X")

    var tttState = new TicTacToeState(arr,TicTacToe.Y)
    game.utility(tttState)

    assert(tttState.getAsString == "OXOXOXXOX")

    var utility = game.utility(tttState)
    assert(utility == 0)


    arr = new Array[Array[Option[String]]](3,3)
    arr(0)(0) = Some("X")
    arr(0)(1) = Some("O")
    arr(0)(2) = Some("X")
    
    arr(1)(0) = Some("O")
    arr(1)(1) = Some("O")
    arr(1)(2) = Some("X")
    
    arr(2)(0) = Some("X")
    arr(2)(1) = Some("X")
    arr(2)(2) = Some("O")

    tttState = new TicTacToeState(arr,TicTacToe.Y)
    game.utility(tttState)

    assert(tttState.getAsString == "XOXOOXXXO")

    utility = game.utility(tttState)
    assert(utility == 0)
    
  }
}
