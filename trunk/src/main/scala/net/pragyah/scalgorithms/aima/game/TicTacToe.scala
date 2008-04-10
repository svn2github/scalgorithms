package net.pragyah.scalgorithms.aima.game

object TicTacToe{
  val X = Player("X")
  val Y = Player("Y")
  
  def getOther(player:Player) = if(player == X) Y else X
}

class TicTacToe extends Game{
  
  
  def initialState():State = {
    val board = new Array[Array[Option[String]]](3,3)
    for(i <- 0 to 2){
		for(j <- 0 to 2){
		  board(i)(j) = None
		}
    }
    TicTacToeState(board,TicTacToe.X)
  }
  
  def successors(state:State) : List[State] ={
    state.successors
  }
  
  def utility(state:State) : double = {

    val board = state.asInstanceOf[TicTacToeState].board
    
    for(row <- 0 to 2){
	  if(board(row)(0) != None && board(row)(1) != None  && board(row)(2) != None 
                 && board(row)(0).get == board(row)(1).get && board(row)(1).get == board(row)(2).get){
	    if(board(row)(1).get == "X") return 1 else return -1
	  }
    }

    for(col <- 0 to 2){
	  if(board(0)(col) != None && board(1)(col) != None  && board(2)(col) != None
	    && board(0)(col).get == board(1)(col).get && board(1)(col).get == board(2)(col).get ){
	    if(board(0)(col).get == "X") 1 else -1
	  }
    }
    
    if(board(0)(0) != None && board(1)(1) != None && board(2)(2) != None
      && board(0)(0).get == board(1)(1).get && board(1)(1).get == board(2)(2).get){
	    if(board(1)(1) == "X") 1 else -1
    }
      
    if(board(2)(0) != None && board(1)(1) != None && board(0)(2) != None
      && board(2)(0).get == board(1)(1).get && board(1)(1).get == board(0)(2).get){
	    if(board(1)(1) == "X") 1 else -1
    }
    
    0
  }
  
  def terminalTest(state:State) : boolean = {
    
    
    
    
    
    false//TODO
  }
}

object TicTacToeState{
  def apply(board:Array[Array[Option[String]]],whoActed:Player) = new TicTacToeState(board,whoActed)
}

class TicTacToeState(val board:Array[Array[Option[String]]],val whoIsToAct:Player) extends State{
  
  assume(board.size == 3)
  assume(board(0).size == 3)

  def successors(): List[State] ={
    
    var successors = List[State]()
    
    for(i <- 0 to 2){
		for(j <- 0 to 2){
		  if(board(i)(j) == None){
		    val newBoard = cloneBoard
            newBoard(i)(j) = Some(whoIsToAct.id)
            successors = successors ::: List(new TicTacToeState(newBoard,TicTacToe.getOther(whoIsToAct)))
		  }
		}
    }
    successors
  }
  
  
  private def cloneBoard() : Array[Array[Option[String]]] = {
    val clone = new Array[Array[Option[String]]](3,3);
    for(i <- 0 to 2){
		for(j <- 0 to 2){
		    clone(i)(j) = board(i)(j)
		}
	}	
    clone
  }
  
  private def getAsList() : List[Option[String]] = {
    var list = List[Option[String]]()
    for(i <- 0 to 2){
		for(j <- 0 to 2){
		    list = list ::: List(board(i)(j))  
		}
	}	
    list
  }
  
  override def toString:String = {
    var sb = new StringBuffer
    sb = sb.append("Next Player ").append(whoIsToAct).append("\nBoard\n")
    for(i <- 0 to 2){
      sb = sb.append("\n")
      
		for(j <- 0 to 2){
		   sb = sb.append(board(i)(j)).append("\t")
		}
	}	
    sb.toString
  }
  
}
