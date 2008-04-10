package net.pragyah.scalgorithms.aima.game

trait Game {
  
  
  def successors(state:State) : List[State]
  def utility(state:State) : double
  def terminalTest(state:State) : boolean
  def initialState():State
  
  var currentState:State = initialState();
  
  def maxDecision(state:State) : Action = {
    val action = maxValue(state)
    currentState = action.to
    return action
  }

  def minDecision(state:State) : Action = {
    val action = minValue(state)
    currentState = action.to
    return action
  }

  def maxValue(state:State) : Action = {

    if(terminalTest(state)) return Action(state,null,utility(state))
    var action = Action(state,null,Double.MinValue)
    
    successors(state).foreach( s => {
	      val minAction = minValue(s)
	      action = if(minAction.utility > action.utility) Action(state,minAction.from,minAction.utility) else action
      }
    )
    action
  }
  
  
  def minValue(state:State) : Action = {

    if(terminalTest(state)) return Action(state,null,utility(state))
    var action = Action(state,null,Double.MaxValue)
    successors(state).foreach( s => {
	      val maxAction = maxValue(s)
	      action = if(maxAction.utility < action.utility) Action(state,maxAction.from,maxAction.utility) else action
      }
    )
    action
  }
  

  def maxDecisionAlphaBeta(state:State) : Action = {
    val action = maxValue(state,(Double.MinValue,Double.MaxValue))
    currentState = action.to
    return action
  }

  def minDecisionAlphaBeta(state:State) : Action = {
    val action = minValue(state,(Double.MinValue,Double.MaxValue))
    currentState = action.to
    return action
  }

  
/*  
 * alpha  = the value of the best(i.e. highest-value) choice we have found so far at any chouce point along the path for MAX
 * beta = the value of the best (i.e. lowest-value) choice we have found so far at any choice point along the path for MIN
 */  
  def maxValue(state:State,alphaBeta:(double,double)) : Action = {

    if(terminalTest(state)) return Action(state,null,utility(state))
    var action = Action(state,null,Double.MinValue)
    
    var alpha = alphaBeta._1
    val beta = alphaBeta._2
    
    successors(state).foreach( s => {
          // if i transitoin to this state 's' .... let's find out what MIN would do from there on .... 
	      val minAction = minValue(s,(alpha,beta))
          // aahaa ... so this is what MIN would do
          // if min cannot make it any worse than what it is ... and infact let me have higher returns than the previous best .. then consider this action and give up the previous best 
	      action = if(minAction.utility > action.utility) Action(state,minAction.from,minAction.utility) else action

          /*
           * Let's see if MIN has found a better(for himself and worst for me) action elsewhere before calling me
           * If yes .. then there is no point in me going any further ... i may end up getting better and better options for myself
           * evaluating all the successor states .. but that bugger wont even come to this node ... so what's the point
           * return back this action telling him that i can beat you if you come here with whatever i have found already ... (even before considering all the nodes)
           * formal language - if the utility of this action is more than the worst (best for MIN) found so far .... 
           * don't go any further on this node .. it's no use since the MIN player won't come to this one anyways .. 
           * since he has a better option to go with
           */
          if(action.utility >= beta) return action  
          // -> if we have found a larger value than found so far (for MAX) .. capture it in alpha
          // let's see if what i have found so far is the best or not .... 
          alpha = if(alpha > action.utility)  
                     alpha // ok .. so this solution is not the best after all .... don't change alpha's value
                  else 
                     action.utility  // nice ... looks like i have found a better utility value ... capture it for future comparision... and for letting the successor states know what i have in store
      }
    )
    action
  }
  
  
  def minValue(state:State,alphaBeta:(double,double)) : Action = {

    if(terminalTest(state)) return Action(state,null,utility(state))
    var action = Action(state,null,Double.MaxValue)
    
    var beta = alphaBeta._2
    val alpha = alphaBeta._1

    successors(state).foreach( s => {
          // if i transitoin to this state 's' .... let's find out what MAX would do from there on .... 
	      val maxAction = maxValue(s,(alpha,beta))
          // aahaa ... so this is what MAX would do
          // if MAX cannot make it any worse (by increasing the utility) than what it is ... and infact let me have higher returns (lower utility) than the previous best .. then consider this action and give up the previous best 
	      action = if(maxAction.utility < action.utility) // hey ... is this expected move of Max give me a lower value than the best one (lowest one) i already have (with some other option)
                        Action(state,maxAction.from,maxAction.utility)  /// ok ... then make that as my best action so far
                   else 
                        action   // it's not better huh!? ... then let me retain the best
          /*
           * Let's see if MAX has found a better(for himself and worst for me) action elsewhere before calling me
           * If yes .. then there is no point in me going any further ... i may end up getting better and better options for myself (with lower utility)
           * evaluating all the successor states .. but that bugger wont even come to this node ... so what's the point
           * return back this action telling him that i can beat you if you come here with whatever i have found already ... (even before considering all the nodes)
           * formal language -> if the utility of this action is lesser than the best (for MAX) we have found so far ... 
           * don't go further on this node .. it's no use since the MAX player won't come to this one anyways 
           * since he has a better option to go with
           */
          if(action.utility <= alpha) return action  
          // -> if we have found a lesser value than found so far (for MIN) capture it in beta
          // let's see if what i have found so far is the best for me or not ...               
          beta = if(beta < action.utility) 
                      beta // ok .. so this solution is not the best for me afterall ... don't change beta''s value ... i don't like this one
                 else 
                      action.utility // nice .. looks like i have found a better(lower) utility value ... capture it for future comparision ... and for letting the successor states know what i have in store
      }
    )
    action
    
    
  }
}
