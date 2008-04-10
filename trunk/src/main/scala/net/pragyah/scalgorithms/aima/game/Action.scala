package net.pragyah.scalgorithms.aima.game

object Action{
 def apply(from:State,to:State,utility:double) = new Action(from,to,utility) 
}

class Action(val from:State,val to:State,val utility:double) {
  
}
