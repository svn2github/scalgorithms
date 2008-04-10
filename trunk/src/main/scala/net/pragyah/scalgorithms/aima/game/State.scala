package net.pragyah.scalgorithms.aima.game

trait State {
  def successors:List[State]
}
