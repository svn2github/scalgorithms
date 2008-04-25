package net.pragyah.scalgorithms.aima.logic.propositional


import scala.collection.mutable.{HashSet}

object TruthTable {
  
  def entails(kb:KnowledgeBase,alpha:Sentence) : boolean = {
    
    val symbols = new HashSet[Symbol]();
    symbols ++= kb.asSentence.symbols
    symbols ++= alpha.symbols
    
    val model = new Model()
    checkAll(kb,alpha,symbols.toList,model)
    
    false
  } 
  
  def checkAll(kb:KnowledgeBase,alpha:Sentence,symbols:List[Symbol],model:Model):boolean = {
    if(symbols.size == 0){
      if(model.isTrue(kb.asSentence)) return model.isTrue(alpha) else return true
    }
    return checkAll(kb,alpha,symbols.tail,extend(symbols.head,True(),model)) &&
      checkAll(kb,alpha,symbols.tail,extend(symbols.head,False(),model)) 
  }

  def extend(symbol:Symbol,v:TruthValue,model:Model):Model = {
    model + (symbol,v)
  }
  
}
