package net.pragyah.scalgorithms.aima.logic.propositional.inference


import scala.collection.mutable.{HashSet}

class TruthTable extends InferenceAlgorithm[Sentence,Sentence]{
  
 override def entails(kb:KnowledgeBase[Sentence],alpha:Sentence) : boolean = {
    
    val symbols = new HashSet[Symbol]();
    symbols ++= kb.asSentence.symbols
    symbols ++= alpha.symbols
    
    val model = new Model()
    checkAll(kb,alpha,symbols.toList,model)
    
    false
  } 
  
  def checkAll(kb:KnowledgeBase[Sentence],alpha:Sentence,symbols:List[Symbol],model:Model):boolean = {
    if(symbols.size == 0){
      //if KB is true in the model .... find out if alpha is true as well ... if not then KB does not entail alpha
      // though if KB is not true .... the sentence KB => alpha does not make much sense  BUT is deemed to be true
      if(model.isTrue(kb.asSentence)) return model.isTrue(alpha) else return true
    }
    return checkAll(kb,alpha,symbols.tail,extend(symbols.head,True(),model)) &&
      checkAll(kb,alpha,symbols.tail,extend(symbols.head,False(),model)) 
  }

  def extend(symbol:Symbol,v:TruthValue,model:Model):Model = {
    model + (symbol,v)
  }
  
}
