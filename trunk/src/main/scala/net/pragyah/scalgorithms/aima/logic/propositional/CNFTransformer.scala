package net.pragyah.scalgorithms.aima.logic.propositional

import net.pragyah.scalgorithms.aima.logic.propositional.Operator.{A,V,->,<->}
  
class CNFTransformer(val sentence:Sentence) {
  
  
  def process() = {
    val s = deriveAV(sentence)
    applyVoverA(s)
  }
  
  def applyVoverA(sentence:Sentence):Sentence = {
    if(sentence.isInstanceOf[BinarySentence[Sentence,Sentence]] && sentence.asInstanceOf[BinarySentence[Sentence,Sentence]].op == V){
      return Equivalences.applyDistributivityVoverA(sentence.asInstanceOf[BinarySentence[Sentence,Sentence]])
    }
    else if(sentence.isInstanceOf[ComplexSentence[Sentence]] && sentence.asInstanceOf[ComplexSentence[Sentence]].op == A){
      var newList:List[Sentence] = Nil
      sentence.asInstanceOf[ComplexSentence[Sentence]].sentences.foreach(s => newList = newList:::List(applyVoverA(s)))
      return  MultiSentence(A,newList).flatten
    }
    else{
      return sentence
    }
    
  }
 
  def deriveAV(sentence:Sentence) : Sentence = {
    sentence match {
	    case s:AtomicSentence => return s
		case s : ![Sentence] => {
		  s.sentence match {
		      //if it's a symbol .... just return the negation applying deriveAV .. as that returns the same symbol anyways
		      case ss : Symbol => return !(deriveAV(ss))
		      //if it's a negation sentence .. then apply the double negation elemination .. thus exposing the underlying sentence and passing it through this recursive method
		      case  ss : ![Sentence] => return deriveAV(Equivalences.applyDoubleNegationElimination(s))
		      // now .. if it's a complex sentence with A or V... app deMorgan equivalence to convert the sentence to conjunctive or disjunctive form
		      case ss:ComplexSentence[Sentence] if (ss.op == A || ss.op == V) => {
		        // if the inner sentence is a complex sentence .. then apply demorgan to the outer sentence and the derive equivalent of the resultant 
		        return deriveAV(Equivalences.applyDeMorgan(s.asInstanceOf[![ComplexSentence[Sentence]]]))
		      }
		      //now .. if it's a binarysentence with -> or --> operator ... just get the equivalent of the underlying sentence and run this through the recursive derive Equivalent again  
		      case ss:Any => return deriveAV(!(deriveAV(ss)))
		  }
		}
        case s:MultiSentence[Sentence] =>{
          var sentences = List[Sentence]()
          s.sentences.foreach(ss => sentences = sentences ::: List(deriveAV(ss)))
          return MultiSentence[Sentence](s.op,sentences).flatten
		}
		case s:BinarySentence[Sentence,Sentence] =>{
		  s.op match{
		       //if it's an implication operator .... applyImplication Elimination on the binary sentence and then send it for equivalence again
		      case -> => return deriveAV(Equivalences.applyImplicationElimination(s).flatten)
		       //if it's a bi-conditional operator .... applyBicondition Elimination on the binary sentence and then send it for equivalence again
		      case <-> => return deriveAV(Equivalences.applyBiconditionalElimination(s).flatten)

              case V 
                if((s.left.isInstanceOf[ComplexSentence[Sentence]] && s.left.asInstanceOf[ComplexSentence[Sentence]].op == A)
                   ||(s.right.isInstanceOf[ComplexSentence[Sentence]] && s.right.asInstanceOf[ComplexSentence[Sentence]].op == A))
                => {                  
                  return deriveAV(Equivalences.applyDistributivityVoverA(s).flatten)
                } 
              //for any other operator .... A or V .... derive equivalences of each of the left and right sub-sentences and return back a new BinarySentence
		      case o:Any => return BinarySentence[Sentence,Sentence](o,deriveAV(s.left),deriveAV(s.right)).flatten 
		  }
		   
		} 
    }
    
  }


}