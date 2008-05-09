package net.pragyah.scalgorithms.aima.logic.propositional

import scala.collection.mutable.Set

trait Sentence  {
  
  
/*
 * Sentence ->  AtomicSentence | ComplexSentence
 * AtomicSentence -> True | False | Symbol
 * Symbol  -> P | Q | R | . . . 
 * ComplexSentence -> !Sentence
 * 					| ( Sentence &&  Sentence )
 * 					| ( Sentence || Sentence )
 * 					| ( Sentence  => Sentence )
 * 					| ( Sentence  <=> Sentence ) 
*/
  
  def symbols:List[Symbol]
  def unary_! = {
    if(this.isInstanceOf[![Sentence]])
      this.asInstanceOf[![Sentence]].sentence
    else
        new !(this)
  }
  
}

trait TruthValue{
  def value:boolean
}

trait AtomicSentence extends Sentence{}

trait ComplexSentence[A <: Sentence] extends Sentence{
  def op:Operator
  def sentences:List[A]
  
  def flatten:Sentence = {
    var newSentences:List[A]= Nil
    if(op == Operator.-> || op == Operator.<-> ){
      return this
    }
    
    sentences.foreach(sentence =>
      if(sentence.isInstanceOf[ComplexSentence[Sentence]] && sentence.asInstanceOf[ComplexSentence[A]].op == op){
        //TODO add recursive flattening here
        newSentences = newSentences ::: sentence.asInstanceOf[ComplexSentence[A]].sentences
      }else{
        newSentences = newSentences ::: List(sentence)
      })
    
    newSentences = newSentences.removeDuplicates
    
    if(op == Operator.A){
      newSentences.foreach(s => if(newSentences.contains(!s)) return False())
    }

    if(op == Operator.V){
      newSentences.foreach(s => if(newSentences.contains(!s)) return True())
    }
    
    
    if(newSentences.size > sentences.size)	return MultiSentence[A](op,newSentences)
    else return this
  }
}

//EmptyClause
object EmptyClause{
  private  val e = new EmptyClause()
  def apply() =  e
 
}
class EmptyClause extends AtomicSentence with TruthValue{
  final def value = false;
  override final def symbols = Nil;
  override def toString = "Empty"
}

//True
object True{
  private val t = new True()
  def apply() =  t
}
class True extends AtomicSentence with TruthValue{ 
  final def value = true;
  override final def symbols = Nil;
  override def toString = "True"
}
//False
object False{
  val f = new False()
  def apply() =  f
}
class False  extends AtomicSentence with TruthValue{ 
  final def value = false
  override final def symbols = Nil
  override def toString = "False"
}

//Symbol
object Symbol{
  def apply(name:String) =  new Symbol(name)
}
class Symbol(val name:String) extends AtomicSentence{
  override def symbols = this::Nil
  override def toString = name
  override def equals(that:Any) = that.isInstanceOf[Symbol] && that.asInstanceOf[Symbol].name == name
}


//Binary Sentence
object BinarySentence{
  def apply[L <: Sentence,R <: Sentence](opr:Operator,left:L,right:R) =  new BinarySentence(opr,left,right)
}
class BinarySentence[L <: Sentence,R <: Sentence](val op:Operator,val left:L, val right:R) extends ComplexSentence[Sentence]{
  //override def op = opr
  override def sentences = left::right::Nil
  override def symbols = {
    var set = Set[Symbol]()
    set ++ left.symbols
    set ++ right.symbols
    set.toList
  } 
  
  override def toString = {
    var leftStr = left.toString
    var rightStr = right.toString
    if(!left.isInstanceOf[Symbol] && !left.isInstanceOf[![Sentence]])
      leftStr = "("+leftStr+")"
    if(!right.isInstanceOf[Symbol] && !right.isInstanceOf[![Sentence]])
      rightStr = "("+rightStr+")"
    
    leftStr+" "+op+" "+rightStr
  }
  
  override def equals(other:Any):boolean ={
    if(!other.isInstanceOf[BinarySentence[L,R]]) return false
    val that = other.asInstanceOf[BinarySentence[L,R]]
    if(this.op != that.op) return false
    op match{
		case Operator.-> => return this.left.equals(that.left) && this.right.equals(that.right) 
		case Operator.<-> => return this.left.equals(that.left) && this.right.equals(that.right)
		case Operator.A => return (this.left.equals(that.left) && this.right.equals(that.right)) ||(this.left.equals(that.right) && this.right.equals(that.left)) 
		case Operator.V => return (this.left.equals(that.left) && this.right.equals(that.right)) ||(this.left.equals(that.right) && this.right.equals(that.left)) 
		case _ => false
    }
    
  }
  
}

//Multi Sentence
object MultiSentence{
  
  def apply[A <: Sentence](op:Operator,_sentences:List[A]) =  new MultiSentence(op)(_sentences)
}
class MultiSentence[A <: Sentence](val op:Operator)(val sentences:List[A]) extends ComplexSentence[A]{
  
  assume(op != Operator.-> && op != Operator.<->)
  
  override def symbols = {
    var set = Set[Symbol]()
    sentences.foldLeft[Set[Symbol]](set)((set,sentence) => set ++ sentence.symbols)
    set.toList
  }
  
  override def equals(other:Any):Boolean = {
    if(!other.isInstanceOf[MultiSentence[A]]) return false
    val that = other.asInstanceOf[MultiSentence[A]]
    if(this.op != that.op) return false
    if(this.sentences.size != this.sentences.size) return false 
    if(this.sentences.diff((that.sentences)).size == 0 && that.sentences.diff((this.sentences)).size == 0) return true
    return false
  }
  
  
  override def toString = {
    var firstStr = sentences.head.toString
    if(sentences.head.isInstanceOf[ComplexSentence[Sentence]] && !sentences.head.isInstanceOf[![Sentence]] )
      firstStr = "("+firstStr+")"
      
    sentences.tail.foldLeft[String](firstStr)((str,sentence) => {
	    var sentenceStr = sentence.toString
	    if(!sentence.isInstanceOf[AtomicSentence] && !sentence.isInstanceOf[![Sentence]]) sentenceStr = "("+sentenceStr+")"
            str + " "+op.toString+" "+sentenceStr
      
      
    })
  }
}

// Negation
object !{
  def apply(sentence:Sentence) = new !(sentence)
}
class ![A <: Sentence](val sentence:A) extends ComplexSentence[A]{
  def op = Operator.NOT
  def symbols = sentence.symbols
  def sentences = sentence::Nil
  override def equals(other:Any) = {
    val b1 = other.isInstanceOf[![A]] 
    var b2 = false
    if(b1) b2 = other.asInstanceOf[![A]].sentence.equals(sentence)
    b2
  }
  override def toString = {
    var sentenceStr = sentence.toString
    if(!sentence.isInstanceOf[AtomicSentence]) sentenceStr = "("+sentenceStr+")"
   "!"+sentenceStr+"" 
  }
}

//Horn Clause
object HornClause{
  def apply(body:List[Symbol],head:Symbol) = new HornClause(body,head)
}

class HornClause(val body:List[Symbol],val head:Symbol) extends BinarySentence(Operator.->,MultiSentence[Symbol](Operator.A,body),head){
  def premiseSymbols = body
  override def toString = super.toString
}


//CNF
object CNF{
  def apply(clauses:List[MultiSentence[Symbol]]) = new CNF(clauses)  
}
class CNF(val clauses:List[MultiSentence[Symbol]]) extends MultiSentence[MultiSentence[Symbol]](Operator.A)(clauses){
  clauses.foreach(clause => assume(clause.op == Operator.V))
}


// OPERATOR

object Operator{
  final val A   = new Operator("&")
  final val V   = new Operator("||")
  final val NOT   = new Operator("!"){
    
  }
  final val -> = new Operator("->")
  final val <-> = new Operator("<->")
}

class Operator  (val rep:String){ 
  override def toString = rep
}
