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
}

trait AtomicSentence extends Sentence{}
trait TruthValue{
  def value:boolean
}

trait ComplexSentence extends Sentence{
  def op:Operator
  def sentences:List[Sentence] 
}

//EmptyClause
object EmptyClause{
  private val e = new EmptyClause()
  def apply() =  e
 
}
class EmptyClause extends AtomicSentence with TruthValue{
  final def value = false;
  override final def symbols = Nil;
  override def toString = "True"
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
// Negation
object !{
  def apply(s:Sentence) = new !(s)
}
class !(val s:Sentence) extends AtomicSentence{
  def symbols = s.symbols
}

//Symbol
object Symbol{
  def apply(name:String) =  new Symbol(name)
}
class Symbol(val name:String) extends AtomicSentence{
  override def symbols = this::Nil
  override def toString = name
}


//Binary Sentence
object BinarySentence{
  def apply[L <: Sentence,R <: Sentence](opr:Operator,left:L,right:R) =  new BinarySentence(opr,left,right)
}
class BinarySentence[L <: Sentence,R <: Sentence](val op:Operator,val left:L, val right:R) extends ComplexSentence{
  //override def op = opr
  override def sentences = left::right::Nil
  override def symbols = {
    var set = Set[Symbol]()
    set ++ left.symbols
    set ++ right.symbols
    set.toList
  } 
  
  override def toString = left+" "+op+" "+right
}

//Multi Sentence
object MultiSentence{
  def apply[A <: Sentence](op:Operator,_sentences:List[A]) =  new MultiSentence(op,_sentences)
}
class MultiSentence[A <: Sentence](val op:Operator,val sentences:List[A]) extends ComplexSentence{
  override def symbols = {
    var set = Set[Symbol]()
    sentences.foldLeft[Set[Symbol]](set)((set,sentence) => set ++ sentence.symbols)
    set.toList
  }
  
  override def toString = {
    sentences.tail.foldLeft[String](sentences.head.toString)((str,sentence) => str + " "+op.toString+" "+sentence)
  }
}

//Horn Clause
object HornClause{
  def apply(body:List[Symbol],head:Symbol) = new HornClause(body,head)
}

class HornClause(val body:List[Symbol],val head:Symbol) extends BinarySentence(Operator.==>,MultiSentence[Symbol](Operator.A,body),head){
  def premiseSymbols = body
  override def toString = super.toString
}


// OPERATOR

object Operator{
  final val A   = new Operator("&")
  final val V   = new Operator("||")
  final val !   = new Operator("!"){
    
  }
  final val ==> = new Operator("=>")
  final val <=> = new Operator("<=>")
}

class Operator  (val rep:String){ 
  override def toString = rep
}
