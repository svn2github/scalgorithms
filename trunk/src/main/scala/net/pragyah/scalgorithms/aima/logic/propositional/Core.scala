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
  def apply(opr:Operator,left:Sentence,right:Sentence) =  new BinarySentence(opr,left,right)
}
class BinarySentence(val opr:Operator,val left:Sentence, val right:Sentence) extends ComplexSentence{
  override def op = opr
  override def sentences = left::right::Nil
  override def symbols = {
    var set = Set[Symbol]()
    set ++ left.symbols
    set ++ right.symbols
    set.toList
  } 
  
  override def toString = left+" "+opr+" "+right
}

//Multi Sentence
object MultiSentence{
  def apply(opr:Operator,_sentences:List[Sentence]) =  new MultiSentence(opr,_sentences)
}
class MultiSentence(val opr:Operator,val _sentences:List[Sentence]) extends ComplexSentence{
  override def op = opr
  override def sentences = _sentences
  override def symbols = {
    var set = Set[Symbol]()
    _sentences.foldLeft[Set[Symbol]](set)((set,sentence) => set ++ sentence.symbols)
    set.toList
  }
}


 // OPERATOR

object Operator{
  final val A   = new Operator("A")
  final val V   = new Operator("V")
  final val !   = new Operator("!"){
    
  }
  final val ==> = new Operator("=>")
  final val <=> = new Operator("<=>")
}

class Operator  (val rep:String){ 
  override def toString = rep
}
