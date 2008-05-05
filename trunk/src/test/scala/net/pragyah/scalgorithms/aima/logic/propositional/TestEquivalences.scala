package net.pragyah.scalgorithms.aima.logic.propositional

import net.pragyah.scalgorithms.aima.logic.propositional.Operator.{A,V,->,<->}
import junit.framework.TestCase
import junit.framework.Assert._

class TestEquivalences extends TestCase{
  
  
  def testDistributivityOfAoverVSymbols = {
    
    val A = Symbol("A")
    val B = Symbol("B")
    val C = Symbol("C")
    
    val orS = BinarySentence[Symbol,Symbol](V,B,C)
    val andS = BinarySentence[Symbol,Sentence](Operator.A,A,orS)
    
    val sentence = Equivalences.applyDistributivityAoverV(andS)
    assert(sentence.op == V)
    
    var symbols = A::A::B::C::Nil
    sentence.sentences.foreach(sentence => {
                                 assert(sentence.op == Operator.A)
                                 assert(sentence.symbols.size == 2)
                                 assert(sentence.symbols.contains(A))
                                 sentence.symbols.foreach(s => symbols = symbols.remove(_ == s))
                               })    
    assert(symbols.size == 0)
  }

  def testDistributivityOfAoverVComplex = {
     
    val A = Symbol("A")
    val B = Symbol("B")
    val C = Symbol("C")
    val D = Symbol("D")
    
    val orAB = BinarySentence[Symbol,Symbol](V,A,B)
    val orCD = BinarySentence[Symbol,Symbol](V,C,D)
    val andS = BinarySentence[Sentence,Sentence](Operator.A,orAB,orCD)
    
    val sentence = Equivalences.applyDistributivityAoverV(andS)
    sentence.sentences.foreach(sentence => {
                                 assert(sentence.op == Operator.A)
                                 sentence.sentences.foreach(_.symbols.contains(A))
                                 
                               })    
    assert(sentence.op == V)
  }
  
  def testDistributivityOfAoverVMultiple = {
     
    val A = Symbol("A")
    val B = Symbol("B")
    val C = Symbol("C")
    val D = Symbol("D")
    
    val orBCD = MultiSentence[Symbol](V,B::C::D::Nil)
    val andS = BinarySentence[Sentence,Sentence](Operator.A,A,orBCD)
    
    val sentence = Equivalences.applyDistributivityAoverV(andS)
    assert(sentence.op == V)
    sentence.sentences.foreach(sentence => {
                                 assert(sentence.op == Operator.A)
                                 assert(sentence.sentences.contains(A))
                                 
                               })    
    assert(sentence.sentences.size == 3)
  }
    
    
  def testDistributivityOfVoverASymbols = {
     
    val A = Symbol("A")
    val B = Symbol("B")
    val C = Symbol("C")
    
    val andS = BinarySentence[Symbol,Symbol](Operator.A,B,C)
    val orS = BinarySentence[Symbol,Sentence](V,A,andS)
    
    val sentence = Equivalences.applyDistributivityVoverA(orS)
    sentence.sentences.foreach(sentence => {
                                 assert(sentence.op == V)
                                 assert(sentence.sentences.contains(A))
                                 
                               })    
    assert(sentence.op == Operator.A)
  }

  def testDistributivityOfVoverAComplex = {
     
    val A = Symbol("A")
    val B = Symbol("B")
    val C = Symbol("C")
    val D = Symbol("D")
    
    val andAB = BinarySentence[Symbol,Symbol](Operator.A,A,B)
    val andCD = BinarySentence[Symbol,Symbol](Operator.A,C,D)
    val orS = BinarySentence[Sentence,Sentence](V,andAB,andCD)
    
    val sentence = Equivalences.applyDistributivityVoverA(orS)
    sentence.sentences.foreach(sentence => {
                                 assert(sentence.op == V)
                                 sentence.sentences.foreach(_.symbols.contains(A))
                                 
                               })    
    assert(sentence.op == Operator.A)
  }
  
  def testDistributivityOfVoverAMultiple = {
     
    val A = Symbol("A")
    val B = Symbol("B")
    val C = Symbol("C")
    val D = Symbol("D")
    
    val andBCD = MultiSentence[Symbol](Operator.A,B::C::D::Nil)
    val orS = BinarySentence[Sentence,Sentence](V,A,andBCD)
    
    val sentence = Equivalences.applyDistributivityVoverA(orS)
    assert(sentence.op == Operator.A)
    sentence.sentences.foreach(sentence => {
                                 assert(sentence.op == V)
                                 assert(sentence.sentences.contains(A))
                                 
                               })    
    assert(sentence.sentences.size == 3)
  }

  
  def testDeMorganAndSymbols = {

     
    val A = Symbol("A")
    val B = Symbol("B")
    val C = Symbol("C")

    val andS:ComplexSentence[Sentence] = MultiSentence[Sentence](Operator.A,A::B::C::Nil)
    //val nandS = !andS  //not sure why this one is not working .. it cribs saying that ![Sentence] found instead of ![Complex[Sentence]]
    val nandS = new !(andS)
    val orS = Equivalences.applyDeMorgan(nandS)
    
    assert(orS.sentences.size == 3)
    assert(orS.sentences.filter(s => !s.isInstanceOf[![Symbol]]).size == 0)
    assert(orS.op == V)
    assert(orS.sentences.contains(!C))
    assert(orS.sentences.contains(!A))
    assert(orS.sentences.contains(!B))
  }

  def testDeMorganOrSymbols = {

     
    val A = Symbol("A")
    val B = Symbol("B")
    val C = Symbol("C")
    

    val orS:ComplexSentence[Sentence] = MultiSentence[Sentence](V,A::B::C::Nil)
    //val nandS = !andS  //not sure why this one is not working .. it cribs saying that ![Sentence] found instead of ![Complex[Sentence]]
    val norS = new !(orS)
    val andS = Equivalences.applyDeMorgan(norS)
    
    assert(andS.sentences.size == 3)
    assert(andS.sentences.filter(s => !s.isInstanceOf[![Symbol]]).size == 0)
    assert(andS.op == Operator.A)
    assert(andS.sentences.contains(!C))
    assert(andS.sentences.contains(!A))
    assert(andS.sentences.contains(!B))
  }
 
  def testDeMorganAndComplexSentences = {



     
    val A = Symbol("A")
    val B = Symbol("B")
    val C = Symbol("C")
    
    val b1 = BinarySentence[Symbol,Symbol](V,A,B)
    val b2 = BinarySentence[Symbol,Symbol](V,B,C)
    val b3 = BinarySentence[Symbol,Symbol](V,C,A)

    val orS:ComplexSentence[Sentence] = MultiSentence[Sentence](Operator.A,b1::b2::b3::Nil)
    //val nandS = !andS  //not sure why this one is not working .. it cribs saying that ![Sentence] found instead of ![Complex[Sentence]]
    val norS = new !(orS)
    val andS = Equivalences.applyDeMorgan(norS)
    
    assert(andS.sentences.size == 3)
    assert(andS.sentences.filter(s => !s.isInstanceOf[![Symbol]]).size == 0)
    assert(andS.op == V)
    assert(andS.sentences.contains(!b1))
    assert(andS.sentences.contains(!b2))
    assert(andS.sentences.contains(!b3))
  }

  def testDeMorganOrComplexSentences = {


     
    val A = Symbol("A")
    val B = Symbol("B")
    val C = Symbol("C")
    
    val b1 = BinarySentence[Symbol,Symbol](Operator.A,A,B)
    val b2 = BinarySentence[Symbol,Symbol](Operator.A,B,C)
    val b3 = BinarySentence[Symbol,Symbol](Operator.A,C,A)

    val orS:ComplexSentence[Sentence] = MultiSentence[Sentence](V,b1::b2::b3::Nil)
    //val nandS = !andS  //not sure why this one is not working .. it cribs saying that ![Sentence] found instead of ![Complex[Sentence]]
    val norS = new !(orS)
    val andS = Equivalences.applyDeMorgan(norS)
    
    assert(andS.sentences.size == 3)
    assert(andS.sentences.filter(s => !s.isInstanceOf[![Symbol]]).size == 0)
    assert(andS.op == Operator.A)
    assert(andS.sentences.contains(!b1))
    assert(andS.sentences.contains(!b2))
    assert(andS.sentences.contains(!b3))
   }
  
  
  def testBiconditionalEliminationSymbols = {
 
    val A = Symbol("A")
    val B = Symbol("B")

    val biCondition = BinarySentence[Sentence,Sentence](Operator.<->,A,B)
    
    val biEl = Equivalences.applyBiconditionalElimination(biCondition) 
    assert(biEl.op == Operator.A)
    biEl.sentences.foreach(s => {

                             assert(s.isInstanceOf[ComplexSentence[Sentence]])
                             assert(s.asInstanceOf[ComplexSentence[Sentence]].op == Operator.->)
                             assert(s.asInstanceOf[ComplexSentence[Sentence]].sentences.size == 2)
                             assert(s.asInstanceOf[ComplexSentence[Sentence]].sentences.contains(A))
                             assert(s.asInstanceOf[ComplexSentence[Sentence]].sentences.contains(B))
                           })
    
    
    
                                                         
                                            
    
  }
   
  def testBiconditionalEliminationSentences = {
 
    val A = Symbol("A")
    val B = Symbol("B")
    val C = Symbol("C")
    val D = Symbol("D")

    val ab = BinarySentence[Sentence,Sentence](Operator.V,A,B)
    val cd = BinarySentence[Sentence,Sentence](Operator.V,C,D)
    
    val biCondition = BinarySentence[Sentence,Sentence](Operator.<->,ab,cd)
    
    val biEl = Equivalences.applyBiconditionalElimination(biCondition) 
    assert(biEl.op == Operator.A)
    biEl.sentences.foreach(s => {
                             assert(s.isInstanceOf[ComplexSentence[Sentence]])
                             assert(s.asInstanceOf[ComplexSentence[Sentence]].op == Operator.->)
                             assert(s.asInstanceOf[ComplexSentence[Sentence]].sentences.size == 2)
                             assert(s.asInstanceOf[ComplexSentence[Sentence]].sentences.contains(ab))
                             assert(s.asInstanceOf[ComplexSentence[Sentence]].sentences.contains(cd))
                           })
    
  }
  
  
  def testImplicationEliminationSymbols = {
 
    val A = Symbol("A")
    val B = Symbol("B")

    val implication = BinarySentence[Sentence,Sentence](->,A,B)
    val result = Equivalences.applyImplicationElimination(implication)
    assert(result.op == V)
    assert(result.left == !(A))
    assert(result.right == B)
  }
  
  def testImplicationEliminationSentences = {
 
    val A = Symbol("A")
    val B = Symbol("B")
    val C = Symbol("C")
    val D = Symbol("D")

    val ab = BinarySentence[Sentence,Sentence](Operator.V,A,B)
    val cd = BinarySentence[Sentence,Sentence](Operator.V,C,D)
  
    val implication = BinarySentence[Sentence,Sentence](->,ab,cd)
    val result = Equivalences.applyImplicationElimination(implication)
    assert(result.op == V)
    assert(result.left == !(ab))
    assert(result.right == cd)
  }
  
  
  def testContrapositionSymbols = {
 
    val A = Symbol("A")
    val B = Symbol("B")

    val implication = BinarySentence[Sentence,Sentence](->,A,B)
    val result = Equivalences.applyContraposition(implication)
   
    assert(result.op == ->)
    assert(result.left == !B)
    assert(result.right == !A)
  }
  
  def testContrapositionSentences = {
 
    val A = Symbol("A")
    val B = Symbol("B")
    val C = Symbol("C")
    val D = Symbol("D")

    val ab = BinarySentence[Sentence,Sentence](Operator.V,A,B)
    val cd = BinarySentence[Sentence,Sentence](Operator.V,C,D)

    val implication = BinarySentence[Sentence,Sentence](->,ab,cd)
    val result = Equivalences.applyContraposition(implication)
   
    assert(result.op == ->)
    assert(result.left == !cd)
    assert(result.right == !ab)
  }
 
  def testDoubleNegationEliminationSymbol = {
     val A = Symbol("A")
    val result = Equivalences.applyDoubleNegationElimination(!(!(A)))
    assume(result == A)
  }

  def testDoubleNegationEliminationSentence = {
     val A = Symbol("A")
    val B = Symbol("B")
    val C = Symbol("C")
    val sentence = BinarySentence[Sentence,Sentence](->,BinarySentence[Symbol,Symbol](Operator.A,A,B),!C)
    
    try{
      val result = Equivalences.applyDoubleNegationElimination(!(sentence))
      fail("Should have thron an exception as it's not a double negation sentence");
    }catch{
    case ex:Exception => 
      assert(true)
    }
    
    
    val result = Equivalences.applyDoubleNegationElimination(!(!(sentence)))
    assume(result == sentence)
  }
  
}