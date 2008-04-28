package net.pragyah.scalgorithms.aima.logic.propositional

import net.pragyah.scalgorithms.aima.logic.propositional.Operator.{A,V,!,==>,<=>}

import junit.framework.TestCase
import junit.framework.Assert._

import scala.collection.mutable.HashSet

class TestCore extends TestCase{
  
  def testTrue = {
    val sTrue = True();
    assert(sTrue.value)
    assert(sTrue.symbols == Nil)
  }

  def testFalse = {
    val sFalse = False();
    assertFalse(sFalse.value)
    assert(sFalse.symbols == Nil)
  }
  
  def testSymbol = {
    val symbol = Symbol("P")
    assert(symbol.name == "P")
    assert(symbol.symbols.head == symbol)
  }

  def testBinary = {
    
    val l = Symbol("L")
    val r = Symbol("R")

    val sentence = BinarySentence(V,l,r)
    assert(sentence.op == V)
    assert(sentence.sentences.length == 2)
    assert(sentence.sentences.contains(l))
    assert(sentence.sentences.contains(r))
    assert(sentence.left == l)
    assert(sentence.right == r)

    assert(sentence.symbols.length == 2)
    assert(sentence.symbols.contains(l))
    assert(sentence.symbols.contains(r))
    
  }

  def testMultipleWithSymbols = {
    val P = Symbol("P")
    val Q = Symbol("Q")
    val R = Symbol("R")
    val T = True()

    val sentence = MultiSentence(A,P::Q::R::T::Nil)
    assert(sentence.op == A)
    assert(sentence.sentences.length == 4 )
    assert(sentence.sentences.contains(P))
    assert(sentence.sentences.contains(Q))
    assert(sentence.sentences.contains(R))
    assert(sentence.sentences.contains(T))

	assert(sentence.symbols.length == 3)
    assert(sentence.symbols.contains(P))
    assert(sentence.symbols.contains(Q))
    assert(sentence.symbols.contains(R))
    
    
    

  }

  def testMultipleWithSentences = {
    val P = Symbol("P")
    val Q = Symbol("Q")
    val R = Symbol("R")
    val S = Symbol("S")
    val T = True()
    
    val sentence1 = BinarySentence(V,P,Q)
    val sentence2 = BinarySentence(V,R,S)
    
    
    val sentence = MultiSentence(A,sentence1::sentence2::T::Nil)
    assert(sentence.op == A)
    assert(sentence.sentences.length == 3 )
    assert(sentence.sentences.contains(sentence1))
    assert(sentence.sentences.contains(sentence2))
    assert(sentence.sentences.contains(T))
    
    val symbols = sentence.symbols
    assert(symbols.size == 4)
    assert(symbols.contains(P))
    assert(symbols.contains(Q))
    assert(symbols.contains(R))
    assert(symbols.contains(S))
    
  }
  
  def testMultipleWithMultipleSentences = {
    val P = Symbol("P")
    val Q = Symbol("Q")
    val R = Symbol("R")
    val S = Symbol("S")
    val T = True()
    
    val bSentence = BinarySentence(A,P,Q)
    
    val sentence1 = MultiSentence(V,bSentence::R::Nil)
    val sentence2 = MultiSentence(V,Q::R::S::Nil)
    val sentence3 = MultiSentence(V,R::S::T::Nil)
    
    
    val sentence = MultiSentence(A,sentence1::sentence2::sentence3::P::Q::Nil)
    assert(sentence.op == A)
    assert(sentence.sentences.length == 5 )
    assert(sentence.sentences.contains(sentence1))
    assert(sentence.sentences.contains(sentence2))
    assert(sentence.sentences.contains(sentence3))
    assert(sentence.sentences.contains(P))
    assert(sentence.sentences.contains(Q))
    
    val symbols = sentence.symbols
    assert(symbols.size == 4)
    assert(symbols.contains(P))
    assert(symbols.contains(Q))
    assert(symbols.contains(R))
    assert(symbols.contains(S))
    
  }

}
