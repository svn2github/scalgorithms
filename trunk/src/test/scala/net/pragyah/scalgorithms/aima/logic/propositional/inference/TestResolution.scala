package net.pragyah.scalgorithms.aima.logic.propositional.inference

import junit.framework.TestCase
import junit.framework.Assert._

import scala.collection.mutable.Set

import net.pragyah.scalgorithms.aima.logic.propositional.MultiSentence
import net.pragyah.scalgorithms.aima.logic.propositional.Operator.{A,V,<->}
import net.pragyah.scalgorithms.aima.logic.propositional.!

class TestResolution extends TestCase{

   def testResolveNonEmpty = {
    //Example from AIMA .. figure 7.13
    val P12 = Symbol("P12")
    val P21 = Symbol("P21")
    val B11 = Symbol("B11")

    val resolution = new Resolution();

    val Ci = MultiSentence(V,!P21::B11::Nil)
    val Cj = MultiSentence(V,!B11::P12::P21::Nil)
    
    //resolution.resolve
    
    var resolvents = resolution.resolve(Ci,Cj)
    
    assert(resolvents.size == 2)
    assert(resolvents.filter(_.symbols.contains(B11)).toList.size == 1)
    assert(resolvents.filter(_.symbols.contains(P21)).toList.size == 1)
        resolvents.foreach(println)

    val r1 = resolvents.filter(_.symbols.contains(B11)).toList.head
    val r2 = resolvents.filter(_.symbols.contains(P21)).toList.head
    assert(r1  != r2)
    
    assert(r1.isInstanceOf[MultiSentence[AtomicSentence]])
    assert(r2.isInstanceOf[MultiSentence[AtomicSentence]])
    
    var r1Sentences =r1.asInstanceOf[MultiSentence[AtomicSentence]].sentences; 
    assert(r1Sentences.contains(B11))
    assert(r1Sentences.contains(P12))
    assert(r1Sentences.contains(!B11))
    
    var r2Sentences =r2.asInstanceOf[MultiSentence[AtomicSentence]].sentences; 
    assert(r2Sentences.contains(P12))
    assert(r2Sentences.contains(P21))
    assert(r2Sentences.contains(!P21))

     
    
  }

  def testResolveNonEmpty2 = {
    //Example from AIMA .. figure 7.13
    val P12 = Symbol("P12")
    val B11 = Symbol("B11")

    val resolution = new Resolution();

    val Ci = MultiSentence(V,!P12::B11::Nil)
    val Cj = !B11
    
    //resolution.resolve
    
    var resolvents = resolution.resolve(Ci,Cj)
    

    assert(resolvents.size == 1)
    assert(resolvents.toList.head == True())
  }

  def testResolveEmpty = {
    val P12 = Symbol("P12")
    val  resolution = new Resolution();

    val Ci = P12
    val Cj =  !P12
	val resolvents = resolution.resolve(Ci,Cj)

    assert(resolvents.size == 1)
    assert(resolvents.toList.head == EmptyClause())
 
  }
  
  def testPairs =  {
    
    val resolution = new Resolution();
    
    val l = 1::2::3::4::5::6::Nil
    var expected = Set[(int,int)]()
    expected += ((1,2))
    expected += ((1,3))
    expected += ((1,4))
    expected += ((1,5))
    expected += ((1,6))
    
    expected += ((2,3))
    expected += ((2,4))
    expected += ((2,5))
    expected += ((2,6))
    
    expected += ((3,4))
    expected += ((3,5))
    expected += ((3,6))
    
    expected += ((4,5))
    expected += ((4,6))
    
    expected += ((5,6))

    val set = resolution.pairs[int](l)

    assertEquals(set.size,expected.size)
    assert(expected.subsetOf(set))
    assert(set.subsetOf(expected))
  }
  
  
  def testAlgorithm = {
    
    val B11 = Symbol("B11")
    val P12 = Symbol("P12")
    val P21 = Symbol("P21")
    
    val orSentence  = BinarySentence[Symbol,Symbol](V,P12,P21)
    val biConditionalSentence = BinarySentence[Symbol,Sentence](<->,B11,orSentence)
    val  kb = KnowledgeBase[Sentence]()
    kb.tell(biConditionalSentence)
    kb.tell(!B11)
              
    
    val resolution = new Resolution();
    assert(resolution.entails(kb,P12))       
                       
  }
  
  

}
