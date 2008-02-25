package net.pragyah.scalgorithms.dynamicprogramming

import junit.framework.TestCase
import junit.framework.Assert._

class TestLongestCommonSubsequence extends TestCase{

  def testLCS_CLRS = {
    val seq2 = "BDCABA";
	val seq1 = "ABCBDAB";
 
    val subSeq = new LongestCommonSubsequence().findSubsequence(seq1,seq2)
    assert(subSeq.contains("BCBA")); subSeq -= "BCBA";
    assert(subSeq.contains("BDAB")); subSeq -= "BDAB";
    assert(subSeq.contains("BCAB")); subSeq -= "BCAB";
    assert(subSeq.size == 0)                                     

  } 

  def testLCS_2 = {
    val seq2 = "TCGAGCCTGAC";
	val seq1 = "ACTGATGAT";
 
    val subSeq = new LongestCommonSubsequence().findSubsequence(seq1,seq2)
    assert(subSeq.contains("CGATGA")); subSeq -= "CGATGA";
    assert(subSeq.contains("TGATGA")); subSeq -= "TGATGA";
    assert(subSeq.size == 0)                                     
  } 

  def testLCS_3 = {
    val seq2 = "TCGAGCCTGACACGTACG";
	val seq1 = "ACTGATGATGACTAGCTATGCTA";
 

    val subSeq = new LongestCommonSubsequence().findSubsequence(seq1,seq2)
    assert(subSeq.contains("CGAGTGACACGTA")); subSeq -= "CGAGTGACACGTA";
    assert(subSeq.contains("CGAGTGACACTAC")); subSeq -= "CGAGTGACACTAC";
    assert(subSeq.contains("CGAGTGACACTAG")); subSeq -= "CGAGTGACACTAG";
    assert(subSeq.contains("CGAGTGACAGTAC")); subSeq -= "CGAGTGACAGTAC";
    assert(subSeq.contains("CGAGTGACAGTAG")); subSeq -= "CGAGTGACAGTAG";
    assert(subSeq.contains("TGAGTGACACGTA")); subSeq -= "TGAGTGACACGTA";
    assert(subSeq.contains("TGAGTGACACTAC")); subSeq -= "TGAGTGACACTAC";
    assert(subSeq.contains("TGAGTGACACTAG")); subSeq -= "TGAGTGACACTAG";
    assert(subSeq.contains("TGAGTGACAGTAC")); subSeq -= "TGAGTGACAGTAC";
    assert(subSeq.contains("TGAGTGACAGTAG")); subSeq -= "TGAGTGACAGTAG";
    assert(subSeq.size == 0)                                     
  } 
  
  
}
