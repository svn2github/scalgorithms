package net.pragyah.scalgorithms.disjointsets

import junit.framework.TestCase
import junit.framework.Assert._

class TestTreeBasedDisjointSet extends TestCase{
  
  def testStringSets = {
    val ds = new TreeBasedDisjointSet[String]()
    
    ds.makeSet("VP3")
    ds.makeSet("BP")
    ds.makeSet("DP")
    ds.makeSet("VP2")
    ds.makeSet("VP1")
    
    assert(ds.findSet("VP3") == "VP3")
    assert(ds.findSet("BP") == "BP")
    assert(ds.findSet("DP") == "DP")
    assert(ds.findSet("VP2") == "VP2")
    assert(ds.findSet("VP1") == "VP1")

    ds.union("VP3","BP")
    ds.union("BP","DP")
    ds.union("DP","VP2")
    ds.union("VP1","VP3")

    assert(ds.findSet("VP3") == ds.findSet("VP3"))
    assert(ds.findSet("BP") == ds.findSet("VP3"))
    assert(ds.findSet("DP") == ds.findSet("VP3"))
    assert(ds.findSet("VP2") == ds.findSet("VP3"))
    assert(ds.findSet("VP1") == ds.findSet("VP3"))

    ds.makeSet("PCP")
    ds.makeSet("PP")
    ds.makeSet("SP")
    ds.makeSet("MP")
    assert(ds.findSet("PCP") == "PCP")
    assert(ds.findSet("PP") == "PP")
    assert(ds.findSet("SP") == "SP")
    assert(ds.findSet("MP") == "MP")
    
    ds.union("PP","PCP")
    ds.union("PCP","SP")
    ds.union("SP","MP")

    assert(ds.findSet("PP") == ds.findSet("PP"))
    assert(ds.findSet("SP") == ds.findSet("PP"))
    assert(ds.findSet("MP") == ds.findSet("PP"))
    assert(ds.findSet("SP") == ds.findSet("PP"))

    ds.union("VP3","PP")
    
    assert(ds.findSet("VP3") == ds.findSet("PP"))
    
    assert(ds.findSet("VP3") == ds.findSet("VP3"))
    assert(ds.findSet("BP") == ds.findSet("VP3"))
    assert(ds.findSet("DP") == ds.findSet("VP3"))
    assert(ds.findSet("VP2") == ds.findSet("VP3"))
    assert(ds.findSet("VP1") == ds.findSet("VP3"))
    
    assert(ds.findSet("PP") == ds.findSet("PP"))
    assert(ds.findSet("SP") == ds.findSet("PP"))
    assert(ds.findSet("MP") == ds.findSet("PP"))
    assert(ds.findSet("SP") == ds.findSet("PP"))
  }

}
