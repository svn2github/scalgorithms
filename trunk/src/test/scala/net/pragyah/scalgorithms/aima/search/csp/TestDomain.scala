package net.pragyah.scalgorithms.aima.search.csp

import junit.framework.TestCase
import junit.framework.Assert._

class TestDomain extends TestCase{
  
  def testDomain = {

    val variables = "V1"::"V2"::"V3"::Nil
    var domain = Domain[String,String](variables)

    assert(domain("V1").size == 0)
    var d = domain + ("V1","v1")
    assert(d != domain)
    domain = d
    assert(domain("V1").size == 1)
    assertTrue(domain("V1").contains("v1"))
    assertTrue(domain.hasEmpty)
    
    d = domain - ("V1","v1")
    assert(d != domain)
    domain = d
    assert(domain("V1").size == 0)
   
    d = domain + ("V1","v11")
    assert(d != domain)
    domain = d
    d = domain + ("V1","v12")
    assert(d != domain)
    domain = d
    d = domain + ("V1","v13")
    assert(d != domain)
    domain = d
    assert(domain("V1").size == 3)
    assert(domain("V1").contains("v11"))
    assert(domain("V1").contains("v12"))
    assert(domain("V1").contains("v13"))
    
    domain = domain + ("V2","v21")
    domain = domain + ("V2","v22")
    domain = domain + ("V2","v23")
    assert(domain("V2").size == 3)

    domain = domain + ("V3","v31")
    domain = domain + ("V3","v32")
    domain = domain + ("V3","v33")
    assert(domain("V3").size == 3)

    assertFalse(domain.hasEmpty)

    domain = domain - ("V3","v31")
    domain = domain - ("V3","v32")
    domain = domain - ("V3","v33")
    assert(domain("V1").size == 3)
    assert(domain("V2").size == 3)
    assert(domain("V3").size == 0)
    assertTrue(domain.hasEmpty)

  }

}
