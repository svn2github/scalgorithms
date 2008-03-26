package net.pragyah.scalgorithms.aima.search.csp

import junit.framework.TestCase
import junit.framework.Assert._

class TestDomain extends TestCase{
  
  def testDomain = {

    val variables = "V1"::"V2"::"V3"::Nil
    val domain = Domain[String,String](variables)

    assert(domain("V1").size == 0)
    domain += ("V1","v1")
    assert(domain("V1").size == 1)
    assertTrue(domain("V1").contains("v1"))
    assertTrue(domain.hasEmpty)
    
    domain -= ("V1","v1")
    assert(domain("V1").size == 0)
   
    domain += ("V1","v11")
    domain += ("V1","v12")
    domain += ("V1","v13")
    assert(domain("V1").size == 3)
    assert(domain("V1").contains("v11"))
    assert(domain("V1").contains("v12"))
    assert(domain("V1").contains("v13"))
    
    domain += ("V2","v21")
    domain += ("V2","v22")
    domain += ("V2","v23")
    assert(domain("V2").size == 3)

    domain += ("V3","v31")
    domain += ("V3","v32")
    domain += ("V3","v33")
    assert(domain("V3").size == 3)

    assertFalse(domain.hasEmpty)

    domain -= ("V3","v31")
    domain -= ("V3","v32")
    domain -= ("V3","v33")
    assert(domain("V1").size == 3)
    assert(domain("V2").size == 3)
    assert(domain("V3").size == 0)
    assertTrue(domain.hasEmpty)

  }

}
