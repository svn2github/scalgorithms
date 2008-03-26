package net.pragyah.scalgorithms.aima.search.csp

import junit.framework.TestCase
import junit.framework.Assert._

class TestAssignment extends TestCase{
  
  def testAssignments = {

    val variables = "V1"::"V2"::"V3"::"V4"::"V5"::"V6"::Nil
    val assignment = Assignment[String,String](variables)

    variables.foreach(v => assert(assignment(v) == None))

    assert(!assignment.complete)
    //INCOMPLETE
    
    assignment += ("V1","val1")
    assert(assignment("V1").get == "val1")
    assert(assignment.has("V1"))

    
    assignment -= ("V1")
    assert(!assignment.has("V1"))
    assert(assignment("V1") == None)
    
    assignment += ("V1","val1")
    assignment += ("V2","val2")
    assignment += ("V3","val3")
    assignment += ("V4","val4")
    assignment += ("V5","val5")
    assignment += ("V6","val6")
    assert(assignment("V1").get == "val1")
    assert(assignment("V2").get == "val2")
    assert(assignment("V3").get == "val3")
    assert(assignment("V4").get == "val4")
    assert(assignment("V5").get == "val5")
    assert(assignment("V6").get == "val6")
    assert(assignment.has("V1"))
    assert(assignment.has("V2"))
    assert(assignment.has("V3"))
    assert(assignment.has("V4"))
    assert(assignment.has("V5"))
    assert(assignment.has("V6"))

    assert(assignment.complete)
    
    assignment -= ("V1")
    assert(assignment.unassigned.head == "V1")
    assert(assignment.unassigned.tail == Nil)

    
  }
  

}
