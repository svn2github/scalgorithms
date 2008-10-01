package net.pragyah.scalgorithms.graphs.search

import scala.collection.mutable.HashMap
import scala.Math

import junit.framework.TestCase
import junit.framework.Assert._

class TestAStar extends TestCase{

  def testGraphOCW_Sec2_6 = {
    val (graph,s,goal,h,solution) = aStarSearchGraphOCW_Sec2_6
    val path = new A_Star().search(graph,s,goal,h)
    assert(path == solution)
  }
  
  def testGraphAIMA_Romania_Chap4 = {
    val (graph,s,goal,h,solution) = aStarSearchGraphAIMA_Romania_Chap4
    val path = new A_Star().search(graph,s,goal,h)
    assert(path == solution)
  }
  
  def aStarSearchGraphOCW_Sec2_6 : (Graph[char],Vertex[char],Vertex[char] => boolean,Vertex[char] => double,List[Vertex[char]]) = {
    
	val vs =  Vertex[char]('S');
	val va =  Vertex[char]('A');
	val vb =  Vertex[char]('B');
	val vc =  Vertex[char]('C');
	val vg =  Vertex[char]('G');

	val vertices:List[Vertex[char]] = vs :: va :: vb :: vc :: vg ::Nil
    val graph = new Graph(vertices,true)

    graph.addEdge(vs,va,1)
    graph.addEdge(vs,vb,2)
    graph.addEdge(va,vc,1)
    graph.addEdge(vb,vc,2)
    graph.addEdge(vc,vg,100)

    val heuristics = new HashMap[Vertex[char],double]()
    heuristics += vs -> 90d
    heuristics += va -> 100d
    heuristics += vb -> 88d
    heuristics += vc -> 100d
    heuristics += vg -> 0d
    
    def goal(vu:Vertex[char]):boolean = vu == vg 
    def h(vu:Vertex[char]):double = heuristics(vu)

    val solution = vs :: va :: vc :: vg ::Nil
    
    (graph,vs,goal,h,solution)
  }

  def aStarSearchGraphAIMA_Romania_Chap4 : (Graph[String],Vertex[String],Vertex[String] => boolean,Vertex[String] => double,List[Vertex[String]]) = {
    
    // coordinates
    val map = new HashMap[String,Tuple2[Int,Int]]()
	 map += "Arad" -> (91,492)
	 map += "Bucharest" -> (400,327)
	 map += "Craiova" -> (253,288)
	 map += "Dobreta" -> (165,299)
	 map += "Eforie" -> (562,293)
	 map += "Fagaras" -> (305,449)
	 map += "Giurgiu" -> (375,270)
	 map += "Hirsova" -> (534,350)
	 map += "Iasi" -> (473,506)
	 map += "Lugoj" -> (165,379)
	 map += "Mehadia" -> (168,339)
	 map += "Neamt" -> (406,537)
	 map += "Oradea" -> (131,571)
	 map += "Pitesti" -> (320,368)
	 map += "Rimnicu" -> (233,410)
	 map += "Sibiu" -> (207,457)
	 map += "Timisoara" -> (94,410)
	 map += "Urziceni" -> (456,350)
	 map += "Vaslui" -> (509,444)
	 map += "Zerind" -> (108,531)    
    
	val va =  Vertex[String]("Arad")
	val vb =  Vertex[String]("Bucharest")
	val vc =  Vertex[String]("Craiova")
	val vd =  Vertex[String]("Dobreta") 
	val ve =  Vertex[String]("Eforie") 
	val vf =  Vertex[String]("Fagaras")  
	val vg =  Vertex[String]("Giurgiu") 
	val vh =  Vertex[String]("Hirsova") 
	val vi =  Vertex[String]("Iasi")  
	val vl =  Vertex[String]("Lugoj")    
	val vm =  Vertex[String]("Mehadia")  
	val vn =  Vertex[String]("Neamt") 
	val vo =  Vertex[String]("Oradea")   
	val vp =  Vertex[String]("Pitesti")  
	val vr =  Vertex[String]("Rimnicu")
	val vs =  Vertex[String]("Sibiu") 
	val vt =  Vertex[String]("Timisoara")
	val vu =  Vertex[String]("Urziceni")
	val vv =  Vertex[String]("Vaslui")
	val vz =  Vertex[String]("Zerind")  

  
	val vertices:List[Vertex[String]] = va ::vb ::vc ::vd ::ve ::vf ::vg ::vh ::vi ::vl ::vm ::vn ::vo ::vp ::vr ::vs ::vt ::vu ::vv ::vz ::Nil                     
 
    val graph:Graph[String] = Graph[String](vertices,false) 
    graph.addEdge(va,vz,75)
    graph.addEdge(va,vs,140)
    graph.addEdge(va,vt,118)

    graph.addEdge(vb,vf,211)
    graph.addEdge(vb,vp,101)
    graph.addEdge(vb,vg,90)
    graph.addEdge(vb,vu,85)
    
    graph.addEdge(vc,vd,120)
    graph.addEdge(vc,vr,146)
    graph.addEdge(vc,vp,138)
    
    graph.addEdge(vd,vm,75)

    graph.addEdge(ve,vh,86)

    graph.addEdge(vf,vs,99)

    graph.addEdge(vh,vu,98)
    
    graph.addEdge(vi,vn,87)
    graph.addEdge(vi,vv,92)
    
    graph.addEdge(vl,vt,111)
    graph.addEdge(vl,vm,70)
    
    graph.addEdge(vo,vz,71)
    graph.addEdge(vo,vs,151)
    
    graph.addEdge(vp,vr,97)
    
    graph.addEdge(vr,vs,80)
    
    graph.addEdge(vu,vv,142)
    

    
    def goal(g:Vertex[String]) = g == vb
    def h(u:Vertex[String])(v:Vertex[String]):double = {
      val (uX,uY) = map(u.data)
      val (vX,vY) = map(v.data)
      Math.sqrt(Math.pow(uX - vX,2) +Math.pow(uY - vY,2))
    }
    
    val solution = va :: vs :: vr :: vp :: vb ::Nil
    
    (graph,va,goal,h(va),solution)
  }

}
