package net.pragyah.scalgorithms.graphs.search

object Main {
  def main(args : Array[String]) : Unit = {
    
    var graph1 = bfsGraphCLRS_Chap22
    var tree = new BFS().bftree(graph1)
    println(" BFS " )
    println(tree)

    val graph2 =  dfsGraphCLRS_chap22
    var trees = new DFS().dftree(graph2)
    println(" DFS " )
    trees.foreach(println)
    
    //Topological sorting
    type tagClass = Tuple4[String,int,int,Option[Vertex[String]]]
    val graph3 =  topologicalSortingGraphCLRS_Chap22
    new DFS().dftree(graph3)
    val topSort:List[Vertex[String]] = graph3.vertices.sort( (v1,v2) => {
                                          v1.tag.asInstanceOf[tagClass]._3 > v2.tag.asInstanceOf[tagClass]._3 
                                        });
    println(" TOPOLOGICAL SORTING " )
    topSort.foreach((v:Vertex[String]) => print(v.data+" -> "))
    println()
    println()
    println()
    println()


    // Strongly Connected Components 
    
    val graph4 =  sccGraphCLRS_Chap22
    new DFS().dftree(graph4)
    val graph4T = graph4.transpose(true)
    graph4T.vertices.foreach(v => {
                              val vtag = v.tag.asInstanceOf[tagClass]
                              v.tag = ("WHITE",vtag._2,vtag._3,None)
                             })

    val trees3 = new DFS().dftree(graph4T,(v1:Vertex[char],v2:Vertex[char])=> v1.tag.asInstanceOf[tagClass]._3 > v2.tag.asInstanceOf[tagClass]._3 )
    //each tree represents a strongly connected component
    println(" STRONGLY CONNECTED COMPONENTS " )
    trees3.foreach(println)
    
 
  }
  
 def bfsGraphCLRS_Chap22(): Graph[char] = {
    //graph definition from CLRS chapter 22 page 533
    val vr =  Vertex[char]('r');
    val vs =  Vertex[char]('s');
    val vt =  Vertex[char]('t');
    val vu =  Vertex[char]('u');
    val vv =  Vertex[char]('v');
    val vw =  Vertex[char]('w');
    val vx =  Vertex[char]('x');
    val vy =  Vertex[char]('y');
    
    val vertices:List[Vertex[char]] = vr :: vs :: vt :: vu :: vv :: vw :: vx :: vy  :: Nil 
    val graph:Graph[char] = Graph(vertices,false)
    
    graph.addEdge(vr,vs)
    graph.addEdge(vr,vv)
    graph.addEdge(vs,vw)
    graph.addEdge(vt,vw)
    graph.addEdge(vt,vx)
    graph.addEdge(vt,vu)
    graph.addEdge(vu,vx)
    graph.addEdge(vu,vy)
    graph.addEdge(vw,vx)
    graph.addEdge(vx,vy)
    graph

  }


  def dfsGraphCLRS_chap22(): Graph[char] = {
    val vu = Vertex[char]('u');
    val vv = Vertex[char]('v');
    val vw = Vertex[char]('w');
    val vx = Vertex[char]('x');
    val vy = Vertex[char]('y');
    val vz = Vertex[char]('z');

	val vertices:List[Vertex[char]] = vu :: vv :: vw :: vx :: vy :: vz :: Nil 
 
    val graph:Graph[char] = Graph[char](vertices,true) 
    graph.addEdge(vu,vv)
    graph.addEdge(vu,vx)
    graph.addEdge(vx,vv)
    graph.addEdge(vv,vy)
    graph.addEdge(vy,vx)
    graph.addEdge(vw,vy)
    graph.addEdge(vw,vz)
    graph.addEdge(vz,vz)
    
    graph
    
  }

  def topologicalSortingGraphCLRS_Chap22(): Graph[String] = {
    val undershorts = Vertex[String]("undershorts");
    val pants = Vertex[String]("pants");
    val belt = Vertex[String]("belt");
    val shirt = Vertex[String]("shirt");
    val tie = Vertex[String]("tie");
    val jacket = Vertex[String]("jacket");
    val socks = Vertex[String]("socks");
    val shoes = Vertex[String]("shoes");
    val watch = Vertex[String]("watch");


    
	val vertices:List[Vertex[String]] = undershorts::pants::belt::shirt::tie::jacket::socks::shoes::watch::Nil 
 
    val graph:Graph[String] = Graph[String](vertices,true) 
    graph.addEdge(undershorts,pants)
    graph.addEdge(socks,shoes)
    graph.addEdge(undershorts,shoes)
    graph.addEdge(pants,shoes)
    graph.addEdge(pants,belt)
    graph.addEdge(shirt,belt)
    graph.addEdge(shirt,tie)
    graph.addEdge(belt,jacket)
    graph.addEdge(tie,jacket)
    
    graph
    
  }

  
  def sccGraphCLRS_Chap22 : Graph[char] = {
    
    val va =  Vertex[char]('a');
    val vb =  Vertex[char]('b');
    val vc =  Vertex[char]('c');
    val vd =  Vertex[char]('d');
    val ve =  Vertex[char]('e');
    val vf =  Vertex[char]('f');
    val vg =  Vertex[char]('g');
    val vh =  Vertex[char]('h');
    
    val vertices:List[Vertex[char]] = va :: vb :: vc :: vd :: ve :: vf :: vg :: vh  :: Nil 
    val graph:Graph[char] = Graph(vertices,true)
    
    graph.addEdge(va,vb)
    graph.addEdge(vb,vc)
    graph.addEdge(vb,ve)
    graph.addEdge(vb,vf)
    graph.addEdge(vc,vd)
    graph.addEdge(vc,vg)
    graph.addEdge(vd,vc)
    graph.addEdge(vd,vh)
    graph.addEdge(ve,va)
    graph.addEdge(ve,vf)
    graph.addEdge(vf,vg)
    graph.addEdge(vg,vf)
    graph.addEdge(vg,vh)
    graph.addEdge(vh,vh)
    graph

  
  }

  
   def bfsGraph(): Graph[int] = {
    val v1 = Vertex[int](1);
    val v2 =  Vertex[int](2);
    val v3 =  Vertex[int](3);
    val v4 =  Vertex[int](4);
    val v5 =  Vertex[int](5);
    val v6 =  Vertex[int](6);
    val v7 =  Vertex[int](7);
    val v8 =  Vertex[int](8);
    
    val vertices:List[Vertex[int]] = v1 :: v2 :: v3 :: v4 :: v5 :: v6 :: v7 :: v8 :: Nil 
    val graph:Graph[int] = Graph(vertices,false)
    
    graph.addEdge(v1,v2)
    graph.addEdge(v2,v3)
    graph.addEdge(v1,v4)
    graph.addEdge(v4,v5)
    graph.addEdge(v4,v7)
    graph.addEdge(v5,v7)
    graph.addEdge(v5,v6)
    graph.addEdge(v7,v6)
    graph.addEdge(v6,v8)
    graph.addEdge(v7,v8)
    graph

  }
  
 
}
