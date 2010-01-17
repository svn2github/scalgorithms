package net.pragyah.scalgorithms.graphs

object Vertex{
  
   def apply[A](data:A) : Vertex[A] = {
    return new Vertex[A](data)
  }
}

class Vertex[A](val data:A){
  
  private[graphs] var edges:List[Edge[A]] = List()
  private[graphs] var _inEdges:List[Edge[A]] = List()
  var tag:Any = None
  
  def addEdge(edge:Edge[A]) : Unit = {
    edges = edges ::: List(edge)
  }
  
  def removeEdge(edge:Edge[A]) : Unit = {
    edges = edges.remove(_ == edge)
  }
  
  def getEdges() : List[Edge[A]] = {
    edges
  }
  
  //TODO add test cases for INEDGES 
  def inEdges = _inEdges
  def addInboundEdge(edge:Edge[A]) = _inEdges = _inEdges ::: List(edge)

  
  def adjacent() : List[Vertex[A]] = {
    edges.flatMap(e => e.vertices.filter(_ != this))
  }
  
  override def toString() : String = {
    var str = data.toString.concat(" -> ");
/*    var str = data.toString.concat(" "+tag+" -> ");
*/    
    edges.foreach(e => {(e.v1,e.v2) match{
                  case (a,b) => if(a == this) str = str.concat(b.data.toString+' ') else str = str.concat(a.data.toString+' ')
                  }
                  str = str.concat("("+e.weight+") ")
                  }
    )
    str
  }
}

object Edge{
   def apply[A](weight:double, v1:Vertex[A], v2:Vertex[A],directed:boolean) : Edge[A] = {
    return new Edge[A](weight,v1,v2,directed,false)
  }
}

class Edge[A](var weight:double, val v1:Vertex[A],val v2:Vertex[A],val directed:boolean,var highlighted:boolean){
  
  var tag:Any = None

  
  directed match{
    
  case true => v1.addEdge(this);v2.addInboundEdge(this) // add only to the parent node in case of a directed graph  //TODO add test case for inbound edge 
  case false => v1.addEdge(this);v2.addEdge(this) 
  }
  
  def vertices() : List[Vertex[A]] = {
    v1::v2::Nil
  }
  
  def getOther(v:Vertex[A]) : Vertex[A] = {	
    v match{
    case vertex if(vertex == v1) => v2
    case vertex if(vertex == v2) => v1
    case _ => null
    }
  }
  
  override def equals(other:Any) = other match{
  case that:Edge[A] => that.weight == this.weight && that.v1 == this.v1 && that.v2 == this.v2 && this.directed == that.directed
  case _ => false
  }
  
  override def toString() : String = {
    var str = v1.data.toString
    directed match{
      case true => str = str.concat(" -> "+v2.data)
      case false => str = str.concat(" <-> "+v2.data) 
    }
    str = str.concat(" ("+weight+")")
    str
  }
}

object Graph{
  def apply[A](vertices:List[Vertex[A]],directed:boolean) : Graph[A] = {
    return new Graph[A](vertices,directed)
  }
}

class Graph[A](val v:List[Vertex[A]],val directed:boolean) {

  
  val valVertexMap = scala.collection.mutable.Map[A,Vertex[A]]() //TODO .. this should not be exposed to the outer world
  
  var vertices = v
  
  def _vertices = vertices
  
  var edges:List[Edge[A]] = List()
//  def edges:List[Edge[A]] = edgeMap.values.toList
  
  var edgeMap = Map[Tuple2[Vertex[A],Vertex[A]],Edge[A]](); 
  
  def ++(v:Vertex[A]) = {
    addVertex(v)
  }
  def ++(list:List[Vertex[A]]) = { list.foreach(addVertex(_))}
  
  def addVertex(v:Vertex[A]) = {
    assume(valVertexMap.get(v.data) == None)
    valVertexMap(v.data) = v 
    vertices = v::vertices
    
  }

  def ++(a:A) = {
    addVertex(a)
  }

  
  def addVertex(a:A) = {
    vertices = new Vertex[A](a)::vertices
  }

  
  def addEdge(from:Vertex[A],to:Vertex[A],weight:double):Edge[A] = {
    assume(vertices.exists(_ ==from) && vertices.exists(_ == to))
    val edge = new Edge[A](weight,from,to,directed,false)
    edges = edge::edges
/*    edgeMap = edgeMap + ((from,to) -> edge)
    
    if(!directed)
      edgeMap = edgeMap + ((to,from) -> edge)
*/	edge  
  }
  
  def addEdge(from:Vertex[A],to:Vertex[A]):Edge[A] = {
    return addEdge(from,to,0)
  }
  
  def ::(from_to:(Vertex[A],Vertex[A])):Edge[A] = {
    addEdge(from_to._1,from_to._2)
  }
  
  def ::(from_to_wt:(Vertex[A],Vertex[A],double)):Edge[A] = {
    addEdge(from_to_wt._1,from_to_wt._2,from_to_wt._3)
  }
  
  def vertex(data:A) : Option[Vertex[A]] = {
    valVertexMap.get(data)
  }
  
  def getEdge(from:Vertex[A],to:Vertex[A]): Option[Edge[A]] = {
    
    edges.find(e => ((e.v1 == from && e.v2 == to) ||(!directed && e.v1 == to && e.v2 == from) ))
    
/*    val edge = edgeMap.get((from,to))
    
    if(edge == None & directed) return None
    else return edgeMap.get((to,from))
*/    
  }
  //MOST OF THE CODE HERE IS INEFFICIENT... but works for now and that's what matters to me :) 
  def removeEdge(from:Vertex[A],to:Vertex[A]){
    val edgeOpt = edges.find(e => e.v1 == from && e.v2 == to)
    if(edgeOpt == None) return;
    val edge = edgeOpt.get
    edge.vertices.foreach(_.removeEdge(edge))
    edges = edges.remove(_ == edge)
  }
  
  def removeEdge(fromTo:Tuple2[Vertex[A],Vertex[A]]){
    removeEdge(fromTo._1,fromTo._2)
  }
  

  
  def transpose(withTag:boolean) : Graph[A] = {
    assume(directed,"Graph is not directed")
    val xVertices:List[Vertex[A]] = vertices.map( v => {
      v.edges = List() 
      if(!withTag) v.tag = None
      v
    });
    val xGraph = Graph[A](xVertices,directed)

    xGraph.edges = edges.map[Edge[A]](e => {Edge[A](e.weight,e.v2,e.v1,e.directed)})
    xGraph
  }
  
  def clearTags = vertices.foreach(_.tag = null)
  
  override def toString():String = {
    var str:String = "Graph ("+vertices.length+","+edges.length+") = \n"
    vertices.foreach(v => str = str.concat("\t"+v.toString).concat("\n"))
    str
  }
}