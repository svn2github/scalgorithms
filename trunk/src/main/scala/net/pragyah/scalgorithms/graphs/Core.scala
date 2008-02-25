package net.pragyah.scalgorithms.graphs

object Vertex{
  
   def apply[A](data:A) : Vertex[A] = {
    return new Vertex[A](data)
  }
}

class Vertex[A](val data:A){
  private[graphs] var edges:List[Edge[A]] = List()
  var tag:Any = None
  
  def addEdge(edge:Edge[A]) : Unit = {
    edges = edges ::: List(edge)
  }
  
  def getEdges() : List[Edge[A]] = {
    edges
  }
  
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
  
  directed match{
  case true => v1.addEdge(this)
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

class Graph[A](var vertices:List[Vertex[A]],val directed:boolean) {
  
  var edges:List[Edge[A]] = List()
  
  def addVertex(v:Vertex[A]) = {
    vertices = v::vertices
  }

  def addVertex(a:A) = {
    vertices = new Vertex[A](a)::vertices
  }

  
  def addEdge(from:Vertex[A],to:Vertex[A],weight:double){
    assume(vertices.exists(_ ==from) && vertices.exists(_ == to))
    val edge = new Edge(weight,from,to,directed,false)
    edges = edge::edges
  }
  
  def addEdge(from:Vertex[A],to:Vertex[A]){
    addEdge(from,to,0)
  }
  
  def ::(from_to:(Vertex[A],Vertex[A])) {
    addEdge(from_to._1,from_to._2)
  }
  
  def ::(from_to_wt:(Vertex[A],Vertex[A],double)) {
    addEdge(from_to_wt._1,from_to_wt._2,from_to_wt._3)
  }
  
  def getEdge(from:Vertex[A],to:Vertex[A]): Option[Edge[A]] = {
    edges.find(e => ((e.v1 == from && e.v2 == to) ||(!directed && e.v1 == to && e.v2 == from) ))
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