package net.pragyah.scalgorithms.trees


object Node{
  def apply[A](data:A) = new Node(data)
}

class Node[A](val data:A){
  
   var degree:int = 0
   var parent:Option[Node[A]] = None
   var children:List[Node[A]] = List()
   var tag:Any = null
   
  
  def setLeftChild(l:Node[A]) = children = l::children 
  def setRightChild(r:Node[A]) = children = children ::: List(r)
  def setLeftChildren(l:List[Node[A]]) = children = l ::: children 
  def setRightChildren(r:List[Node[A]]) = children = children ::: r 
  def addChild(c:Node[A]) = setLeftChild(c)
  def getChild(c:A) = {
   val list = children.filter(_.data == c)
   if(list.isEmpty)
     null
   else
       list.head
  }
  
  def traversePreOrder[B](z: B)(f: (B, A) => B): B = {
    val b = f(z,data)
    children.foreach(_.traversePreOrder(b)(f))
    b
  }
  
  def setParent(p:Node[A]) = parent = Some(p)
  
  override def toString() : String = {
    var str = data.toString()
    
    children.foreach( c => str = str.concat("\n\t").concat(c.data.toString))
    str
  }
  
  def toTreeString(indent:String,indentLevel:int) : String = {
    var indentStr = indent
    for(i <- 1 to indentLevel) indentStr = indentStr.concat(indent)
    
    var str = indentStr.concat(data.toString)
    children.foreach(child => str = str.concat("\n").concat(child.toTreeString(indent,indentLevel+1)))
    str
  }
  
  
}

object Tree{
  def apply[A](root:Node[A]) = new Tree(root)
  def apply[A](root:A) = new Tree(Node(root))
}

class Tree[A](var root:Node[A]){
  
  override def toString() : String = {
    root.toTreeString("\t",0)
  }
  
  
}