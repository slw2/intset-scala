// A class of objects to represent a set

// I will not use a dummy header node.

// I will avoid storing repetitions in my linked list, so that the length of the list can
// be found more easily, and also so that printing the list is easier.

// I will not store the elements in increasing order.


class IntSet{
  // State: S : P(Int) (where "P" represents power set)

  // The following lines just define some aliases, so we can subsequently
  // write "Node" rather than "IntSet.Node".
  private type Node = IntSet.Node
  // Constructor
  private def Node(datum: Int, next: Node) = new IntSet.Node(datum, next)

  // Init: S = {}
  private var theSet : Node = null // or however empty set is represented
  
  /** Find an element e in the list.
    * post: return the node containing e or, if e is not in the list, return null */
  def find(e: Int): Node ={
    var n = theSet
    while(n != null && n.datum != e) n = n.next
    n
  }
  
  /** Convert the set to a string. */
  override def toString : String ={
    var string = ""
    var n = theSet
    if (n == null) return "{}"
    else{
      string = "{"
      while(n.next != null){
        string = string + n.datum + ", "
        n = n.next
      }
    }  
    string = string + n.datum + "}"
    string
  }

  /** Add element e to the set
    * Post: S = S_0 U {e} */
  def add(e: Int) ={
    if (find(e) == null){
      val a = Node(e, theSet)
      theSet = a
    }
  }

  /** Length of the list
    * Post: S = S_0 && returns #S */
  def size : Int ={
    var count = 0
    var n = theSet
    while (n != null){
      count += 1
      n = n.next
    }
    count  
  }  

  /** Does the set contain e?
    * Post: S = S_0 && returns (e in S) */
  def contains(e: Int) : Boolean ={
    find(e) != null
  }  

  /** Return any member of the set.  (This is comparable to the operation
    * "head" on scala.collection.mutable.Set, but we'll use a name that does
    * not suggest a particular order.)
    * Pre: S != {}
    * Post: S = S_0 && returns e s.t. e in S */
  def any : Int ={
    assert(theSet != null)
    theSet.datum
  }  

  /** Does this equal that?
    * Post: S = S_0 && returns that.S = S */
  override def equals(that: Any) : Boolean = that match {
    case s: IntSet => {
      var n = theSet
      var k = s.theSet
      while (n != null && s.find(n.datum) != null) n = n.next
      while (k != null && find(k.datum) != null) k = k.next
      n == null && k == null
    }  
    case _ => false
  }

  /** Remove e from the set; result says whether e was in the set initially
    * Post S = S_0 - {e} && returns (e in S_0) */
  def remove(e: Int) : Boolean ={
    var n = find(e)
    if (n != null){
      if (n == theSet) {theSet = theSet.next; true}
      else{
        var prev = theSet
        while (prev.next != n) prev = prev.next
        prev.next = prev.next.next
        true
      }
    }  
    else false    
  }  
    
  /** Test whether this is a subset of that.
    * Post S = S_0 && returns S subset-of that.S */
  def subsetOf(that: IntSet) : Boolean ={
    var n = that.theSet
    while (n != null && find(n.datum) != null) n = n.next
    n == null
  }  

  // ----- optional parts below here -----

  /** return union of this and that.  
    * Post: S = S_0 && returns res s.t. res.S = this U that.S */
  def union(that: IntSet) : IntSet ={
    var n = theSet
    var k = that.theSet
    var UnionOf = new IntSet
    var unionOf = UnionOf.Node(n.datum, null)
    while (n.next != null){
      n = n.next
      if (UnionOf.find(n.datum) == null){
        val n1 = Node(n.datum, unionOf)
        unionOf = n1
      }
    }  
    while (k != null){
      if (UnionOf.find(k.datum) == null){
        val k1 = Node(k.datum, unionOf)
        unionOf = k1  
      k = k.next
      }
    }  
    UnionOf
  }  

  /** return intersection of this and that.  
    * Post: S = S_0 && returns res s.t. res.S = this intersect that.S */
  def intersect(that: IntSet) : IntSet ={
    var n = theSet
    var k = that
    var Intersection = new IntSet
    var intersection = Node(0, null)
    while (n != null){
      if (that.find(n.datum) != null){
        if (intersection == null) intersection = Node(n.datum, null)
        else{
          val n1 = Node(n.datum, intersection)
          intersection = n1
          n = n.next
        }
      }  
    }
    Intersection
  }      

  /** map
    * Post: S = S_0 && returns res s.t. res.S = {f(x) | x <- S} */
  def map(f: Int => Int) : IntSet ={
    var n = theSet
    var Mapping = new IntSet
    Mapping.add(f(n.datum))
    while (n.next != null){
      n = n.next
      Mapping.add(f(n.datum))
    }
    Mapping
  }  

  /** filter
    * Post: S = S_0 && returns res s.t. res.S = {x | x <- S && p(x)} */
  def filter(p : Int => Boolean) : IntSet ={
    var n = theSet
    var Filtered = new IntSet
    while (n != null){
      if ((p(n.datum))) Filtered.add(n.datum)   
      n = n.next
    }
    Filtered    
  }
  
}


// The companion object
object IntSet{
  /** The type of nodes defined in the linked list */
  private class Node(var datum: Int, var next: Node)

  /** Factory method for sets.
    * This will allow us to write, for example, IntSet(3,5,1) to
    * create a new set containing 3, 5, 1 -- once we have defined 
    * the main constructor and the add operation. 
    * post: returns res s.t. res.S = {x1, x2,...,xn}
    *       where xs = [x1, x2,...,xn ] */
  def apply(xs: Int*) : IntSet = {
    val s = new IntSet; for(x <- xs) s.add(x); s
  }
}
