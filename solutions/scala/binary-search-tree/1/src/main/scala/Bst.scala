case class Bst(value: Int, left: Option[Bst] = None, right: Option[Bst] = None) {
  def insert(n: Int): Bst = {
    if (n <= value) {
      val leftValue = if (left.isEmpty) Bst(n) else left.get.insert(n)
      Bst(value, Some(leftValue), right)
    }
    else {
      val rightValue = if (right.isEmpty) Bst(n) else right.get.insert(n)
      Bst(value, left, Some(rightValue))
    }
  }

  
}

object Bst{
  def toList(node: Bst): List[Int] = {
  node match {
    case Bst(value, None, None) => List(value)
    case Bst(value, Some(left), Some(right)) => toList(left).concat(List(value)).concat(toList(right))
    case Bst(value, Some(left), None) => toList(left).concat(List(value))
    case Bst(value, None, Some(right)) => List(value).concat(toList(right))
  }
}
  def fromList(l:List[Int]) :Bst = l match {
            case Nil => throw new IllegalArgumentException("At least 1 item in the list is required")
            case x::xs => xs.foldLeft(Bst(x))((agg,x) => agg.insert(x))
    } 
    
  
}

