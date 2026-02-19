object MatchingBrackets {
  def isPaired(str: String): Boolean = {
    val chars = str.filter("()[]{}".contains(_)).toList
    val pairs = Map(')' -> '(', ']' -> '[', '}' -> '{')
    val stack = chars.foldLeft(List[Char]()) { (stack, c) =>
      c match {
        case c if "({[".contains(c) => c :: stack
        case c if ")}]".contains(c) => stack match {
          case head :: tail if head == pairs(c) => stack.tail
          case _ => return false
        }
      }
    }
    stack.isEmpty
  }
}