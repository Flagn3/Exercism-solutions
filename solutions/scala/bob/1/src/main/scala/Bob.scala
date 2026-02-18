object Bob {
  def response(s: String): String = {

    if (s.trim.isEmpty){
      return "Fine. Be that way!"
    }
    (isQuestion(s), isCapital(s)) match {
      case (true, true) => "Calm down, I know what I'm doing!"
      case (true, _) => "Sure."
      case (_, true) => "Whoa, chill out!"
      case _ => "Whatever."
    }
  }

  def isQuestion(s: String): Boolean = {
    s.trim.endsWith("?")
  }

  def isCapital(s: String): Boolean = {
    s.toUpperCase == s && s.exists(_.isLetter)
  }
}
