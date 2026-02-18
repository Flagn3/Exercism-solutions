object SecretHandshake {
  def commands(x: Int): List[String] = {

    if (x >= 16) commands(x - 16).reverse
    else if (x >= 8) commands(x - 8) :+ "jump"
    else if (x >= 4) commands(x - 4) :+ "close your eyes"
    else if (x >= 2) commands(x - 2) :+ "double blink"
    else if (x >= 1) commands(x - 1) :+ "wink"
    else Nil
  }
}