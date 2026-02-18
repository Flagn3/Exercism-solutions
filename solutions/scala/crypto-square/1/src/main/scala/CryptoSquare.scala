import scala.math.*

object CryptoSquare {
  private def normalize(text: String): String = {
    text.toLowerCase().trim().filter(_.isLetterOrDigit)
  }
  private def calculateRectangle(text: String): (Int, Int) = {
    val c = ceil(sqrt(text.length))
    val r = ceil(text.length / c).toInt
    (c.toInt, r)
  }
  def ciphertext(text: String): String = {
    text match {
      case "" => ""
      case _ => {
        val t = normalize(text)
        val (c, r) = calculateRectangle(t)
        t.grouped(c).flatMap(d => d.padTo(c, ' ').zipWithIndex).toList.sortBy(_._2).map(_._1).grouped(r).map(_.mkString).mkString(" ")
      }
    }
  }
}