case class Triangle(s1: Double, s2: Double, s3: Double) {

  def equilateral: Boolean = s1 == s2 && s2 == s3 && checkLegalTriangle

  def isosceles: Boolean = (s1 == s2 || s2 == s3 || s1 == s3) && checkLegalTriangle

  def scalene: Boolean = s1 != s2 && s2 != s3 && s1 != s3 && checkLegalTriangle

  def checkLegalTriangle: Boolean = (s1 + s2 + s3 > 0) && (s1 + s2 >= s3) && (s2 + s3 >= s1) && (s1 + s3 >= s2)
}