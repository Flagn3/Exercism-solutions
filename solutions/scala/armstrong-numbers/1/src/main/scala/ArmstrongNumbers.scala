object ArmstrongNumbers {
  def isArmstrongNumber(n: Int): Boolean = {
    val digits = n.toString.map(_.asDigit)
    digits.map(d => Math.pow(d, digits.size).toInt).sum == n
  }
}