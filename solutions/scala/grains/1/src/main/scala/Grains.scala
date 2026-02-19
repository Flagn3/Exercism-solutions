object Grains {
  def square(n: Int): Option[BigInt] = if (n <= 0 || n > 64) None else Some((1 until n).foldLeft(1: BigInt) { (acc, _) => acc * 2 })

  def total: BigInt = (1 to 64).toList.flatMap(square).sum
}