case class Clock(h: Int, min: Int) {
  def toMinutes: Int = h * 60 + min

  def -(that: Clock): Clock = Clock(this.toMinutes - that.toMinutes)

  def +(that: Clock): Clock = Clock(this.toMinutes + that.toMinutes)


}

object Clock {
  def apply(h: Int, min: Int): Clock = Clock(h * 60 + min)

  def apply(min: Int): Clock = if (min >= 0) new Clock(min / 60 % 24, min % 60) else Clock(1440 + (min % -1440))
}