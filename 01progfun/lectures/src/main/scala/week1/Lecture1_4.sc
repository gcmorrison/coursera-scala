object Lecture1_4 {
  def and(x: Boolean, y: => Boolean) = if (x) y else false

  def or(x: Boolean, y: Boolean) = if (x) true else y

  and(false, false)
  and(false, true)
  and(true, false)
  and(true, true)

  or(false, false)
  or(false, true)
  or(true, false)
  or(true, true)

  def loop: Boolean = loop

  and(false, loop)
}