object Lecture3_3 {

  /*
  REPEAT {
    command
  } (condition)
   */
  def REPEAT(command: => Unit)(condition: => Boolean): Unit = {
    command
    if (condition) ()
    else REPEAT(command)(condition)
  }

  // For-Loops
  // Java syntax: for (int i = 1; i < 3; i = i + 1) { System.out.print(i + " "); }
  for (i <- 1 until 3) {
    System.out.print(i + " ")
  }
}