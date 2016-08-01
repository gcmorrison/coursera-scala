object Lecture1_3 {
  class HelloThread extends Thread {
    override def run(): Unit = {
      println("Hello World!")
    }
  }

  val t = new HelloThread
  t.start()
  t.join()

  // ---------------------------------------------------------- //
  class HelloThread2 extends Thread {
    override def run(): Unit = {
      println("Hello")
      println("World!")
    }
  }

  val t2 = new HelloThread2
  val s2 = new HelloThread2
  t2.start()
  s2.start()
  t2.join()
  s2.join()

  // ---------------------------------------------------------- //
  private var uidCount = 0L
  def getUniqueId(): Long = {
    uidCount = uidCount + 1
    uidCount
  }
  def startThread() = {
    val t = new Thread {
      override def run(): Unit = {
        val uids = for (i <- 0 until 10) yield getUniqueId()
        println(uids)
      }
    }
    t.start()
    t
  }
  startThread(); startThread()

  // ---------------------------------------------------------- //
  private var uidCountAtomic = 0L
  private val x = new AnyRef {}
  def getUniqueIdAtomic(): Long = x.synchronized {
    uidCountAtomic = uidCountAtomic + 1
    uidCountAtomic
  }
  def startThreadAtomic() = {
    val t = new Thread {
      override def run(): Unit = {
        val uids = for (i <- 0 until 10) yield getUniqueIdAtomic()
        println(uids)
      }
    }
    t.start()
    t
  }
  startThreadAtomic(); startThreadAtomic()
}