object Lecture1_4_Fixed {
  def startThread(a: Account, b: Account, n: Int) = {
    val t = new Thread {
      override def run(): Unit = {
        for (i <- 0 until n) {
          a.transfer(b, 1)
        }
      }
    }

    t.start()
    t
  }

  private var uidCount = 0L
  private val x = new AnyRef {}
  def getUniqueId(): Long = x.synchronized {
    uidCount = uidCount + 1
    uidCount
  }

  val a1 = new Account(500000)
  val a2 = new Account(700000)

  val t = startThread(a1, a2, 150000)
  val s = startThread(a2, a1, 150000)
  t.join()
  s.join()
}

class Account(private var amount: Int = 0) {
  val uid = Lecture1_4_Fixed.getUniqueId()

  private def lockAndTransfer(target: Account, n: Int) =
    this.synchronized {
      target.synchronized {
        this.amount -= n
        target.amount += n
      }
    }

  def transfer(target: Account, n: Int) = {
    if (this.uid < target.uid) this.lockAndTransfer(target, n)
    else target.lockAndTransfer(this, -n)
  }
}
