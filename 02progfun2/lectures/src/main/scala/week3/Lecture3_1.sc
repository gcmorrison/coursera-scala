object Lecture3_1 {
  val account = new BankAccount
  // Fields with state
  var x: String = "abc"

  x = "hi"
  count += 1
  count
  var count = 111

  // Stream example
  def cons[T](hd: T, tl: => Stream[T]) = new Stream[T] {
    override def head = hd

    private var tlOpt: Option[Stream[T]] = None

    def tail: T = tlOpt match {
      case Some(x) => x
      case None => tlOpt = Some(tl); tail
    }
  }

  account deposit 10
  account withdraw 4
  account withdraw 6
  account withdraw 1

  // Class with state
  class BankAccount {
    private var balance = 0

    def deposit(amount: Int): Unit = {
      if (amount > 0) balance += amount
    }

    def withdraw(amount: Int): Int = {
      if (0 < amount && amount <= balance) {
        balance -= amount
        balance
      } else throw new Error("Insufficient funds")
    }
  }

}
