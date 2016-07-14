object Lecture4_2 {

  val a = new BankAccount
  val b = new BankAccount
  val c = consolidated(List(a, b))
  val xchange = Signal(246.00)
  val inDollar = Signal(c() * xchange())
  c()

  a deposit 20
  c()
  b deposit 30
  c()

  def consolidated(accts: List[BankAccount]): Signal[Int] =
    Signal(accts.map(_.balance()).sum)

  // Broken example of the Functional Reactive Programming (FRP) Pattern
  class BankAccount {
    val balance = Var(0)

    def currentBalance() = balance

    def deposit(amount: Int): Unit = {
      if (amount > 0) {
        val bal = balance()
        balance() = bal + amount
      }
    }

    def withdraw(amount: Int): Unit = {
      if (0 < amount && amount <= balance()) {
        val bal = balance()
        balance() = bal - amount
      } else throw new Error("Insufficient funds")
    }
  }

  inDollar()

  b withdraw 10
  inDollar()
}