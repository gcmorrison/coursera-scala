
object Lecture4_1 {

  // Observer Pattern

  val a = new BankAccount
  val b = new BankAccount
  val c = new Consolidator(List(a, b))

  c.totalBalance

  a.deposit(10)
  c.totalBalance

  b.deposit(40)
  c.totalBalance

  b.withdraw(25)
  c.totalBalance

  trait Publisher {
    private var subscribers: Set[Subscriber] = Set()

    def subscribe(subscriber: Subscriber): Unit = subscribers += subscriber

    def unsubscribe(subscriber: Subscriber): Unit = subscribers -= subscriber

    def publish(): Unit = subscribers.foreach(_.handler(this))
  }

  trait Subscriber {
    def handler(publisher: Publisher)
  }

  // Publisher
  class BankAccount extends Publisher {
    private var balance = 0

    def currentBalance() = balance

    def deposit(amount: Int): Unit = {
      if (amount > 0) balance += amount
      publish()
    }

    def withdraw(amount: Int): Unit = {
      if (0 < amount && amount <= balance) {
        balance -= amount
        publish()
      } else throw new Error("Insufficient funds")
    }
  }

  class Consolidator(observed: List[BankAccount]) extends Subscriber {
    observed.foreach(_.subscribe(this))

    private var total: Int = _ // Variable is initially uninitialized
    compute()

    def handler(pub: Publisher) = compute()

    private def compute() = total = observed.map(_.currentBalance()).sum

    def totalBalance = total
  }

}
