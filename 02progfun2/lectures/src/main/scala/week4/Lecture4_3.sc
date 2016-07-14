import scala.util.DynamicVariable

object Lecture4_3 {

  val a = new BankAccount
  val b = new BankAccount
  val c = consolidated(List(a, b))
  val xchange = Signal(246.00)
  val inDollar = Signal(c() * xchange())

  def consolidated(accts: List[BankAccount]): Signal[Int] =
    Signal(accts.map(_.balance()).sum)

  // Functional Reactive Programming (FRP) Pattern example implementation
  class Signal[T](expr: => T) {

    import Signal._

    private var myExpr: () => T = _
    private var myValue: T = _
    private var observers: Set[Signal[_]] = Set()
    update(expr)

    def apply(): T = {
      observers += caller.value
      assert(!caller.value.observers.contains(this), "cyclic signal definition")
      myValue
    }

    protected def update(expr: => T): Unit = {
      myExpr = () => expr
      computeValue()
    }

    protected def computeValue(): Unit = {
      val newValue = caller.withValue(this)(myExpr())
      if (myValue != newValue) {
        myValue = newValue
        val obs = observers
        observers = Set()
        obs.foreach(_.computeValue())
      }
    }
  }

  class StackableVariable[T](init: T) {
    private var values: List[T] = List(init)

    def value: T = values.head

    def withValue[R](newValue: T)(op: => R): R = {
      values = newValue :: values
      try op finally values = values.tail
    }
  }

  class Var[T](expr: => T) extends Signal[T](expr) {
    override def update(expr: => T): Unit = super.update(expr)
  }

  // Code that uses the above FRP
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

  object Signal {
    // This is not thread-safe
    //    private val caller = new StackableVariable[Signal[_]](NoSignal)

    // Thread-local alternative: Each thread has its own thread-safe instance of caller
    private val caller = new DynamicVariable[Signal[_]](NoSignal)

    def apply[T](expr: => T) = new Signal(expr)
  }

  c()

  a deposit 20
  c()
  b deposit 30
  c()

  object NoSignal extends Signal[Nothing](???) {
    override def computeValue() = ()
  }

  object Var {
    def apply[T](expr: => T) = new Var(expr)
  }

  inDollar()

  b withdraw 10
  inDollar()
}