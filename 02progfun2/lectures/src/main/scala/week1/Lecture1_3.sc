object Lecture1_3 {

  // First attempt
  trait Generator1[+T] {
    def generate: T
  }

  val integers1 = new Generator1[Int] {
    val rand = new java.util.Random()

    def generate = rand.nextInt()
  }
  val booleans1 = new Generator1[Boolean] {
    def generate = integers1.generate > 0
  }
  val pairs1 = new Generator1[(Int, Int)] {
    def generate: (Int, Int) = (integers1.generate, integers1.generate)
  }

  // Second attempt
  //  val booleans2 = for (x <- integers1) yield x > 0
  //
  //  def pairs2[T, U](t: Generator1[T], u: Generator1[U]) = for {
  //    x <- t
  //    y <- u
  //  } yield (x, y)

  // Higher-order function attempt
  trait Generator[+T] {
    // an alias of "this"
    self =>

    def generate: T

    def map[S](f: T => S): Generator[S] = new Generator[S] {
      def generate = f(self.generate)
    }

    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      def generate = f(self.generate).generate
    }
  }

  val integers = new Generator[Int] {
    val rand = new java.util.Random()

    def generate = rand.nextInt()
  }

  val booleans = new Generator[Boolean] {
    def generate = integers.generate > 0
  }

  def pairs[T, U](t: Generator[T], u: Generator[U]) = new Generator1[(T, U)] {
    def generate = (t.generate, u.generate)
  }

  def single[T](x: T): Generator[T] = new Generator[T] {
    def generate = x
  }

  def choose(lo: Int, hi: Int): Generator[Int] =
    for (x <- integers) yield lo + x % (hi - lo)

  def oneOf[T](xs: T*): Generator[T] =
    for (idx <- choose(0, xs.length)) yield xs(idx)

  def lists: Generator[List[Int]] = for {
    isEmpty <- booleans
    list <- if (isEmpty) emptyLists else nonEmptyLists
  } yield list

  def emptyLists = single(Nil)

  def nonEmptyLists = for {
    head <- integers
    tail <- lists
  } yield head :: tail

  // Example Tree Generator
  trait Tree

  case class Inner(left: Tree, right: Tree) extends Tree

  case class Leaf(x: Int) extends Tree

  def leafs: Generator[Leaf] = for {
    x <- integers
  } yield Leaf(x)

  def inners: Generator[Inner] = for {
    left <- trees
    right <- trees
  } yield Inner(left, right)

  def trees: Generator[Tree] = for {
    isLeaf <- booleans
    tree <- if (isLeaf) leafs else inners
  } yield tree

  trees.generate
}
