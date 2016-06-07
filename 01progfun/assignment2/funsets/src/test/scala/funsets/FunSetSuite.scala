package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s1, 2), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect only contains elements that are in both sets") {
    new TestSets {
      val s4 = singletonSet(3)
      assert(!contains(intersect(s1, s2), 1), "Intersect 1")
      assert(!contains(intersect(s1, s2), 2), "Intersect 2")
      assert(contains(intersect(s3, s4), 3), "Intersect 3")
    }
  }

  test("diff only contains elements of one set that isn't in the other") {
    new TestSets {
      val diffSet = diff(union(singletonSet(1), union(singletonSet(2), singletonSet(3))), s2)

      assert(contains(diffSet, 1), "Diff 1")
      assert(!contains(diffSet, 2), "Diff 2")
      assert(contains(diffSet, 3), "Diff 3")
    }
  }

  test("filter only contains elements of a set that satisfies the predicate") {
    new TestSets {
      val filterSet = filter(union(union(singletonSet(1), union(singletonSet(2), singletonSet(3))), singletonSet(4)), x => x % 2 == 0)

      assert(!contains(filterSet, 1), "Filter 1")
      assert(contains(filterSet, 2), "Filter 2")
      assert(!contains(filterSet, 3), "Filter 3")
      assert(contains(filterSet, 4), "Filter 4")
    }
  }

  test("forall checks that all values in the set satisfies the predicate") {
    new TestSets {
      val filterSet = union(union(singletonSet(1), union(singletonSet(2), singletonSet(3))), singletonSet(4))

      assert(!forall(filterSet, x => x < 2), "ForAll 1")
      assert(forall(filterSet, x => x < 5), "ForAll 2")
      assert(!forall(filterSet, x => x % 2 == 0), "ForAll 3")
      assert(forall(filterSet, x => x % 6 < 5), "ForAll 4")
    }
  }

  test("exists checks whether any values in the set satisfies the predicate") {
    new TestSets {
      val filterSet = union(union(singletonSet(1), union(singletonSet(2), singletonSet(3))), singletonSet(4))

      assert(exists(filterSet, x => x < 2), "Exist 1")
      assert(!exists(filterSet, x => x > 4), "Exist 2")
      assert(exists(filterSet, x => x % 2 == 0), "Exist 3")
      assert(!exists(filterSet, x => x % 6 == 5), "Exist 4")
    }
  }

  test("map creates a new set with a transform function applied to all the values in the given set") {
    new TestSets {
      val mapSet = map(union(singletonSet(1), singletonSet(3)), x => x * 2)

      assert(!contains(mapSet, 1), "Map 1")
      assert(contains(mapSet, 2), "Map 2")
      assert(!contains(mapSet, 3), "Map 3")
      assert(!contains(mapSet, 4), "Map 4")
      assert(!contains(mapSet, 5), "Map 5")
      assert(contains(mapSet, 6), "Map 6")
    }
  }

}
