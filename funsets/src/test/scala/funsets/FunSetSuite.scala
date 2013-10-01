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
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  test("contains even") {
    assert(contains(x => x % 2 == 0, 100))
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
    val s12 = union(s1, s2)
    val s23 = union(s2, s3)
    val empty: Set = x => false
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1 & doesn't contain 11") {

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
      assert(!contains(s1, 11), "Singleton")
    }
  }

  test("union") {
    new TestSets {
      val s = union(union(s1, s2), empty)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersection") {
    new TestSets {
      val si = intersect(s12, s23)
      val sie = intersect(s12, empty)

      assert(!contains(si, 1))
      assert(contains(si, 2))
      assert(!contains(si, 3))

      for (i <- 1 to 10) {
        assert(contains(s2, i) == contains(si, i))
        assert(!contains(sie, i))
      }
    }
  }

  test("diff") {
    new TestSets {
      val sd = diff(s12, s23)
      val sde = diff(s12, empty)
      val sed = diff(empty, s12)

      assert(contains(sd, 1))
      assert(!contains(sd, 2))
      assert(!contains(sd, 3))

      assert(contains(sde, 1))
      assert(contains(sde, 2))
      assert(!contains(sde, 3))

      assert(!contains(sed, 1))
      assert(!contains(sed, 2))
      assert(!contains(sed, 3))
    }
  }

  test("filter") {
    new TestSets {
      val su = union(s12, s23)
      val sf = filter(su, x => x % 2 == 1)
      val sfe = filter(empty, x => true)

      assert(!contains(sf, 0))
      assert(contains(sf, 1))
      assert(!contains(sf, 2))
      assert(contains(sf, 3))
      assert(!contains(sf, 4))

      for (i <- -10 to 10) {
        assert(!contains(sfe, i))
      }
    }
  }

  test("forall") {
    new TestSets {

      def oddset(): Set = {
        def loop(i: Int, os: Set): Set = {
          if (i > bound) os
          else if (i % 2 != 0) loop(i + 1, union(os, singletonSet(i)))
          else loop(i + 1, os)
        }
        loop(-bound, empty)
      }

      val os = oddset()

      assert(!forall(os, x => x % 2 == 0))
      assert(forall(os, x => x % 2 != 0))
    }
  }

  test("exists") {
    new TestSets {

      def oddset(): Set = {
        def loop(i: Int, os: Set): Set = {
          if (i > bound) os
          else if (i % 2 != 0) loop(i + 1, union(os, singletonSet(i)))
          else loop(i + 1, os)
        }
        loop(-bound, empty)
      }

      val os = oddset()

      assert(!exists(os, x => x % 2 == 0))

      val s123 = union(union(s1, s2), s3)

      assert(!exists(s123, x => x == 0))
      assert(exists(s123, x => x == 2))
    }
  }

  test("map") {
    new TestSets {

      val s123 = union(union(s1, s2), s3)
      
      val ms = map(s123, x => 10 * x)
      
      assert(contains(ms, 10))
      assert(contains(ms, 20))
      assert(contains(ms, 30))
    }
  }

}
