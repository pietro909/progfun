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
    val s3b = singletonSet(3)
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

  test("intersect contains only the elements which are in both set") {
    new TestSets {
      val u = union(s1, s2)
      val s = intersect(u, s2)
      assert(contains(s, 2), "Intersect 1")
      assert(!contains(s, 1), "Intersect 2")
    }
  }

  test("diff contains only the elements which are in the first set but not in the second") {
    new TestSets {
      val u = union(s1, s2)
      val s = diff(u, s2)
      assert(contains(s, 1), "Diff 1")
      assert(!contains(s, 2), "Diff 2")
    }
  }

  test("filter contains only the elements which are in the set and satisfy p") {
    new TestSets {
      val u = union(s1, s2)
      val s = filter(u, (n: Int) => n == 1)
      assert(contains(s, 1), "Filter 1")
      assert(!contains(s, 2), "Filter 2")
    }
  }

  test("forall is true if all the element of a set satisfy the given predicate") {
    new TestSets {
      val s = union(s1,s2)
      val isTrue = forall(s, (n: Int) => n < 3)
      assert(isTrue, "Forall 1")
      val isFalse = forall(s, (n: Int) => n > 3)
      assert(!isFalse, "Forall 2")
    }
  }

  test("exists is true if at least one the element of a set satisfies the given predicate") {
    new TestSets {
      val s = union(s1,s2)
      val isTrue = exists(s, (n: Int) => n < 2)
      assert(isTrue, "Exists 1")
      val isFalse = exists(s, (n: Int) => n > 3)
      assert(!isFalse, "Exists 2")
    }
  }

  test("map returns a set of tranformed element from s using f") {
    new TestSets {
      val s = union(s2, s3)
      val doubled = map(s, (n: Int) => n * 2)
      assert(contains(doubled, 4), "Map 1")
    }
  }

}
