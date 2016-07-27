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
    assert(contains(singletonSet(100), 100))
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
    }
  }

  test("singletonSet(2) contains 2") {
    new TestSets {
      assert(contains(s2, 2), "Singleton")
    }
  }

  test("singletonSet(3) contains 3") {
    new TestSets {
      assert(contains(s3, 3), "Singleton")
    }
  }

  test("singletonSet(2) doesn't contain 4") {
    new TestSets {
      assert(!contains(s2, 4), "Singleton")
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

  test("union of a union with a set contains all elemets of each set") {
    new TestSets {
      val u1 = union(union(s1, s2), s3)
      assert(contains(u1, 1), "Union 1")
      assert(contains(u1, 2), "Union 1")
      assert(contains(u1, 3), "Union 2")
    }
  }

  trait TestSets2 {
    val s1 = union(singletonSet(1), singletonSet(2))
    val s2 = union(singletonSet(2), singletonSet(4))
    val s3 = union(singletonSet(3), singletonSet(5))
  }

  test("intersection contains only the elements in common of both sets") {
    new TestSets2 {
      val i1 = intersect(s1, s2)
      assert(contains(i1, 2), "Intersection1")
    }
  }

  test("intersection of two sets with no elements in common contains no elements") {
    new TestSets2 {
      val i1 = intersect(s1, s3)
      assert(!contains(i1, 2), "Empty intersection")
    }
  }

  test("diff contains the elements of the first set that are not in the second") {
    new TestSets2 {
      val d1 = diff(s1, s2)
      assert(contains(d1, 1), "Number in diff")
      assert(!contains(d1, 2), "Number not in diff")
    }
  }

  test("diff of the same set is empty") {
    new TestSets2 {
      val d1 = diff(s1, s1)
      assert(!contains(d1, 1), "First element")
      assert(!contains(d1, 2), "Second element")
    }
  }

  trait TestSets3 {
    val s1 = union(union(singletonSet(1), singletonSet(2)), union(singletonSet(3), singletonSet(4)))
  }

  test("filter of even numbers contain only even numbers") {
    new TestSets3 {
      val f1 = filter(s1, { num => num % 2 == 0 })
      assert(contains(f1, 2), "Filter 1")
      assert(contains(f1, 4), "Filter 1")
      assert(!contains(f1, 3), "Filter 1")
    }
  }

  test("filter a set with a function that doesn't apply to any elements yields an empty set") {
    new TestSets3 {
      val f1 = filter(s1, { num => num > 4 })
      assert(!contains(f1, 5), "Filter 1")
      assert(!contains(f1, 10), "Filter 1")
    }
  }

  trait TestSets4 {
    val s1 = union(union(singletonSet(1), singletonSet(2)), singletonSet(3))
    val s2 = union(union(singletonSet(2), singletonSet(4)), union(singletonSet(6), singletonSet(8)))
  }

  test("for all a from the set, where a is an even number, it's true that a % 2 == 0") {
    new TestSets4 {
      assert(forall(s2, { n => n % 2 == 0 }) === true, "Forall 2")
    }
  }

  test("for all a between 1 and 3, it's not true that a % 2 == 0") {
    new TestSets4 {
      assert(forall(s1, { n => n % 2 == 0 }) === false, "Forall 1")
    }
  }

  test("for all a from the set, where 1 <= a <= 3, it's not true that a * a >= 10") {
    val s1 = union(union(singletonSet(1), singletonSet(2)), singletonSet(3))
    assert(forall(s1, { n => n * n >= 10 }) === false, "Forall 2")
  }

  test("exists in a set of even number is false for an odd number") {
    new TestSets4 {
      assert(exists(s2, { n => n % 2 != 0 }) === false, "Exists 1")
    }
  }

  test("exists in a set with an even number is true for an even number") {
    new TestSets4 {
      assert(exists(s1, {n => n % 2 == 0 }) === true, "Exists 2")
    }
  }

  test("mapping a set with the squared function returns all the elements squared") {
    new TestSets4 {
      assert(contains(map(s1, { n => n * n }), 1))
      assert(contains(map(s1, { n => n * n }), 4))
      assert(contains(map(s1, { n => n * n }), 9))
    }
  }

  test("mapping a set with a +1 function to all elements doesn't return any element besides all the elements +1") {
    new TestSets4 {
      assert(forall(map(s1, { n => n + 1 }), { n => n >= 2 && n <= 4 }) === true)
    }
  }
}
