package fpinscala.datastructures

import org.scalatest.{FunSuite, Matchers}
import List._

class ListTest extends FunSuite with Matchers {

  test("testDrop") {
    drop(Nil, 0) shouldBe Nil
    drop(Nil, 1) shouldBe Nil
    drop(List(1), 0) shouldBe List(1)
    drop(List(1, 2), 0) shouldBe List(1, 2)
    drop(List(1, 2), 1) shouldBe List(2)
    drop(List(1, 2), 2) shouldBe Nil
    drop(List(1), 10) shouldBe Nil
  }

  test("testFoldLeft") {}

  test("testMap") {}

  test("testInit") {
    init(Nil) shouldBe Nil
    init(List(1)) shouldBe Nil
    init(List(1, 2)) shouldBe List(1)
    init(List(1, 2, 3)) shouldBe List(1, 2)
  }

  test("testTail") {
    tail(Nil) shouldBe Nil
    tail(List(1)) shouldBe Nil
    tail(List(1, 2)) shouldBe List(2)
  }

  test("testLength") {}

  test("testSetHead") {
    setHead(Nil, 1) shouldBe List(1)
    setHead(List(1), 2) shouldBe List(2)
    setHead(List(1, 2), 3) shouldBe List(3, 2)
  }

  test("testDropWhile") {
    dropWhile(Nil, (x: Nothing) => true) shouldBe Nil
    dropWhile(Nil, (x: Nothing) => false) shouldBe Nil
    dropWhile(List(1), (_: Int) > 0) shouldBe Nil
    dropWhile(List(1), (_: Int) < 0) shouldBe List(1)
    dropWhile(List(1, 2), (_: Int) > 0) shouldBe Nil
    dropWhile(List(1, 2), (_: Int) < 0) shouldBe List(1, 2)
  }

}
