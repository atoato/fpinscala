package fpinscala.datastructures

import fpinscala.datastructures.List._
import org.scalatest.{FunSuite, Matchers}

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

  test("testMap") {}

  test("testInit") {
    collection.immutable.List(1).init shouldBe collection.immutable.List.empty
    collection.immutable.List(1, 2).init shouldBe collection.immutable.List(1)
    collection.immutable.List(1, 2, 3).init shouldBe collection.immutable
      .List(1, 2)

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

  test("testLength") {
    List.length(Nil) shouldBe 0
    List.length(List(1)) shouldBe 1
    List.length(List(1, 2)) shouldBe 2
  }

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

  test("ex3.8") {
    foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) shouldBe List(1, 2, 3)
  }

  test("3.10 testFoldLeft") {
    foldLeft(Nil, Nil: List[Int]) { case (acc, x) => Cons(x, acc) } shouldBe Nil
    foldLeft(List(1), Nil: List[Int]) { case (acc, x) => Cons(x, acc) } shouldBe List(
      1)
    foldLeft(List(1, 2), Nil: List[Int]) { case (acc, x) => Cons(x, acc) } shouldBe List(
      2,
      1)
    foldLeft(List(1, 2, 3), Nil: List[Int]) { case (acc, x) => Cons(x, acc) } shouldBe List(
      3,
      2,
      1)
    foldLeft(List((1 to 3): _*), 0)(_ + _) shouldBe 6
  }

  test("3.11 sum, product, length with foldLeft") {
    sum3(Nil) shouldBe 0
    sum3(List(1)) shouldBe 1
    sum3(List(1, 2)) shouldBe 3
    product3(Nil) shouldBe 1
    product3(List(1)) shouldBe 1
    product3(List(1, 2)) shouldBe 2
    length3(Nil) shouldBe 0
    length3(List(1)) shouldBe 1
    length3(List(1, 2)) shouldBe 2
    length3(List((1 to 20): _*)) shouldBe 20
  }

  test("3.12 reverse") {
    reverse(Nil) shouldBe Nil
    reverse(List(1)) shouldBe List(1)
    reverse(List(1, 2)) shouldBe List(2, 1)
  }

  test("3.13 foldLeft in terms of foldRight") {
    foldLeftWithFoldRight(Nil, Nil: List[Int]) { case (acc, x) => Cons(x, acc) } shouldBe Nil
    foldLeftWithFoldRight(List(1), Nil: List[Int]) { case (acc, x) => Cons(x, acc) } shouldBe List(
      1)
    foldLeftWithFoldRight(List(1, 2), Nil: List[Int]) { case (acc, x) => Cons(x, acc) } shouldBe List(
      2,
      1)
    foldLeftWithFoldRight(List(1, 2, 3), Nil: List[Int]) { case (acc, x) => Cons(x, acc) } shouldBe List(
      3,
      2,
      1)

  }

  test("3.14 Implement append in terms of either foldLeft or foldRight") {
    appendWithFold(Nil, Nil) shouldBe Nil
    appendWithFold(Nil, List(1)) shouldBe List(1)
    appendWithFold(List(1), List(2)) shouldBe List(1, 2)
    appendWithFold(List(1), List(2, 3)) shouldBe List(1, 2, 3)
  }

  test("3.15 Write a function that concatenates a list of lists into a single list") {
    concat(List(Nil)) shouldBe Nil
    concat(List(List(1), Nil)) shouldBe List(1)
    concat(List(Nil, List(1))) shouldBe List(1)
    concat(List(List(1), List(2))) shouldBe List(1, 2)
    concat(List(List(1, 2), List(3, 4))) shouldBe List(1, 2, 3, 4)
  }

  test("3.16 Write a function that transforms a list of integers by adding 1 to each element") {
    addOne(Nil) shouldBe Nil
    addOne(List(1)) shouldBe List(2)
    addOne(List(1, 2)) shouldBe List(2, 3)
  }

  test("3.17 Write a function that turns each value in a List[Double] into a String") {
    doublesToString(Nil) shouldBe Nil
    doublesToString(List(1.1)) shouldBe List("1.1")
    doublesToString(List(1.1, 2.2)) shouldBe List("1.1", "2.2")
  }

  test("3.18 Write a function map") {
    map(List(1))(_ + 1) shouldBe List(2)
    map(List(1, 2))(_ + 1) shouldBe List(2, 3)
    map(List(1, 2))(_ * 2) shouldBe List(2, 4)
  }

  test("3.19 Write a function filter") {
    filter(List(1))(_ > 1) shouldBe Nil
    filter(List(1))(_ == 1) shouldBe List(1)
    filter(List(1, 2, 3, 4))(_ % 2 == 0) shouldBe List(2, 4)
  }

  test("3.20 Write a function flatMap") {
    flatMap(List(1,2,3))(i => List(i,i)) shouldBe List(1,1,2,2,3,3)
  }

  test("3.21 Use flatMap to implement filter") {
    flatMapFilter(List(1))(_ > 1) shouldBe Nil
    flatMapFilter(List(1))(_ == 1) shouldBe List(1)
    flatMapFilter(List(1, 2, 3, 4))(_ % 2 == 0) shouldBe List(2, 4)
  }

  test("3.22 Write a function that accepts two lists and constructs a new list by adding corresponding elements") {
    addInts(List(1,2,3), List(4,5,6)) shouldBe List(5,7,9)
  }
}
