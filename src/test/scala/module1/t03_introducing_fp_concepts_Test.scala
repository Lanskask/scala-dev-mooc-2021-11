package module1

import org.scalatest.FunSuite

class t03_introducing_fp_concepts_Test extends FunSuite {

  test("testCons") {
    val l1 = list.List(1,2,3)
    val l2 = list.List()

    assert(l1.cons(4).mkString(", ") == "4, 1, 2, 3")
    assert(l2.cons(4).mkString(", ") == "4")
  }

  test("testMkString") {
    val l1 = list.List(1,2,3)
    val l2 = list.List()

    assert(l1.mkString(", ") == "1, 2, 3")
    assert(l2.mkString(", ") == "")
  }

  test("testReverse") {
    val l1 = list.List(1,2,3)
    val result = l1.reverse().mkString(",")
    assert(result == "3,2,1")
  }

  test("testMap") {
    val l1 = list.List(1,2,3)
    val result = l1.map(x => x + 1).mkString(", ")
    assert(result == "2, 3, 4")
  }

  test("testFilter") {
    val l1 = list.List(1,2,3)
    val result = l1.filter(x => x - 1 != 0).mkString(", ")
    assert(result == "2, 3")
  }

  test("testIncList") {
    val l1 = list.List(1,2,3)

    assert(list.incList(l1).mkString(", ") == "2, 3, 4")
  }

  test("testShoutString") {
    val l1 = list.List("1","2","3")

    assert(list.shoutString(l1).mkString(", ") == "1!, 2!, 3!")
  }

}
