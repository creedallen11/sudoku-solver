import Solver._

/**
  * Created by Creed on 6/6/16.
  */
class Parse_and_GettersSuite extends org.scalatest.FunSuite {

  test("Empty string builds empty board") {
    val emptyStr = "................................................................................."
    assert(parse(emptyStr).toString == new Board(Map()).toString)
  }

  test("Basic parse test builds .") {
    val input = "1................................................................................"
    val expected = new Board(Map((0,0) -> List(1), (0,1) -> List(2,3,4,5,6,7,8,9),
      (0,2) -> List(2,3,4,5,6,7,8,9),(0,3) -> List(2,3,4,5,6,7,8,9),(0,4) -> List(2,3,4,5,6,7,8,9)
      ,(0,5) -> List(2,3,4,5,6,7,8,9),(0,6) -> List(2,3,4,5,6,7,8,9),(0,7) -> List(2,3,4,5,6,7,8,9),
      (0,8) -> List(2,3,4,5,6,7,8,9),(1,0) -> List(2,3,4,5,6,7,8,9),(2,0) -> List(2,3,4,5,6,7,8,9),
      (3,0) -> List(2,3,4,5,6,7,8,9),(4,0) -> List(2,3,4,5,6,7,8,9),(5,0) -> List(2,3,4,5,6,7,8,9),
      (6,0) -> List(2,3,4,5,6,7,8,9),(7,0) -> List(2,3,4,5,6,7,8,9),(8,0) -> List(2,3,4,5,6,7,8,9),
      (1,1) -> List(2,3,4,5,6,7,8,9),(1,2) -> List(2,3,4,5,6,7,8,9),
      (2,1) -> List(2,3,4,5,6,7,8,9),(2,2) -> List(2,3,4,5,6,7,8,9)))
    assert(parse(input).toString === expected.toString)
  }

  test("value at returns a Some(value) when there is a value") {
    val instance = new Board(Map((0,0) -> List(1), (1,1) -> List(2),
      (2,2) -> List(3), (3,3) -> List(4), (4,4) -> List(5), (5,5) -> List(6),
      (6,6) -> List(7), (7,7) -> List(8), (8,8) -> List(9)))
    assert(instance.valueAt(0,0) == Some(1))
  }

  test("value at returns None when there is a List of ints") {
    val instance = new Board(Map((0,0) -> List(1), (1,1) -> List(2),
      (2,2) -> List(3), (3,3) -> List(4), (4,4) -> List(5), (5,5) -> List(6),
      (6,6) -> List(7), (7,7) -> List(8), (8,8) -> List(9)))
    assert(instance.valueAt(3, 2).isEmpty)
  }

  test("Do parse and valueAt work?") {
    val str = ".1.....2..3..9..1656..7...33.7..8..........89....6......6.254..9.5..1..7..3.....2"
    val b = parse(str)
    for (r <- 0.to(8); c <- 0.to(8)) {
      str(r * 9 + c) match {
        case '.' => ()
        case ch => assert(b.valueAt(r, c).contains(ch.toString.toInt))
      }
    }
  }

  test("Does availableValuesAt work (row elimination)?") {
    val str = "12..............................................................................."
    val b = parse(str)
    assert(b.availableValuesAt(0, 2).toSet == Set(3,4,5,6,7,8,9))
  }

  test("Does availableValuesAt work (column elimination)?") {
    val str = "1.......................................................................2........"
    val b = parse(str)
    assert(b.availableValuesAt(2, 0).toSet == Set(3,4,5,6,7,8,9))
  }

  test("Does availableValuesAt work (box elimination)?") {
    val str = "1................................................................................"
    val b = parse(str)
    assert(b.availableValuesAt(1, 1).toSet == Set(2,3,4,5,6,7,8,9))
  }


}
