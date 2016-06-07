import Solver._

/**
  * Created by Creed on 6/7/16.
  */
class BoardMovesSuite extends org.scalatest.FunSuite {

  test("Does place work?") {
    val str = "12..............................................................................."
    assert(parse(str).place(0, 2, 3).valueAt(0, 2).contains(3))
  }

  val puz = "...8...42.....1.7.9....2..3..7....5.1...3...4.4....8..4..9....6.5.2...1.28...6..."

  test("isSolved returns true if every cell is constrained to one value") {
    val solvedInstance = new Board((for (i <- 0 to 8; j <- 0 to 8) yield (i, j) -> List(1)) toMap)
    assert(solvedInstance.isSolved)
  }
  test("isSolved returns false on unsolved board that is solvable") {
    assert(!parse(puz).isSolved)
  }

  test("isUnsolvable returns true when a cell is restricted to the empty list") {
    // a board containing a mapping to empty list
    val board = new Board(Map((4,2) -> List()))
    assert(board.isUnsolvable)
  }

  test("isUnsolvable returns true <=> a cell is restricted to the empty cell"){
    assert(new Board(parse(puz).available updated((0, 0), List[Int]())).isUnsolvable)
  }

  test("place executes basic case properly") {
    val board = new Board(Map())
    val emptyStr = "................................................................................."
    // Expected map after playing a 1 in (0,0) on empty board.
    val expected = new Board(Map((0,0)->List(1), (0,1)->List(2,3,4,5,6,7,8,9), (0,2)->List(2,3,4,5,6,7,8,9),
      (0,3)->List(2,3,4,5,6,7,8,9), (0,4)->List(2,3,4,5,6,7,8,9), (0,5)->List(2,3,4,5,6,7,8,9),
      (0,6)->List(2,3,4,5,6,7,8,9),(0,7)->List(2,3,4,5,6,7,8,9),(0,8)->List(2,3,4,5,6,7,8,9),

      (1,0)->List(2,3,4,5,6,7,8,9),(2,0)->List(2,3,4,5,6,7,8,9),(3,0)->List(2,3,4,5,6,7,8,9),
      (4,0)->List(2,3,4,5,6,7,8,9),(5,0)->List(2,3,4,5,6,7,8,9),(6,0)->List(2,3,4,5,6,7,8,9),
      (7,0)->List(2,3,4,5,6,7,8,9),(8,0)->List(2,3,4,5,6,7,8,9),

      (1,1)->List(2,3,4,5,6,7,8,9), (1,2)->List(2,3,4,5,6,7,8,9),
      (2,1)->List(2,3,4,5,6,7,8,9), (2,2)->List(2,3,4,5,6,7,8,9)
    )) // Change this to for yield apply

    assert(parse(emptyStr).place(0,0,1).toString == expected.toString)
  }

}
