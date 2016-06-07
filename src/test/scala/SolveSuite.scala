import Solver._

/**
  * Created by Creed on 6/7/16.
  */
class SolveSuite extends org.scalatest.FunSuite {

  val allPositions = for (r <- 0.to(8); c <- 0.to(8)) yield { (r, c) }

  // Produces true if the board does not exclude the given solution
  private def canFindSolution(solutionStr: String) = {
    val sol = solutionStr.toArray
    (board: Board) => {
      allPositions.forall { case (r, c) =>
        sol(r * 9 + c) match {
          case '.' => true
          case ch =>  board.valueAt(r, c) match {
            case Some(v) => v == ch.toString.toInt
            case None => board.availableValuesAt(r, c).toSet.contains(ch.toString.toInt)
          }
        }
      }
    }
  }

  test("Does isSolved work?") {
    assert(parse("853697421914238675762145893128563947475982136396471582581724369637859214249316758").isSolved)
  }

  test("Does solve work (search space has less than 128 states)?") {
    val str = ".................5...145893128563947475982136396471582581724369637859214249316758"
    val b = parse(str).solve.get
    assert(b.isSolved)
    assert(canFindSolution(str)(b))
  }

  test("Does solve work (search space has less than 300 states)?") {
    val str = ".................5...145..3128563947475982136396471582581724369637859214249316758"
    val b = parse(str).solve.get
    assert(b.isSolved)
    assert(canFindSolution(str)(b))
  }

  test("Does solve work (search space has less than 1000 states)?") {
    val str = ".................5...145...1285639474759821363964715.2581724369637859.14........."
    val b = parse(str).solve.get
    assert(b.isSolved)
    assert(canFindSolution(str)(b))
  }

//  test("Does solve work from empty board?") {
//    val str = "................................................................................."
//    val b = parse(str).solve.get
//    assert(b.isSolved)
//    assert(canFindSolution(str)(b))
//  }



}
