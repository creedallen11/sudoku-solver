/**
  * Created by Creed on 6/4/16.
  */
object Solver extends App {

  /*
  Takes a string representation of a Sudoku board and returns a Board object.
  Each block of 9 characters is a row (row-major order).
  ASSUMPTION: String is 81 characters and matches the regex """(\.|[1-9]){81}""".r
   */
  def parse(str: String) = {


   /* Takes the list of singletons as lst, and recursively updates peers of all singletons so the available at
   of each coordinate is consistent with placements. */
    def buildStartMap(lst: List[((Int, Int), Int)], m: Map[(Int, Int), List[Int]]): Map[(Int, Int), List[Int]] =
      lst match {
        case Nil => m
        case head::tail => buildStartMap(lst.tail, filterPeers(m, peers(head._1._1, head._1._2), head._2))
      }

    /* Update the row, col and box of all singletons (placed numbers). */
    def filterPeers(m: Map[(Int, Int), List[Int]], keys: List[(Int, Int)], n: Int): Map[(Int, Int), List[Int]] = keys match {
      case Nil => m
      case head::tail => filterPeers(m updated(head, m(head) filter (x => x != n)), tail, n)
    }

    // Generate key->value for initial unfiltered map representing a board.
    val keys = (for (row <- 0 to 8; col <- 0 to 8) yield (row, col)) toList
    val values = for (v <- str.toList) yield {
      if (v == '.') (1 to 9).toList
      else List(v.asDigit)
    }

    // (row, col) -> legal placements at (row, col)
    val unfilteredMap = (keys zip values) toMap

    // Generate list of coordinates that need their peers updated.
    val toUpdate = (for (i <- 0 to 8; j <- 0 to 8 if unfilteredMap((i, j)).length == 1)
      yield {
        ((i, j), unfilteredMap(i, j).head)
      }) toList

    new Board(buildStartMap(toUpdate, unfilteredMap))
  }

  /*
  Takes in a (row, col) coordinate of Sudoku Board and returns a list of peers of
  (row, col) excluding (row, col). Peers are defined as the coordinate's row, col and
  grid box in the sudoku puzzle.
  ASSUMPTION: 0 <= row/col <= 8
   */
  def peers(row: Int, col: Int): List[(Int, Int)] = {
    val rowStart = row - (row % 3)
    val colStart = col - (col % 3)

    val box = (for (i <- rowStart to rowStart + 2 ; j <- colStart to colStart + 2) yield (i, j)).toList

    val unfilteredPeers = ((for (i <- 0 to 8) yield (row, i)) ++ (for (j <- 0 to 8) yield (j, col))++ box).toList

    (unfilteredPeers filter (x => x !=(row, col))).distinct
  }

  // Top-left corner is (0, 0). Bottom-right corner is (8,8).
  // You don't have to have a field called available. Feel free to change it.
  class Board(val available: Map[(Int, Int), List[Int]]) {

    /* Assumes a missing value means all available. Returns list of available drops as position. */
    def availableValuesAt(row: Int, col: Int): List[Int] = {
      available.getOrElse((row, col), 1.to(9).toList)
    }

    /* Returns Some(value) if there is a value restriction, else None */
    def valueAt(row: Int, col: Int): Option[Int] = {
      available get (row, col) match {
        case Some(x) => if (x.length == 1) Some(x.head) else None
        case None => None
      }
    }

    def isSolved: Boolean = {
      for (i <- 0 to 8; j <- 0 to 8 if valueAt(i, j).isEmpty) return false
      true
    }

    def isUnsolvable: Boolean = {
      for (i <- 0 to 8; j <- 0 to 8)
        if (availableValuesAt(i, j).isEmpty) return true
      false
    }

    /* Places a value at (row, col) and updates the board accordingly. */
    def place(row: Int, col: Int, value: Int): Board = {
      require(availableValuesAt(row, col).contains(value))
      val pMap = available updated((row, col), List(value))
      val peersLst = peers(row, col) filter (x => pMap(x).contains(value))

      /* take the map, take the list of peers that need update, the value) */
      def updateMap(m: Map[(Int, Int), List[Int]], pList: List[(Int, Int)], n: Int): Map[(Int, Int), List[Int]] =
        pList match {
          case Nil => m
          case head::tail =>
            val updatedMap = m updated (pList.head, m(pList.head._1, pList.head._2) filter (x => x != n))
            // if an updated mapping became a singleton, update its peers too
            if (updatedMap(pList.head._1, pList.head._2).length == 1)
              updateMap(
                updateMap(updatedMap, peers(pList.head._1, pList.head._2) filter (x => updatedMap(x._1, x._2) contains updatedMap(pList.head._1, pList.head._2).head),
                  updatedMap(pList.head._1, pList.head._2).head), tail, n)
            else updateMap(updatedMap, tail, n)
        }
      new Board(updateMap(pMap, peersLst, value))
    }

    /* Returns a list of boards in the next branching considering all possible ways to place the first value. */
    def nextStates(): List[Board] = {
      if (isUnsolvable) {
        List()
      }
      else {
        def valueCount(board: Board): Int = {
          val count = (for (i <- 0 to 8; j <- 0 to 8 if board.available(i, j).length > 1) yield board.available(i, j).length - 1) toList

          count.sum
        }

        val nextMovesCount = (for (i <- 0 to 8; j <- 0 to 8 if available(i, j).length > 1) yield {
          for (value <- available(i,j)) yield {
            (place(i,j, value), valueCount(place(i,j, value)))
          }
        } ) toList

        val tups = nextMovesCount.flatten.sortWith((x,y) => y._2 > x._2 )
        for (v <- tups) yield v._1
      }
    }

    /* Find and return Option[solved board] */
    def solve(): Option[Board] = {
      if (this.isSolved) {
        return Some(this)
      }
      else for (boards <- this.nextStates()) {
        boards.solve() match {
          case Some(x) => return Some(x)
          case None =>
        }
        None
      }
      None
    }

    override def toString: String = {
      var str: String = ""
      for(i <- 0 to 8){
        if(i == 0 || i == 3 || i == 6) str = str + "-------------\n"
        for(j <- 0 to 8){
          if(j == 0 || j == 3 || j == 6) str = str + "|"
          // hack so toString can be used in testing, should call class method
          val ls:List[Int] = available.getOrElse((i, j), (1 to 9).toList)
          if(ls.size == 1) str = str + ls.head
          else str = str + "."
          if(j == 8) str = str + "|"
        }
        str = str + "\n"
        if(i == 8) str = str + "-------------\n"
      }
      str
    }
  }
}
