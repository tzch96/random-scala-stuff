import scala.util.Random
import scala.util.control.Breaks._

/** Simple implementation of Conway's Game of Life */
object GameOfLife {
  val random = new Random()

  def main(args: Array[String]) = {
    val width = 5
    val height = 10
    var board = Array.ofDim[Int](width, height)

    // Populate the board with 0 or 1 at random
    for (i <- 0 until width) {
      for (j <- 0 until height) {
        board(i)(j) = random.nextInt(2)
      }
    }

    for (i <- 0 to 10) {
      println()
      printBoard(board, width, height)
      board = nextBoard(board, width, height)
    }
  }

  def printBoard(board: Array[Array[Int]], width: Int, height: Int): Unit = {
    for (i <- 0 until width) {
      for (j <- 0 until height) {
        print(board(i)(j))
      }
      println()
    }
  }

  def nextBoard(board: Array[Array[Int]], width: Int, height: Int): Array[Array[Int]] = {
    var nextBoard = Array.ofDim[Int](width, height)

    for (i <- 0 until width) {
      for (j <- 0 until height) {
        var aliveNeighbors = 0
        for (k <- -1 to 1) {
          for (l <- -1 to 1) {
            breakable {
              if (i + k < 0 || j + l < 0 || i + k > width - 1 || j + l > height - 1) {
                break
              } else {
                aliveNeighbors += board(i + k)(j + l)
              }
            }
          }
        }

        var currentCell = board(i)(j)
        aliveNeighbors -= currentCell

        // game rules
        if (currentCell == 1 && (aliveNeighbors < 2 || aliveNeighbors > 3))
          nextBoard(i)(j) = 0
        else if (currentCell == 0 && aliveNeighbors == 3)
          nextBoard(i)(j) = 1
        else
          nextBoard(i)(j) = currentCell
      }
    }

    return nextBoard
  }
}
