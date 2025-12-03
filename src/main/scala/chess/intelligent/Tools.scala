package chess.intelligent

import chess.board._

import scala.collection.mutable.ListBuffer

def squareIndexes(board:Board) =
  board.squares.foreach { row =>
      row.foreach { square =>
        square.occupiedBy.foreach { piece =>
          val attackSquares = piece.attackSquares()

          attackSquares.foreach { targetSquare =>
            if piece.color == "white" then
              targetSquare.seenByWhite += 1
            else
              targetSquare.seenByBlack += 1
          }
        }
      }
    }

def hangingPieces(board: Board): List[Square] =
  val nums = ListBuffer.empty[Square]
  board.squares.foreach { row =>
    row.foreach { square =>
      if square.seenByBlack > square.seenByWhite && square.occupiedBy.exists(_.color == "white") then
        nums += square
      else if square.seenByBlack < square.seenByWhite && square.occupiedBy.exists(_.color == "black") then
        nums += square
    }}
  val targets = nums.toList
  targets