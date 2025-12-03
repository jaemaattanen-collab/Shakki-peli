package chess.pieces

import chess.board._
import scala.collection.mutable.ListBuffer

trait DirectionalMovement { self: Piece =>
  protected def generateDirectionalMoves(directions: List[(Int, Int)]): List[Square] = 
    val moves = ListBuffer[Square]()
    val (col, row) = square.getCoordinates
    val boardSquares = board.squares
    for ((dx, dy) <- directions) {
      var currentCol = col + dx
      var currentRow = row + dy
      var continue = true
      while (continue && board.isWithinBoard(currentCol, currentRow)) {
        val targetSquare = boardSquares(currentRow)(currentCol)
        targetSquare.occupiedBy match 
          case Some(piece) =>
            if (piece.color != color) then
              // Can capture opponent's piece
              moves += targetSquare
            // Stop in either case (own piece or opponent's piece)
            continue = false
          case None =>
            // Empty square - can move here
            moves += targetSquare
        currentCol += dx
        currentRow += dy } }
    moves.toList
  end generateDirectionalMoves
}
