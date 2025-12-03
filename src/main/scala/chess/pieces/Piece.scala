package chess.pieces

import chess.types.PieceType
import chess.board.{Square, Board}

import java.awt.Image

abstract class Piece(val color: String, var square: Square, val board: Board, val pieceType: PieceType) {
  // Upon creation, set this piece on the square
  square.occupiedBy = Some(this)
  var hasMoved: Boolean = false // Track if the piece has moved
  var isCaptured: Boolean = false // **Add this line**

  def getImage: Image

  def possibleMoves(): List[Square]

  def attackSquares(): List[Square]
  

  def updatePossibleMoves(newMoves: List[Square]): Unit

  def moveTo(to: Square, isSimulation: Boolean = false): Unit = 
    square.occupiedBy = None
    square = to
    to.occupiedBy = Some(this)
    if (!isSimulation) then
      hasMoved = true
  end moveTo

  def capture(): Unit = 
    square.occupiedBy = None
    hasMoved = true
    isCaptured = true
  end capture

  def promoteTo(newType: String): Unit =
    val square = this.square
    square.occupiedBy = None

    val promotedPiece = newType.toLowerCase match
      case "queen"  => new Queen(color, square, board)
      case "rook"   => new Rook(color, square, board)
      case "bishop" => new Bishop(color, square, board)
      case "knight" => new Knight(color, square, board)
      case _        => new Queen(color, square, board)

    square.occupiedBy = Some(promotedPiece)
  end promoteTo
}
