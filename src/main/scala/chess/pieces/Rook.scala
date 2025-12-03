package chess.pieces

import chess.types.PieceType._
import chess.board.{Square, Board}
import chess.utils.ImageManager
import java.awt.Image

class Rook(color: String, initialSquare: Square, board: Board) extends Piece(color, initialSquare, board, Rook) with DirectionalMovement {
  private var currentPossibleMoves: List[Square] = calculatePossibleMoves()

  override def getImage: Image = 
    val imageName = if (color == "white") "white-rook.png" else "black-rook.png"
    ImageManager.getImage(imageName)
  end getImage
  
  private def calculatePossibleMoves(): List[Square] = 
    // Rook moves in four straight directions
    val directions = List((0, -1), (0, 1), (-1, 0), (1, 0))
    val moves = generateDirectionalMoves(directions)
    moves
  end calculatePossibleMoves

  override def possibleMoves(): List[Square] = calculatePossibleMoves()

  override def attackSquares(): List[Square] = 
    val attacks = scala.collection.mutable.ListBuffer[Square]()
    val (col, row) = square.getCoordinates
    val boardSquares = board.squares

    val directions = List((0, -1), (0, 1), (-1, 0), (1, 0))
    for ((dx, dy) <- directions) {
      var currentCol = col + dx
      var currentRow = row + dy
      var continue = true

      while (continue && board.isWithinBoard(currentCol, currentRow)) {
        val targetSquare = boardSquares(currentRow)(currentCol)
        attacks += targetSquare
        targetSquare.occupiedBy match
          case Some(_) =>
            continue = false
          case None =>
        currentCol += dx
        currentRow += dy } }
    attacks.toList
  end attackSquares

  override def updatePossibleMoves(newMoves: List[Square]): Unit = currentPossibleMoves = newMoves
}