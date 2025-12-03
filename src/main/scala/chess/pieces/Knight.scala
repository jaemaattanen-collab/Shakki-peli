package chess.pieces

import chess.types.PieceType._
import chess.board.{Square, Board}
import chess.utils.ImageManager

import java.awt.Image

class Knight(color: String, initialSquare: Square, board: Board) extends Piece(color, initialSquare, board, Knight) {

  private var currentPossibleMoves: List[Square] = calculatePossibleMoves()

  override def getImage: Image =
    val imageName = if (color == "white") "white-knight.png" else "black-knight.png"
    ImageManager.getImage(imageName)
  end getImage

  private def calculatePossibleMoves(): List[Square] = 
    val moves = scala.collection.mutable.ListBuffer[Square]()
    val (col, row) = square.getCoordinates
    val boardSquares = board.squares
    val moveOffsets = List((2, 1), (1, 2), (-1, 2), (-2, 1), (-2, -1), (-1, -2), (1, -2), (2, -1))
    for ((dx, dy) <- moveOffsets) {
      val newCol = col + dx
      val newRow = row + dy
      if (board.isWithinBoard(newCol, newRow)) then
        val targetSquare = boardSquares(newRow)(newCol)
        targetSquare.occupiedBy match
          case Some(piece) =>
            if (piece.color != color) then
              moves += targetSquare
          case None =>
            moves += targetSquare }
    moves.toList
  end calculatePossibleMoves

  override def possibleMoves(): List[Square] = calculatePossibleMoves()

  override def attackSquares(): List[Square] =
    val attacks = scala.collection.mutable.ListBuffer[Square]()
    val (col, row) = square.getCoordinates
    val boardSquares = board.squares
    val moveOffsets = List((2, 1), (1, 2), (-1, 2), (-2, 1), (-2, -1), (-1, -2), (1, -2), (2, -1))
    for ((dx, dy) <- moveOffsets) {
      val newCol = col + dx
      val newRow = row + dy
      if (board.isWithinBoard(newCol, newRow)) then
        val targetSquare = boardSquares(newRow)(newCol)
        attacks += targetSquare }
    attacks.toList
  end attackSquares

  override def updatePossibleMoves(newMoves: List[Square]): Unit = currentPossibleMoves = newMoves
}
