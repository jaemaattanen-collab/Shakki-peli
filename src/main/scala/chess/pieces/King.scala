package chess.pieces

import chess.types.PieceType._
import chess.board.{Square, Board}
import chess.utils.ImageManager
import java.awt.Image

class King(color: String, initialSquare: Square, board: Board) extends Piece(color, initialSquare, board, King) {
  private var currentPossibleMoves: List[Square] = calculatePossibleMoves()

  override def getImage: Image =
    val imageName = if (color == "white") "white-king.png" else "black-king.png"
    ImageManager.getImage(imageName)
  end getImage

  private def calculatePossibleMoves(): List[Square] = 
    val moves = scala.collection.mutable.ListBuffer[Square]()
    val (col, row) = square.getCoordinates
    val boardSquares = board.squares
    val moveOffsets = List((1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (1, -1), (-1, 1), (-1, -1))
    for ((dx, dy) <- moveOffsets) {
      val newCol = col + dx
      val newRow = row + dy
      if (board.isWithinBoard(newCol, newRow)) then
        val targetSquare = boardSquares(newRow)(newCol)
        targetSquare.occupiedBy match
          case Some(piece) =>
            if piece.color != color then
              moves += targetSquare
          case None =>
            moves += targetSquare}
    if (!hasMoved && !board.isKingInCheck(color)._1) then
      if (canCastle(col, row, 1)) then
        moves += boardSquares(row)(col + 2)
      if (canCastle(col, row, -1))
        moves += boardSquares(row)(col - 2) // Add castling move to possible moves
    moves.toList
  end calculatePossibleMoves


  private def canCastle(col: Int, row: Int, direction: Int): Boolean =
    val rookCol = if (direction == 1) 7 else 0
    val squaresToCheck = if (direction == 1) List(col + 1, col + 2) else List(col - 1, col - 2, col - 3)
    
    // Check if all squares to check are within bounds
    if (!squaresToCheck.forall(c => c >= 0 && c < 8)) return false
    
    val boardSquares = board.squares
    val pathClear = squaresToCheck.forall(c => boardSquares(row)(c).occupiedBy.isEmpty)
    if (pathClear) 
      val attackSquares = board.generateAttackSquaresBuffer(if (color == "white") "black" else "white")
      val pathSafe = squaresToCheck.map(c => boardSquares(row)(c)).forall(!attackSquares.contains(_))
      val rookInPlace = boardSquares(row)(rookCol).occupiedBy match 
        case Some(rook: Rook) if !rook.hasMoved && rook.color == color =>
          true
        case _ =>
          false
      pathSafe && rookInPlace
    else 
      false
  end canCastle

  override def possibleMoves(): List[Square] = calculatePossibleMoves()

  override def attackSquares(): List[Square] =
    val attacks = scala.collection.mutable.ListBuffer[Square]()
    val (col, row) = square.getCoordinates
    val boardSquares = board.squares
    val attackOffsets = List((1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (1, -1), (-1, 1), (-1, -1))
    for ((dx, dy) <- attackOffsets) {
      val newCol = col + dx
      val newRow = row + dy
      if (board.isWithinBoard(newCol, newRow)) then
        val targetSquare = boardSquares(newRow)(newCol)
        attacks += targetSquare }
    attacks.toList
  end attackSquares

  override def updatePossibleMoves(newMoves: List[Square]): Unit = currentPossibleMoves = newMoves
}
