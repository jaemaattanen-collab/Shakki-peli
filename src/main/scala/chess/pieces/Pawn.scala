package chess.pieces

import chess.types.PieceType._
import chess.board.{Square, Board}
import chess.utils.ImageManager
import java.awt.Image

class Pawn(color: String,initialSquare: Square,board: Board) extends Piece(color, initialSquare, board, Pawn) {
  var justMovedTwoSquares: Boolean = false
  private var currentPossibleMoves: List[Square] = calculatePossibleMoves()

  override def getImage: Image =
    val imageName = if (color == "white") "white-pawn.png" else "black-pawn.png"
    ImageManager.getImage(imageName)
  end getImage

  private def calculatePossibleMoves(): List[Square] =
    if (isCaptured) then
      return List.empty[Square]
    val moves = scala.collection.mutable.ListBuffer[Square]()
    val (col, row) = square.getCoordinates
    val boardSquares = board.squares
    val direction = if (color == "white") -1 else 1
    val startRow = if (color == "white") 6 else 1
    val oneStepRow = row + direction
    if (board.isWithinBoard(col, oneStepRow)) then
      val oneStepSquare = boardSquares(oneStepRow)(col)
      if (oneStepSquare.occupiedBy.isEmpty) then
        moves += oneStepSquare
        if (row == startRow) then
          val twoStepRow = row + 2 * direction
          val twoStepSquare = boardSquares(twoStepRow)(col)
          if (twoStepSquare.occupiedBy.isEmpty) then
            moves += twoStepSquare
    val diagLeftCol = col - 1
    if (board.isWithinBoard(diagLeftCol, oneStepRow)) then
      val diagLeftSquare = boardSquares(oneStepRow)(diagLeftCol)
      diagLeftSquare.occupiedBy match 
        case Some(piece) if piece.color != color =>
          moves += diagLeftSquare
        case _ => 
    val diagRightCol = col + 1
    if (board.isWithinBoard(diagRightCol, oneStepRow)) then
      val diagRightSquare = boardSquares(oneStepRow)(diagRightCol)
      diagRightSquare.occupiedBy match 
        case Some(piece) if piece.color != color =>
          moves += diagRightSquare
        case _ =>
    val leftCol = col - 1
    val rightCol = col + 1
    if (row == (if (color == "white") 3 else 4)) then
      val enPassantRow = row + direction
      if (board.isWithinBoard(leftCol, row)) then
        val leftSquare = boardSquares(row)(leftCol)
        leftSquare.occupiedBy match
          case Some(pawn: Pawn) if pawn.color != color && pawn.justMovedTwoSquares =>
            moves += boardSquares(enPassantRow)(leftCol)
            println("En Passant is possible to the left")
          case _ =>
      if (board.isWithinBoard(rightCol, row)) then
        val rightSquare = boardSquares(row)(rightCol)
        rightSquare.occupiedBy match 
          case Some(pawn: Pawn) if pawn.color != color && pawn.justMovedTwoSquares =>
            moves += boardSquares(enPassantRow)(rightCol)
            println("En Passant is possible to the right")
          case _ =>
    moves.toList
  end calculatePossibleMoves

  override def possibleMoves(): List[Square] =
    if (isCaptured) then
      return List.empty[Square]
    calculatePossibleMoves()
  end possibleMoves

  override def attackSquares(): List[Square] =
    if (isCaptured) then
      return List.empty[Square]
    val attacks = scala.collection.mutable.ListBuffer[Square]()
    val (col, row) = square.getCoordinates
    val boardSquares = board.squares
    val direction = if (color == "white") -1 else 1
    val diagLeftCol = col - 1
    val diagLeftRow = row + direction
    if (board.isWithinBoard(diagLeftCol, diagLeftRow)) then
      attacks += boardSquares(diagLeftRow)(diagLeftCol)
    val diagRightCol = col + 1
    val diagRightRow = row + direction
    if (board.isWithinBoard(diagRightCol, diagRightRow)) then
      attacks += boardSquares(diagRightRow)(diagRightCol)
    attacks.toList
  end attackSquares

  def isEnPassantCapture(to: Square): Boolean =
    val (currentCol, currentRow) = square.getCoordinates
    val (targetCol, targetRow) = to.getCoordinates
    val direction = if (color == "white") -1 else 1
    val result = if (Math.abs(targetCol - currentCol) == 1 && targetRow - currentRow == direction) then
      val adjacentSquare = board.squares(currentRow)(targetCol)
      adjacentSquare.occupiedBy match
        case Some(pawn: Pawn) if pawn.color != color && pawn.justMovedTwoSquares =>
          true
        case _ => false
    else
      false
    result
  end isEnPassantCapture

  override def updatePossibleMoves(newMoves: List[Square]): Unit = currentPossibleMoves = newMoves

  override def moveTo(to: Square, isSimulation: Boolean = false): Unit =
    val fromRow = square.getCoordinates._2
    if (!isSimulation && isEnPassantCapture(to)) then
      val capturedPawnSquare = board.squares(fromRow)(to.getCoordinates._1)
      capturedPawnSquare.occupiedBy.foreach(_.capture())
      capturedPawnSquare.occupiedBy = None
    super.moveTo(to, isSimulation)
    if (!isSimulation) then
      val toRow = to.getCoordinates._2
      justMovedTwoSquares = Math.abs(toRow - fromRow) == 2
  end moveTo
}
