package chess.state

import chess.board.{Board, Square}
import chess.pieces.Piece
import chess.controllers.GameController

class Position(val piecePlacement: Array[Array[Option[Piece]]], val sideToMove: String, val castlingRights: CastlingRights, val enPassantTarget: Option[Square], val halfmoveClock: Int, val fullmoveNumber: Int) {
  lazy val positionKey: Int = {
    val sb = new StringBuilder
    for (row <- piecePlacement) {
      for (sq <- row) {
        sq match 
          case Some(piece) =>
            sb.append(piece.color.head)
            sb.append(piece.pieceType.toString.head)
          case None => sb.append(".")
      }
    }
    sb.append(sideToMove.head)
    sb.append(castlingRights.toString)
    enPassantTarget.foreach(s => sb.append(s.name))
    sb.toString.hashCode
  }
  override def equals(other: Any): Boolean = 
    other match 
      case that: Position => this.positionKey == that.positionKey
      case _ => false

  override def hashCode(): Int = positionKey}

case class CastlingRights(whiteKingSide: Boolean, whiteQueenSide: Boolean, blackKingSide: Boolean, blackQueenSide: Boolean) {
  override def toString: String = {
    val rights = new StringBuilder
    if (whiteKingSide) rights.append("K")
    if (whiteQueenSide) rights.append("Q")
    if (blackKingSide) rights.append("k")
    if (blackQueenSide) rights.append("q")
    if (rights.isEmpty) "-" else rights.toString
  }
}

object Position {
  def fromGameState(board: Board, controller: GameController): Position = 
    val piecePlacement = Array.ofDim[Option[Piece]](8,8)
    for (r <- 0 until 8; c <- 0 until 8) {
      piecePlacement(r)(c) = board.squares(r)(c).occupiedBy
    }

    val castlingRights = controller.castlingRights
    val enPassantTarget = board.enPassantTarget
    val halfmoveClock = controller.halfmoveClock
    val fullmoveNumber = controller.fullmoveNumber

    new Position(
      piecePlacement = piecePlacement,
      sideToMove = controller.currentPlayer,
      castlingRights = castlingRights,
      enPassantTarget = enPassantTarget,
      halfmoveClock = halfmoveClock,
      fullmoveNumber = fullmoveNumber
    )
  
}