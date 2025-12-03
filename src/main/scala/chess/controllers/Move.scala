// src/main/scala/chess/controllers/Move.scala

package chess.controllers
import chess.types._
import chess.pieces.Piece
import chess.board.Square

case class Move(
  piece: Piece,
  from: Square,
  to: Square,
  capturedPiece: Option[Piece],
  promotedPieceType: Option[PieceType] = None,
  tob: Long = -1L,
  parentTob: Long = -1L,
  halfmoveDistanceFromStart: Int = 0,
  notation: Option[String] = None,
  isVariation: Boolean = false,       // True if this move is part of a variation (not mainline)
  variationRootTob: Option[Long] = None, // Points to the first move of this variation branch
  // For sacrifice detection: how many pieces see the target square BEFORE the move
  targetSquareDefenders: Int = 0,     // Own pieces defending the target square (excluding the moving piece)
  targetSquareAttackers: Int = 0      // Opponent pieces attacking the target square
)