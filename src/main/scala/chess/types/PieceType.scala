// src/main/scala/chess/types/PieceType.scala

package chess.types

/**
 * Enumeration of all possible piece types in chess.
 */
sealed trait PieceType

object PieceType {
  case object King extends PieceType
  case object Queen extends PieceType
  case object Rook extends PieceType
  case object Bishop extends PieceType
  case object Knight extends PieceType
  case object Pawn extends PieceType
}
