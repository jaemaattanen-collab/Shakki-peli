package chess.pgn

import chess.types.PieceType

/**
 * Core PGN data structures for representing chess games with full PGN support
 */

/**
 * Represents a complete PGN game with metadata and move tree
 */
case class PGNGame(
  headers: Map[String, String],
  moves: List[PGNMove],
  result: Option[String],
  variations: List[PGNVariation] = List.empty
)

/**
 * Represents a single move in PGN notation with optional variations and comments
 */
case class PGNMove(
  moveNumber: Int,
  whiteMove: Option[String],
  blackMove: Option[String],
  whiteComment: Option[String] = None,
  blackComment: Option[String] = None,
  variations: List[PGNVariation] = List.empty,
  annotations: List[PGNAnnotation] = List.empty
)

/**
 * Represents a variation (alternative move sequence) in PGN
 */
case class PGNVariation(
  startingMoveNumber: Int,
  startingColor: String,
  moves: List[PGNMove],
  comment: Option[String] = None,
  originMoveNumber: Option[Int] = None,
  originColor: Option[String] = None,
  leadingVariations: List[PGNVariation] = List.empty
)

/**
 * PGN annotations (NAG - Numeric Annotation Glyph)
 */
case class PGNAnnotation(
  nag: String,
  color: String,
  position: String = "after"
)

/**
 * Parsed move data for internal representation
 */
case class ParsedMove(
  piece: String,
  fromSquare: Option[String],
  toSquare: String,
  isCapture: Boolean,
  isCheck: Boolean,
  isCheckmate: Boolean,
  isCastling: Boolean,
  castlingType: Option[String],
  promotion: Option[PieceType],
  annotation: Option[String]
)

/**
 * PGN parsing result
 */
case class PGNParsingResult(
  game: Option[PGNGame],
  errors: List[String] = List.empty,
  warnings: List[String] = List.empty
)

/**
 * PGN export options
 */
case class PGNExportOptions(
  includeComments: Boolean = true,
  includeVariations: Boolean = true,
  includeAnnotations: Boolean = true,
  maxLineLength: Int = 80,
  includeHeaders: Boolean = true
)
