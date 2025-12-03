package chess.pgn

import chess.types.PieceType

/**
 * Core PGN data structures for representing chess games with full PGN support
 */

/**
 * Represents a complete PGN game with metadata and move tree
 */
case class PGNGame(
  headers: Map[String, String],  // PGN headers like Event, Site, Date, etc.
  moves: List[PGNMove],          // Main line moves
  result: Option[String],        // Game result: "1-0", "0-1", "1/2-1/2", "*"
  variations: List[PGNVariation] = List.empty  // Alternative move sequences from the starting position
)

/**
 * Represents a single move in PGN notation with optional variations and comments
 */
case class PGNMove(
  moveNumber: Int,                    // Move number (1, 2, 3, ...)
  whiteMove: Option[String],          // White's move in algebraic notation
  blackMove: Option[String],          // Black's move in algebraic notation
  whiteComment: Option[String] = None, // Comment after white's move
  blackComment: Option[String] = None, // Comment after black's move
  variations: List[PGNVariation] = List.empty, // Variations from this position
  annotations: List[PGNAnnotation] = List.empty // NAGs and other annotations
)

/**
 * Represents a variation (alternative move sequence) in PGN
 */
case class PGNVariation(
  startingMoveNumber: Int,        // Move number where variation starts
  startingColor: String,          // "white" or "black" - color to move at variation start
  moves: List[PGNMove],           // The variation moves
  comment: Option[String] = None, // Comment for the variation line
  originMoveNumber: Option[Int] = None, // Move number that spawned this variation (if attached to a move)
  originColor: Option[String] = None,   // Color of the move that spawned this variation
  leadingVariations: List[PGNVariation] = List.empty // Variations that appear before the first move in this line
)

/**
 * PGN annotations (NAG - Numeric Annotation Glyph)
 */
case class PGNAnnotation(
  nag: String,                 // Numeric annotation glyph like "!", "?", "!!", etc.
  color: String,               // "white" or "black" - which half-move this annotation refers to
  position: String = "after"   // "before" or "after" the move (defaults to after)
)

/**
 * Parsed move data for internal representation
 */
case class ParsedMove(
  piece: String,               // Piece symbol (N, B, R, Q, K, or "" for pawn)
  fromSquare: Option[String],  // Origin square (e.g., "e2") for disambiguation
  toSquare: String,            // Destination square (e.g., "e4")
  isCapture: Boolean,          // Whether this is a capture
  isCheck: Boolean,            // Whether this move gives check
  isCheckmate: Boolean,        // Whether this move gives checkmate
  isCastling: Boolean,         // Whether this is castling
  castlingType: Option[String], // "kingside" or "queenside" for castling
  promotion: Option[PieceType], // Promotion piece type
  annotation: Option[String]   // Move annotation (!, ?, !!, ??, !?, ?!)
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
  maxLineLength: Int = 80,  // Maximum line length for formatting
  includeHeaders: Boolean = true
)