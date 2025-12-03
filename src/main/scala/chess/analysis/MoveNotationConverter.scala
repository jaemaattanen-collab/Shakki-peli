package chess.analysis

import chess.board.Board
import chess.pieces._

/**
 * Converts UCI notation (e.g., "e2e4") to standard chess notation (e.g., "e4")
 */
object MoveNotationConverter {
  
  /**
   * Convert a UCI move to chess notation
   * @param uciMove UCI format move (e.g., "e2e4", "e7e8q")
   * @param board Current board state
   * @return Chess notation (e.g., "e4", "Nf3", "O-O", "exd5")
   */
  def uciToChessNotation(uciMove: String, board: Board): String = {
    if (uciMove.length < 4) return uciMove
    
    val fromSquare = uciMove.substring(0, 2)
    val toSquare = uciMove.substring(2, 4)
    val promotion = if (uciMove.length > 4) Some(uciMove.charAt(4)) else None
    
    // Get piece at from square
    val piece = board.getSquareByName(fromSquare).flatMap(_.occupiedBy)
    
    piece match {
      case Some(p: King) =>
        // Check for castling
        if (fromSquare == "e1" && toSquare == "g1") "O-O"
        else if (fromSquare == "e1" && toSquare == "c1") "O-O-O"
        else if (fromSquare == "e8" && toSquare == "g8") "O-O"
        else if (fromSquare == "e8" && toSquare == "c8") "O-O-O"
        else "K" + toSquare
        
      case Some(p: Pawn) =>
        val isCapture = board.getSquareByName(toSquare).exists(_.occupiedBy.isDefined) ||
                       (toSquare == board.enPassantTarget.map(_.name).getOrElse(""))
        promotion match {
          case Some(promo) =>
            val promoChar = promo.toUpper match {
              case 'Q' => "Q"
              case 'R' => "R"
              case 'B' => "B"
              case 'N' => "N"
              case _ => ""
            }
            if (isCapture) s"${fromSquare.charAt(0)}x$toSquare=$promoChar"
            else s"$toSquare=$promoChar"
          case None =>
            if (isCapture) s"${fromSquare.charAt(0)}x$toSquare"
            else toSquare
        }
        
      case Some(piece: Piece) =>
        val pieceChar = piece match {
          case _: Knight => "N"
          case _: Bishop => "B"
          case _: Rook => "R"
          case _: Queen => "Q"
          case _ => ""
        }
        val isCapture = board.getSquareByName(toSquare).exists(_.occupiedBy.isDefined)
        val captureChar = if (isCapture) "x" else ""
        
        // Check if we need disambiguation (multiple pieces of same type can move to same square)
        val disambiguation = getDisambiguation(piece, fromSquare, toSquare, board)
        
        pieceChar + disambiguation + captureChar + toSquare
        
      case None => toSquare  // Fallback
    }
  }
  
  /**
   * Get disambiguation string if needed (e.g., "Nbd7" when both knights can go to d7)
   */
  private def getDisambiguation(piece: Piece, fromSquare: String, toSquare: String, board: Board): String = {
    // For now, simplified - would need full move generation to be completely accurate
    // Check if file or rank disambiguation is needed
    val fromFile = fromSquare.charAt(0)
    val fromRank = fromSquare.charAt(1)
    
    // Find other pieces of same type that could move to toSquare
    val similarPieces = board.squares.flatten.flatMap(_.occupiedBy).filter { p =>
      p != piece && 
      p.getClass == piece.getClass && 
      p.color == piece.color &&
      p.possibleMoves().exists(_.name == toSquare)
    }
    
    if (similarPieces.isEmpty) {
      ""  // No disambiguation needed
    } else {
      // Check if file disambiguation is enough
      val sameFile = similarPieces.exists(p => p.square.name.charAt(0) == fromFile)
      val sameRank = similarPieces.exists(p => p.square.name.charAt(1) == fromRank)
      
      if (!sameFile) fromFile.toString
      else if (!sameRank) fromRank.toString
      else fromSquare  // Full square name needed
    }
  }
}
