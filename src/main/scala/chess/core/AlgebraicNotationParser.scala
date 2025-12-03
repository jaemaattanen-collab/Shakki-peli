package chess.core

import chess.board.{Board, Square}
import chess.pieces.Piece
import chess.types.PieceType
import chess.controllers.GameController

/**
 * Parser for algebraic chess notation (SAN - Standard Algebraic Notation)
 * Converts notation like "Nf3", "exd5", "O-O", "Bb5+" into board moves
 * Simply executes the move on the board without creating Move objects
 */
object AlgebraicNotationParser {
  
  /**
   * Parse algebraic notation and execute the move on the board
   * Returns true if successful, false if parsing fails
   */
  def parseAndExecute(notation: String, board: Board, controller: GameController, isWhite: Boolean): Boolean = {
    val cleanNotation = notation.replaceAll("[+#!?]", "").trim() // Remove check/mate/annotation symbols
    
    // Handle castling
    if (cleanNotation == "O-O" || cleanNotation == "0-0") {
      return handleCastling(board, controller, kingside = true, isWhite)
    }
    if (cleanNotation == "O-O-O" || cleanNotation == "0-0-0") {
      return handleCastling(board, controller, kingside = false, isWhite)
    }
    
    // Parse regular move
    parseRegularMove(cleanNotation, board, controller, isWhite)
  }
  
  private def handleCastling(board: Board, controller: GameController, kingside: Boolean, isWhite: Boolean): Boolean = {
    val rank = if (isWhite) 7 else 0
    val kingCol = 4
    val rookCol = if (kingside) 7 else 0
    val newKingCol = if (kingside) 6 else 2
    val newRookCol = if (kingside) 5 else 3
    
    val kingSquare = board.squares(rank)(kingCol)
    val rookSquare = board.squares(rank)(rookCol)
    val targetSquare = board.squares(rank)(newKingCol)
    
    // Verify king and rook are present
    if (kingSquare.occupiedBy.exists(_.pieceType == PieceType.King) &&
        rookSquare.occupiedBy.exists(_.pieceType == PieceType.Rook)) {
      
      // Execute castling
      val king = kingSquare.occupiedBy.get
      val rook = rookSquare.occupiedBy.get
      
      // Move king
      targetSquare.occupiedBy = Some(king)
      kingSquare.occupiedBy = None
      
      // Move rook
      board.squares(rank)(newRookCol).occupiedBy = Some(rook)
      rookSquare.occupiedBy = None
      
      return true
    }
    
    false
  }
  
  private def parseRegularMove(notation: String, board: Board, controller: GameController, isWhite: Boolean): Boolean = {
    // Determine piece type
    val pieceType = notation.charAt(0) match {
      case 'N' => PieceType.Knight
      case 'B' => PieceType.Bishop
      case 'R' => PieceType.Rook
      case 'Q' => PieceType.Queen
      case 'K' => PieceType.King
      case _ => PieceType.Pawn // No prefix means pawn
    }
    
    // Extract notation without piece prefix
    val moveNotation = if (pieceType == PieceType.Pawn) notation else notation.substring(1)
    
    // Check for capture
    val isCapture = moveNotation.contains('x')
    
    // Extract destination square (last 2 characters, e.g., "e4", "d5")
    val destination = moveNotation.takeRight(2)
    if (destination.length != 2) {
      println(s"ERROR: Invalid destination in notation: $notation")
      return false
    }
    
    val destFile = destination.charAt(0) - 'a'
    val destRank = 8 - (destination.charAt(1) - '0')
    
    if (destFile < 0 || destFile > 7 || destRank < 0 || destRank > 7) {
      println(s"ERROR: Invalid destination coordinates: $destination")
      return false
    }
    
    val destSquare = board.squares(destRank)(destFile)
    
    // Parse disambiguation (e.g., "Nbd7" means knight from b-file, "N5f3" means knight from 5th rank)
    val disambiguationPart = moveNotation.substring(0, moveNotation.length - 2).replace("x", "")
    
    // Find all pieces of the correct type and color that can move to destination
    val candidates = findCandidatePieces(board, pieceType, isWhite, destSquare, disambiguationPart, isCapture)
    
    if (candidates.isEmpty) {
      println(s"ERROR: No valid piece found for notation: $notation")
      return false
    }
    
    if (candidates.size > 1) {
      println(s"WARNING: Multiple candidates for notation: $notation (using first)")
    }
    
    val (fromSquare, piece) = candidates.head
    
    // Execute the move
    destSquare.occupiedBy = Some(piece)
    fromSquare.occupiedBy = None
    
    // Handle pawn promotion (simple - always promote to queen)
    if (pieceType == PieceType.Pawn && (destRank == 0 || destRank == 7)) {
      val promotedPiece = new chess.pieces.Queen(piece.color, destSquare, board)
      destSquare.occupiedBy = Some(promotedPiece)
    }
    
    true
  }
  
  private def findCandidatePieces(
    board: Board,
    pieceType: PieceType,
    isWhite: Boolean,
    destSquare: Square,
    disambiguation: String,
    isCapture: Boolean
  ): List[(Square, Piece)] = {
    val candidates = scala.collection.mutable.ListBuffer[(Square, Piece)]()
    val color = if (isWhite) "white" else "black"
    
    // Parse disambiguation hints
    val fileHint: Option[Int] = disambiguation.find(c => c >= 'a' && c <= 'h').map(c => c - 'a')
    val rankHint: Option[Int] = disambiguation.find(c => c >= '1' && c <= '8').map(c => 8 - (c - '0'))
    
    for {
      rank <- 0 until 8
      file <- 0 until 8
      square = board.squares(rank)(file)
      piece <- square.occupiedBy
      if piece.pieceType == pieceType && piece.color == color
    } {
      // Apply disambiguation filters
      if (fileHint.isDefined && file != fileHint.get) {
        // Skip - wrong file
      } else if (rankHint.isDefined && rank != rankHint.get) {
        // Skip - wrong rank
      } else if (canPieceReachSquare(board, square, destSquare, piece, isCapture)) {
        candidates += ((square, piece))
      }
    }
    
    candidates.toList
  }
  
  private def canPieceReachSquare(board: Board, fromSquare: Square, toSquare: Square, piece: Piece, isCapture: Boolean): Boolean = {
    val (fromFile, fromRank) = fromSquare.getCoordinates
    val (toFile, toRank) = toSquare.getCoordinates
    val fileDiff = toFile - fromFile
    val rankDiff = toRank - fromRank
    
    piece.pieceType match {
      case PieceType.Pawn =>
        val direction = if (piece.color == "white") -1 else 1
        val startRank = if (piece.color == "white") 6 else 1
        
        if (isCapture) {
          // Pawn capture: one square diagonally forward
          rankDiff == direction && math.abs(fileDiff) == 1
        } else {
          // Pawn advance: one or two squares forward
          fileDiff == 0 && (
            rankDiff == direction || 
            (fromRank == startRank && rankDiff == 2 * direction && !isPathBlocked(board, fromSquare, toSquare))
          )
        }
        
      case PieceType.Knight =>
        (math.abs(fileDiff) == 2 && math.abs(rankDiff) == 1) ||
        (math.abs(fileDiff) == 1 && math.abs(rankDiff) == 2)
        
      case PieceType.Bishop =>
        math.abs(fileDiff) == math.abs(rankDiff) && !isPathBlocked(board, fromSquare, toSquare)
        
      case PieceType.Rook =>
        (fileDiff == 0 || rankDiff == 0) && !isPathBlocked(board, fromSquare, toSquare)
        
      case PieceType.Queen =>
        (fileDiff == 0 || rankDiff == 0 || math.abs(fileDiff) == math.abs(rankDiff)) && 
        !isPathBlocked(board, fromSquare, toSquare)
        
      case PieceType.King =>
        math.abs(fileDiff) <= 1 && math.abs(rankDiff) <= 1
    }
  }
  
  private def isPathBlocked(board: Board, fromSquare: Square, toSquare: Square): Boolean = {
    val (fromFile, fromRank) = fromSquare.getCoordinates
    val (toFile, toRank) = toSquare.getCoordinates
    
    val fileDiff = toFile - fromFile
    val rankDiff = toRank - fromRank
    
    val fileStep = if (fileDiff == 0) 0 else fileDiff / math.abs(fileDiff)
    val rankStep = if (rankDiff == 0) 0 else rankDiff / math.abs(rankDiff)
    
    var currentFile = fromFile + fileStep
    var currentRank = fromRank + rankStep
    
    while (currentFile != toFile || currentRank != toRank) {
      if (board.squares(currentRank)(currentFile).occupiedBy.isDefined) {
        return true // Path is blocked
      }
      currentFile += fileStep
      currentRank += rankStep
    }
    
    false // Path is clear
  }
}
