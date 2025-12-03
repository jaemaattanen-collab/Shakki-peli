package chess.analysis

import chess.board.Board
import chess.pieces._
import chess.controllers.GameController

/**
 * Generates FEN (Forsyth-Edwards Notation) strings from board positions
 * FEN format: position currentPlayer castling enPassant halfmove fullmove
 * Example: "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
 */
object FENGenerator {
  
  /**
   * Generate FEN string from current board state
   */
  def generateFEN(board: Board, controller: GameController): String = {
    val position = generatePositionPart(board)
    val activeColor = if (controller.currentPlayer == "white") "w" else "b"
    val castling = generateCastlingPart(board)
    val enPassant = generateEnPassantPart(board)
    val halfmove = controller.halfmoveClock.toString
    val fullmove = controller.fullmoveNumber.toString
    
    s"$position $activeColor $castling $enPassant $halfmove $fullmove"
  }
  
  /**
   * Generate the position part (piece placement)
   */
  private def generatePositionPart(board: Board): String = {
    val ranks = for (row <- 0 until 8) yield {
      var emptyCount = 0
      val rankStr = new StringBuilder
      
      for (col <- 0 until 8) {
        val square = board.squares(row)(col)
        square.occupiedBy match {
          case Some(piece) if !piece.isCaptured =>
            if (emptyCount > 0) {
              rankStr.append(emptyCount)
              emptyCount = 0
            }
            rankStr.append(pieceToFEN(piece))
          case _ =>
            emptyCount += 1
        }
      }
      
      if (emptyCount > 0) {
        rankStr.append(emptyCount)
      }
      
      rankStr.toString
    }
    
    ranks.mkString("/")
  }
  
  /**
   * Convert piece to FEN character
   */
  private def pieceToFEN(piece: Piece): Char = {
    val symbol = piece match {
      case _: Pawn   => 'p'
      case _: Knight => 'n'
      case _: Bishop => 'b'
      case _: Rook   => 'r'
      case _: Queen  => 'q'
      case _: King   => 'k'
      case _ => '?'
    }
    
    if (piece.color == "white") symbol.toUpper else symbol
  }
  
  /**
   * Generate castling rights part
   */
  private def generateCastlingPart(board: Board): String = {
    var castling = ""
    
    // Check white king and rooks
    val whiteKing = findKing(board, "white")
    val blackKing = findKing(board, "black")
    
    whiteKing.foreach { king =>
      if (!king.hasMoved) {
        // Check kingside rook (h1)
        board.squares(7)(7).occupiedBy match {
          case Some(rook: Rook) if rook.color == "white" && !rook.hasMoved =>
            castling += "K"
          case _ =>
        }
        
        // Check queenside rook (a1)
        board.squares(7)(0).occupiedBy match {
          case Some(rook: Rook) if rook.color == "white" && !rook.hasMoved =>
            castling += "Q"
          case _ =>
        }
      }
    }
    
    blackKing.foreach { king =>
      if (!king.hasMoved) {
        // Check kingside rook (h8)
        board.squares(0)(7).occupiedBy match {
          case Some(rook: Rook) if rook.color == "black" && !rook.hasMoved =>
            castling += "k"
          case _ =>
        }
        
        // Check queenside rook (a8)
        board.squares(0)(0).occupiedBy match {
          case Some(rook: Rook) if rook.color == "black" && !rook.hasMoved =>
            castling += "q"
          case _ =>
        }
      }
    }
    
    if (castling.isEmpty) "-" else castling
  }
  
  /**
   * Generate en passant target square
   */
  private def generateEnPassantPart(board: Board): String = {
    board.enPassantTarget match {
      case Some(square) => square.name
      case None => "-"
    }
  }
  
  /**
   * Find king piece on the board
   */
  private def findKing(board: Board, color: String): Option[King] = {
    board.squares.flatten
      .flatMap(_.occupiedBy)
      .collectFirst {
        case king: King if king.color == color && !king.isCaptured => king
      }
  }
  
  /**
   * Parse FEN string to extract components (for debugging/testing)
   */
  def parseFEN(fen: String): Map[String, String] = {
    val parts = fen.split(" ")
    Map(
      "position" -> parts.lift(0).getOrElse(""),
      "activeColor" -> parts.lift(1).getOrElse("w"),
      "castling" -> parts.lift(2).getOrElse("-"),
      "enPassant" -> parts.lift(3).getOrElse("-"),
      "halfmove" -> parts.lift(4).getOrElse("0"),
      "fullmove" -> parts.lift(5).getOrElse("1")
    )
  }
  
  /**
   * Setup a board from a FEN string.
   * Returns Right(Unit) on success, Left(errorMessage) on failure.
   */
  def setupBoardFromFEN(board: Board, fen: String): Either[String, Unit] = {
    try {
      val parts = fen.split(" ")
      if (parts.length < 1) return Left("Invalid FEN: empty string")
      
      val position = parts(0)
      val ranks = position.split("/")
      if (ranks.length != 8) return Left(s"Invalid FEN: expected 8 ranks, got ${ranks.length}")
      
      // Clear board first
      for (row <- 0 until 8; col <- 0 until 8) {
        board.squares(row)(col).occupiedBy = None
      }
      
      // Parse each rank and place pieces
      for ((rankStr, row) <- ranks.zipWithIndex) {
        var col = 0
        for (c <- rankStr) {
          if (c.isDigit) {
            col += c.asDigit
          } else {
            if (col < 8) {
              val piece = fenCharToPiece(c, row, col, board)
              piece.foreach(p => board.squares(row)(col).occupiedBy = Some(p))
              col += 1
            }
          }
        }
      }
      
      Right(())
    } catch {
      case e: Exception => Left(s"FEN parsing error: ${e.getMessage}")
    }
  }
  
  /**
   * Convert FEN character to a piece instance
   */
  private def fenCharToPiece(c: Char, row: Int, col: Int, board: Board): Option[Piece] = {
    val color = if (c.isUpper) "white" else "black"
    val square = board.squares(row)(col)
    
    c.toLower match {
      case 'k' => Some(new King(color, square, board))
      case 'q' => Some(new Queen(color, square, board))
      case 'r' => Some(new Rook(color, square, board))
      case 'b' => Some(new Bishop(color, square, board))
      case 'n' => Some(new Knight(color, square, board))
      case 'p' => Some(new Pawn(color, square, board))
      case _ => None
    }
  }
}
