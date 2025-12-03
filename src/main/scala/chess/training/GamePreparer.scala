package chess.training

import chess.board.Board
import chess.controllers.{GameController, Move}
import chess.pgn.{PGNGame, PGNParser, SANParser}
import chess.analysis.FENGenerator
import chess.pieces.{Pawn => PawnPiece, Piece => ChessPiece}
import chess.board.Square
import chess.types.PieceType
import chess.state.CastlingRights

import java.io.File
import scala.collection.mutable.ListBuffer

/**
 * Prepares PGN games for analysis by replaying them on a temporary board
 * and collecting Move objects and FEN positions.
 * 
 * This class creates temporary game state to avoid modifying the main game.
 */
class GamePreparer {
  
  /**
   * Load and prepare games from multiple PGN files.
   * 
   * @param files List of PGN files to load
   * @return List of prepared games ready for analysis
   */
  def prepareGamesFromFiles(files: Seq[File]): Seq[AnalyzableGame] = {
    var gameIndex = 0
    files.flatMap { file =>
      val games = loadAndPrepareFile(file, gameIndex)
      gameIndex += games.length
      games
    }
  }
  
  /**
   * Load and prepare games from a single PGN file.
   * 
   * @param file The PGN file
   * @param startIndex Starting index for game numbering
   * @return List of prepared games
   */
  def loadAndPrepareFile(file: File, startIndex: Int = 0): Seq[AnalyzableGame] = {
    try {
      val content = scala.io.Source.fromFile(file).mkString
      prepareGamesFromString(content, startIndex)
    } catch {
      case e: Exception =>
        println(s"Error loading ${file.getName}: ${e.getMessage}")
        Seq.empty
    }
  }
  
  /**
   * Prepare games from PGN string content.
   * 
   * @param pgnContent The PGN content (may contain multiple games)
   * @param startIndex Starting index for game numbering
   * @return List of prepared games
   */
  def prepareGamesFromString(pgnContent: String, startIndex: Int = 0): Seq[AnalyzableGame] = {
    val pgnGames = PGNParser.parseMultipleGames(pgnContent)
    
    pgnGames.zipWithIndex.flatMap { case (pgnGame, idx) =>
      prepareGame(pgnGame, startIndex + idx)
    }
  }
  
  /**
   * Prepare a single PGN game for analysis.
   * Creates a temporary board and controller, replays the game,
   * and collects Move objects and FEN positions.
   * 
   * @param pgnGame The parsed PGN game
   * @param gameIndex Index for this game
   * @return Prepared game if successful, None if replay failed
   */
  def prepareGame(pgnGame: PGNGame, gameIndex: Int): Option[AnalyzableGame] = {
    // Create temporary game state
    val tempController = new GameController()
    val tempBoard = new Board(tempController)
    tempController.board = Some(tempBoard)
    
    // Reset to starting position
    resetState(tempController, tempBoard)
    
    // Collect FENs and moves
    val fens = ListBuffer[String]()
    val moves = ListBuffer[Move]()
    
    // Add starting position FEN
    fens += FENGenerator.generateFEN(tempBoard, tempController)
    
    // Build SAN entries from PGN moves
    val sanEntries = buildSanEntries(pgnGame.moves)
    tempController.updateAllPossibleMoves()
    
    var plyIndex = 0
    var error: Option[String] = None
    
    while (error.isEmpty && plyIndex < sanEntries.length) {
      val (san, isWhiteMove, moveNumber, colourLabel) = sanEntries(plyIndex)
      
      SANParser.parseMove(san, isWhiteMove, tempController, tempBoard) match {
        case Left(message) =>
          error = Some(s"$message (move $moveNumber $colourLabel)")
          
        case Right(parsed) =>
          tempController.currentPlayer = colourLabel
          val originPiece = parsed.from.occupiedBy
          
          originPiece match {
            case None =>
              error = Some(s"No piece on ${parsed.from.name} for '$san' (move $moveNumber $colourLabel)")
              
            case Some(piece) =>
              // Store move count before action
              val moveCountBefore = tempController.moveHistoryManager.getMainlineMoves.length
              
              // Set up promotion handler if needed
              val previousPromotionHandler = tempController.onPawnPromotion
              tempController.onPawnPromotion = promotionCallback(parsed.promotion)
              
              val moved = tempController.handleAction(piece, parsed.to)
              tempController.onPawnPromotion = previousPromotionHandler
              
              if (!moved) {
                error = Some(s"Failed to apply '$san' (move $moveNumber $colourLabel)")
              } else {
                // Collect the Move object that was just created
                val currentMoves = tempController.moveHistoryManager.getMainlineMoves
                if (currentMoves.length > moveCountBefore) {
                  moves += currentMoves.last
                }
                
                // Collect FEN after the move
                fens += FENGenerator.generateFEN(tempBoard, tempController)
                
                tempController.updateAllPossibleMoves()
              }
          }
      }
      plyIndex += 1
    }
    
    error match {
      case Some(msg) =>
        println(s"Game preparation failed for game ${gameIndex + 1}: $msg")
        None
        
      case None =>
        Some(AnalyzableGame(
          pgnGame = pgnGame,
          moves = moves.toList,
          fens = fens.toList,
          gameIndex = gameIndex
        ))
    }
  }
  
  private def promotionCallback(
    promotion: Option[PieceType]
  ): Option[(PawnPiece, Square, Square, Option[ChessPiece]) => PieceType] = {
    promotion.map { promotionType =>
      (_: PawnPiece, _: Square, _: Square, _: Option[ChessPiece]) => promotionType
    }
  }
  
  private def buildSanEntries(moves: List[chess.pgn.PGNMove]): Vector[(String, Boolean, Int, String)] = {
    val buffer = scala.collection.mutable.ArrayBuffer[(String, Boolean, Int, String)]()
    moves.foreach { move =>
      move.whiteMove.map(_.trim).filter(_.nonEmpty).foreach { san =>
        buffer += ((san, true, move.moveNumber, "white"))
      }
      move.blackMove.map(_.trim).filter(_.nonEmpty).foreach { san =>
        buffer += ((san, false, move.moveNumber, "black"))
      }
    }
    buffer.toVector
  }
  
  private def resetState(controller: GameController, board: Board): Unit = {
    controller.resetGame()
    controller.currentPlayer = "white"
    controller.winner = None
    controller.halfmoveClock = 0
    controller.fullmoveNumber = 0
    controller.totalMoves = 0
    controller.enPassantTarget = None
    controller.castlingRights = CastlingRights(
      whiteKingSide = true, 
      whiteQueenSide = true, 
      blackKingSide = true, 
      blackQueenSide = true
    )
    controller.board = Some(board)
    board.resetBoard()
    controller.deselectPiece()
    controller.onPawnPromotion = None
  }
}
