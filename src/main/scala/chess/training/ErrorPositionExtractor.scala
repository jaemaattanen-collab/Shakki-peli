package chess.training

import chess.analysis.{GameAnalyzer, MoveAnalysis, MoveClassification}

import scala.concurrent.{Future, ExecutionContext}
import java.io.File

/**
 * Extracts error positions from analyzed games for training purposes.
 */
class ErrorPositionExtractor(gameAnalyzer: GameAnalyzer)(implicit ec: ExecutionContext) {
  
  private val gamePreparer = new GamePreparer()
  
  /**
   * Classifications that are considered "errors" for training.
   * Only Mistakes and Blunders - not Inaccuracies.
   */
  val errorClassifications: Set[MoveClassification] = Set(
    MoveClassification.Mistake,
    MoveClassification.Blunder
  )
  
  /**
   * Load and prepare games from multiple PGN files.
   * 
   * @param files List of PGN files to load
   * @return List of prepared games ready for analysis
   */
  def loadGames(files: Seq[File]): Seq[AnalyzableGame] = {
    gamePreparer.prepareGamesFromFiles(files)
  }
  
  /**
   * Load games from a single PGN file that may contain multiple games.
   * 
   * @param file PGN file to load
   * @return List of prepared games
   */
  def loadGamesFromFile(file: File): Seq[AnalyzableGame] = {
    gamePreparer.loadAndPrepareFile(file)
  }
  
  /**
   * Analyze a prepared game and extract error positions.
   * 
   * @param game The prepared game with moves and FENs
   * @param playerColor The color the player was playing ("white", "black", or "both")
   * @param depth Analysis depth for Stockfish
   * @param progressCallback Optional callback for progress updates
   * @return Future containing error positions found in the game
   */
  def extractErrorsFromGame(
    game: AnalyzableGame,
    playerColor: String = "both",
    depth: Int = 18,
    progressCallback: Option[Int => Unit] = None
  ): Future[Seq[ErrorPosition]] = {
    
    if (game.moves.isEmpty || game.fens.isEmpty) {
      return Future.successful(Seq.empty)
    }
    
    // Analyze the game
    val analysisFuture = progressCallback match {
      case Some(callback) =>
        gameAnalyzer.analyzeGameWithProgress(game.moves, game.fens, depth, callback)
      case None =>
        gameAnalyzer.analyzeGame(game.moves, game.fens, depth)
    }
    
    analysisFuture.map { analyses =>
      extractErrorPositionsFromAnalysis(analyses, game.fens, game.gameIndex, playerColor)
    }
  }
  
  /**
   * Extract error positions from already-analyzed moves.
   * 
   * @param analyses List of move analyses
   * @param fens List of FEN positions
   * @param gameIndex Index of the game
   * @param playerColor Which player's errors to extract
   * @return Error positions found
   */
  def extractErrorPositionsFromAnalysis(
    analyses: Seq[MoveAnalysis],
    fens: Seq[String],
    gameIndex: Int,
    playerColor: String = "both"
  ): Seq[ErrorPosition] = {
    
    analyses.zipWithIndex.flatMap { case (analysis, index) =>
      val isWhiteMove = analysis.isWhiteMove
      val moveColor = if (isWhiteMove) "white" else "black"
      
      // Check if we should include this player's moves
      val includePlayer = playerColor == "both" || playerColor == moveColor
      
      // Check if this is an error classification
      val isError = errorClassifications.contains(analysis.classification)
      
      if (includePlayer && isError && analysis.bestMove.isDefined) {
        // Get the FEN before the move was played
        val fenBefore = if (index < fens.length) fens(index) else ""
        
        // Create the played move notation (UCI format for consistency)
        val playedMove = {
          val fromSquare = analysis.move.from.name.toLowerCase
          val toSquare = analysis.move.to.name.toLowerCase
          s"$fromSquare$toSquare"
        }
        
        // Get the previous move (opponent's move that led to this position)
        val previousMove: Option[String] = if (index > 0) {
          val prevAnalysis = analyses(index - 1)
          val fromSquare = prevAnalysis.move.from.name.toLowerCase
          val toSquare = prevAnalysis.move.to.name.toLowerCase
          Some(s"$fromSquare$toSquare")
        } else {
          None
        }
        
        // Calculate move number (1-indexed, full moves)
        val moveNumber = (index / 2) + 1
        
        Some(ErrorPosition(
          fen = fenBefore,
          playerColor = moveColor,
          playedMove = playedMove,
          bestMove = analysis.bestMove.getOrElse(""),
          classification = analysis.classification,
          evalBefore = analysis.evalBefore,
          evalAfter = analysis.evalAfter,
          gameIndex = gameIndex,
          moveNumber = moveNumber,
          previousMove = previousMove
        ))
      } else {
        None
      }
    }
  }
  
  /**
   * Analyze multiple games and extract all error positions.
   * 
   * @param games List of prepared games to analyze
   * @param playerColor Which player's errors to extract
   * @param depth Analysis depth
   * @param overallProgress Callback for overall progress (game index, total games)
   * @param moveProgress Callback for per-move progress within current game
   * @return Future with all error positions
   */
  def extractErrorsFromMultipleGames(
    games: Seq[AnalyzableGame],
    playerColor: String = "both",
    depth: Int = 18,
    overallProgress: Option[(Int, Int) => Unit] = None,
    moveProgress: Option[Int => Unit] = None
  ): Future[Seq[ErrorPosition]] = {
    
    def processGames(
      remaining: Seq[AnalyzableGame],
      accumulated: Seq[ErrorPosition]
    ): Future[Seq[ErrorPosition]] = {
      remaining match {
        case Seq() =>
          Future.successful(accumulated)
          
        case game +: rest =>
          overallProgress.foreach(_(game.gameIndex + 1, games.length))
          
          extractErrorsFromGame(game, playerColor, depth, moveProgress).flatMap { errors =>
            processGames(rest, accumulated ++ errors)
          }
      }
    }
    
    processGames(games, Seq.empty)
  }
  
  /**
   * Determine player color from game headers.
   * 
   * @param game The prepared game
   * @param playerName The player's name/username to look for
   * @return "white", "black", or "both" if player not found
   */
  def detectPlayerColor(game: AnalyzableGame, playerName: String): String = {
    game.playerColor(playerName).getOrElse("both")
  }
}

/**
 * Helper object for creating ErrorPositionExtractor
 */
object ErrorPositionExtractor {
  def apply(gameAnalyzer: GameAnalyzer)(implicit ec: ExecutionContext): ErrorPositionExtractor = {
    new ErrorPositionExtractor(gameAnalyzer)
  }
}
