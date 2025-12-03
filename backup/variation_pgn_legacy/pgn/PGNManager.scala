package chess.pgn

import chess.controllers.GameController
import chess.board.Board
import java.io.{File, PrintWriter}
import scala.annotation.unused

/**
 * Manages PGN load/save operations while synchronising the live game state,
 * headers and on-the-fly variation drafts created in the UI.
 */
class PGNManager(
  gameController: GameController,
  @unused board: Board,
  liveVariationRepository: Option[LiveVariationRepository] = None
) {

  // Store parsed PGN data
  private var currentPGN: Option[PGNGame] = None
  /**
   * Load a PGN game from file
   */
  def loadPGN(file: File): Either[String, PGNGame] = {
    try {
      val source = scala.io.Source.fromFile(file)
      val pgnText = source.mkString
      source.close()

      val result = PGNParser.parsePGN(pgnText)
      result.game match {
        case Some(game) =>
          currentPGN = Some(game)
          Right(game)
        case None =>
          Left(s"Failed to parse PGN: ${result.errors.mkString(", ")}")
      }
    } catch {
      case e: Exception =>
        Left(s"Error reading PGN file: ${e.getMessage}")
    }
  }

  /**
   * Load PGN from string
   */
  def loadPGNFromString(pgnText: String): Either[String, PGNGame] = {
    val result = PGNParser.parsePGN(pgnText)
    result.game match {
      case Some(game) =>
        currentPGN = Some(game)
        Right(game)
      case None =>
        Left(s"Failed to parse PGN: ${result.errors.mkString(", ")}")
    }
  }

  /**
   * Save current game as PGN file
   */
  def savePGN(file: File, customHeaders: Map[String, String] = Map.empty): Either[String, Unit] = {
    try {
      val gameForExport = buildCurrentGame(customHeaders)
      val pgnText = PGNExporter.exportPGNGame(gameForExport)

      val writer = new PrintWriter(file)
      writer.write(pgnText)
      writer.close()

      currentPGN = Some(gameForExport)
      Right(())
    } catch {
      case e: Exception =>
        Left(s"Error saving PGN file: ${e.getMessage}")
    }
  }

  /**
   * Get current PGN as formatted string
   */
  def getCurrentPGNString: String = {
    PGNExporter.exportPGNGame(buildCurrentGame())
  }

  /**
   * Get current PGN game data
   */
  def getCurrentPGN: Option[PGNGame] = 
    currentPGN

  /**
   * Get PGN headers
   */
  def getHeaders: Map[String, String] = {
    currentPGN.map(_.headers).getOrElse(Map.empty)
  }

  /**
   * Set PGN headers
   */
  def setHeaders(headers: Map[String, String]): Unit = {
    currentPGN = currentPGN.map(_.copy(headers = headers))
  }

  /**
   * Update a specific header
   */
  def updateHeader(key: String, value: String): Unit = {
    val updatedHeaders = getHeaders + (key -> value)
    setHeaders(updatedHeaders)
  }

  /**
   * Get game result
   */
  def getResult: Option[String] = {
    currentPGN.flatMap(_.result)
  }

  /**
   * Set game result
   */
  def setResult(result: String): Unit = {
    currentPGN = currentPGN.map(_.copy(result = Some(result)))
  }

  /**
   * Get all moves in PGN format
   */
  def getPGNMoves: List[PGNMove] = {
    currentPGN.map(_.moves).getOrElse(List.empty)
  }

  /**
   * Convert internal move history to PGN moves
   */
  def convertToPGNMoves: List[PGNMove] = {
    val notations = gameController.moveHistoryManager.getMoveNotations
    if notations.isEmpty then List.empty
    else PGNMoveBuilder.fromHalfMoveNotations(notations)
  }

  /**
   * Validate PGN format
   */
  def validatePGN(pgnText: String): (Boolean, List[String], List[String]) = {
    val result = PGNParser.parsePGN(pgnText)
    val isValid = result.game.isDefined
    val errors = result.errors
    val warnings = result.warnings
    (isValid, errors, warnings)
  }

  /**
   * Get PGN statistics
   */
  def getPGNStats: Map[String, Any] = {
    currentPGN match {
      case Some(game) =>
        Map(
          "totalMoves" -> game.moves.length,
          "hasVariations" -> game.variations.nonEmpty,
          "result" -> game.result.getOrElse("*"),
          "headers" -> game.headers.size,
          "whiteMoves" -> game.moves.count(_.whiteMove.isDefined),
          "blackMoves" -> game.moves.count(_.blackMove.isDefined)
        )
      case None =>
        Map(
          "totalMoves" -> 0,
          "hasVariations" -> false,
          "result" -> "*",
          "headers" -> 0,
          "whiteMoves" -> 0,
          "blackMoves" -> 0
        )
    }
  }

  /**
   * Clear current PGN data
   */
  def clearPGN(): Unit = {
    currentPGN = None
  }

  /**
   * Check if current game has unsaved PGN changes
   */
  def hasUnsavedChanges: Boolean = {
    val currentPGNString = PGNExporter.exportPGNGame(buildCurrentGame())
    val savedPGNString = currentPGN.map(PGNExporter.exportPGNGame(_)).getOrElse("")
    currentPGNString != savedPGNString
  }

  private def buildCurrentGame(customHeaders: Map[String, String] = Map.empty): PGNGame = {
    val resultString = PGNMoveBuilder.resultFromWinner(gameController.winner)
    val baseHeaders = currentPGN.map(_.headers).getOrElse(PGNHeaderBuilder.defaultFor(gameController))
    val enrichedHeaders = PGNHeaderBuilder.withGameState(
      baseHeaders = baseHeaders,
      overrides = customHeaders,
      result = resultString,
      plyCount = gameController.moveHistoryManager.getMoveHistory.size
    )

    val rawNotations = gameController.moveHistoryManager.getMoveNotations.toList
    val overlayNotations = liveVariationRepository
      .map(_.overlayMainline(rawNotations))
      .getOrElse(rawNotations)
    val adjustedNotations = liveVariationRepository
      .map(_.restoreMainline(overlayNotations))
      .getOrElse(overlayNotations)
    val baseMoves =
      if adjustedNotations.isEmpty then Nil
      else PGNMoveBuilder.fromHalfMoveNotations(adjustedNotations)
    val liveVariations = liveVariationRepository.map(_.exportVariations).getOrElse(Nil)
    val (movesWithVariations, rootVariations) = attachVariationsToMoves(baseMoves, liveVariations)

    PGNGame(
      headers = enrichedHeaders,
      moves = movesWithVariations,
      result = Some(resultString),
      variations = rootVariations
    )
  }

  private[pgn] def attachVariationsToMoves(
    moves: List[PGNMove],
    variations: List[PGNVariation]
  ): (List[PGNMove], List[PGNVariation]) = {
    if variations.isEmpty then return (moves, Nil)

    val (anchored, rootLevel) = variations.partition(_.originMoveNumber.isDefined)
    val validMoveNumbers = moves.map(_.moveNumber).toSet
    val (attachable, orphaned) = anchored.partition(v => validMoveNumbers.contains(v.originMoveNumber.get))
    val grouped = attachable.groupBy(_.originMoveNumber.get)

    val movesWithAttachments = moves.map { move =>
      grouped.get(move.moveNumber) match
        case Some(extras) if extras.nonEmpty =>
          move.copy(variations = move.variations ++ extras)
        case _ => move
    }

    val rootVariations = rootLevel ++ orphaned.map(_.copy(originMoveNumber = None, originColor = None))
    (movesWithAttachments, rootVariations)
  }

}