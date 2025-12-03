package chess.pgn

import chess.controllers.GameController
import chess.board.Board
import scala.annotation.unused
import scala.collection.mutable.{ListBuffer, StringBuilder}

/**
 * PGN Export functionality - converts internal game state to PGN format
 */
object PGNExporter {

  /**
   * Export current game state to PGN format
   */
  def exportGame(
    controller: GameController,
    @unused board: Board,
    options: PGNExportOptions = PGNExportOptions()
  ): String = {
    val sb = new StringBuilder()
    val result = PGNMoveBuilder.resultFromWinner(controller.winner)
    val headers = PGNHeaderBuilder.withGameState(
      baseHeaders = PGNHeaderBuilder.defaultFor(controller),
      overrides = Map.empty,
      result = result,
      plyCount = controller.moveHistoryManager.getMoveCount
    )

    // Add headers
    if (options.includeHeaders) {
      sb.append(formatHeaders(headers))
      sb.append("\n\n")
    }

    // Add moves
    val movesLine = generateMovesFromHistory(controller, options)
    if movesLine.nonEmpty then
      sb.append(movesLine)
      sb.append(" ")

    // Add result
    sb.append(result)

    sb.toString()
  }

  /**
   * Generate move text from move history
   */
  private def generateMovesFromHistory(controller: GameController, options: PGNExportOptions): String = {
    val moves = PGNMoveBuilder.fromHalfMoveNotations(controller.moveHistoryManager.getMoveNotations)
    if moves.isEmpty then ""
    else formatMovesLine(moves, options, Nil)
  }


  /**
   * Export game with custom headers
   */
  def exportGameWithHeaders(
    controller: GameController,
    @unused board: Board,
    customHeaders: Map[String, String],
    options: PGNExportOptions = PGNExportOptions()
  ): String = {
    val sb = new StringBuilder()
    val fallbackResult = PGNMoveBuilder.resultFromWinner(controller.winner)
    val result = customHeaders.getOrElse("Result", fallbackResult)
    val headers = PGNHeaderBuilder.withGameState(
      baseHeaders = PGNHeaderBuilder.defaultFor(controller),
      overrides = customHeaders,
      result = result,
      plyCount = controller.moveHistoryManager.getMoveCount
    )

    // Add custom headers
    if (options.includeHeaders) {
      sb.append(formatHeaders(headers))
      sb.append("\n\n")
    }

    // Add moves
    val movesLine = generateMovesFromHistory(controller, options)
    if movesLine.nonEmpty then
      sb.append(movesLine)
      sb.append(" ")

    // Add result
    sb.append(result)

    sb.toString()
  }

  /**
   * Export a parsed PGN game with full variation support
   */
  def exportPGNGame(
    game: PGNGame,
    options: PGNExportOptions = PGNExportOptions()
  ): String = {
    val sb = new StringBuilder()

    if (options.includeHeaders) {
      sb.append(formatHeaders(game.headers))
      sb.append("\n\n")
    }

    val movesSection = formatMovesLine(game.moves, options, game.variations)
    if (movesSection.nonEmpty) {
      sb.append(movesSection)
      sb.append(" ")
    }

    sb.append(game.result.getOrElse("*"))
    sb.toString().trim
  }

  private def formatHeaders(headers: Map[String, String]): String = {
    val sb = new StringBuilder()
    headers.foreach { case (key, value) =>
      sb.append(s"[$key \"$value\"]\n")
    }
    sb.toString().trim
  }

  private def formatMovesLine(
    moves: List[PGNMove],
    options: PGNExportOptions,
    leadingVariations: List[PGNVariation]
  ): String = {
    val parts = ListBuffer[String]()
    var needsMoveNumberPrefix = false  // Track if we need N... prefix after variation

    moves.foreach { move =>
      // White's move
      if (move.whiteMove.isDefined) {
        val whiteSegment = new StringBuilder()
        whiteSegment.append(s"${move.moveNumber}. ")
        val whiteVariations = move.variations.filter(_.originColor.contains("white"))
        val halfMove = formatHalfMove(
          notation = move.whiteMove.get,
          comment = move.whiteComment,
          annotations = move.annotations.filter(_.color == "white"),
          variations = whiteVariations,
          options = options
        )
        if (halfMove.nonEmpty) {
          whiteSegment.append(halfMove)
          parts += whiteSegment.toString.trim
        }
        // If white move had variations, black's move needs move number prefix
        needsMoveNumberPrefix = whiteVariations.nonEmpty && options.includeVariations
      }

      // Black's move
      if (move.blackMove.isDefined) {
        val blackVariations = move.variations.filter(_.originColor.contains("black"))
        // Add move number prefix if: no white move, OR we just had white variations
        val prefix = if (move.whiteMove.isEmpty || needsMoveNumberPrefix) s"${move.moveNumber}... " else ""
        val halfMove = formatHalfMove(
          notation = move.blackMove.get,
          comment = move.blackComment,
          annotations = move.annotations.filter(_.color == "black"),
          variations = blackVariations,
          options = options
        )
        if (halfMove.nonEmpty) {
          parts += s"$prefix$halfMove".trim
        }
        needsMoveNumberPrefix = false  // Reset after black's move
      }
    }

    if (options.includeVariations) {
      leadingVariations.foreach { variation =>
        val formatted = formatVariation(variation, options)
        if (formatted.nonEmpty) {
          parts += formatted
        }
      }
    }

    parts.mkString(" ").trim
  }

  private def formatHalfMove(
    notation: String,
    comment: Option[String],
    annotations: List[PGNAnnotation],
    variations: List[PGNVariation],
    options: PGNExportOptions
  ): String = {
    val builder = new StringBuilder()
    builder.append(notation)

    if (options.includeAnnotations) {
      annotations.foreach { ann =>
        if (ann.nag.startsWith("$")) {
          builder.append(" ").append(ann.nag)
        } else {
          builder.append(ann.nag)
        }
      }
    }

    if (options.includeComments) {
      comment.filter(_.nonEmpty).foreach { text =>
        builder.append(" {").append(text).append("}")
      }
    }

    if (options.includeVariations) {
      variations.foreach { variation =>
        val formatted = formatVariation(variation, options)
        if (formatted.nonEmpty) {
          builder.append(" ").append(formatted)
        }
      }
    }

    builder.toString.trim
  }

  private def formatVariation(variation: PGNVariation, options: PGNExportOptions): String = {
    val inner = formatMovesLine(variation.moves, options, variation.leadingVariations)
    val contentBuilder = new StringBuilder()

    if (inner.nonEmpty) {
      contentBuilder.append(inner)
    }

    if (options.includeComments) {
      variation.comment.filter(_.nonEmpty).foreach { text =>
        if (contentBuilder.nonEmpty) {
          contentBuilder.append(" ")
        }
        contentBuilder.append("{").append(text).append("}")
      }
    }

    s"(${contentBuilder.toString.trim})"
  }
}
