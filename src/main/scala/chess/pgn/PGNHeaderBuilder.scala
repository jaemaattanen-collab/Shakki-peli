package chess.pgn

import chess.controllers.GameController
import java.time.LocalDate

/**
 * Central place for constructing PGN header maps so callers don't duplicate defaults.
 */
object PGNHeaderBuilder {

  /**
   * Produce the standard header set for the given controller snapshot.
   */
  def defaultFor(controller: GameController): Map[String, String] = {
    val result = PGNMoveBuilder.resultFromWinner(controller.winner)
    val plyCount = controller.moveHistoryManager.getMoveCount.toString

    Map(
      "Event" -> "Casual Game",
      "Site" -> "Local Game",
      "Date" -> LocalDate.now().toString,
      "Round" -> "-",
      "White" -> "Player",
      "Black" -> "Player",
      "Result" -> result,
      "Annotator" -> "Chess Application",
      "PlyCount" -> plyCount
    )
  }

  /**
   * Merge overrides and ensure the key game-state headers stay in sync.
   */
  def withGameState(
      baseHeaders: Map[String, String],
      overrides: Map[String, String],
      result: String,
      plyCount: Int
  ): Map[String, String] = {
    val merged = if overrides.nonEmpty then baseHeaders ++ overrides else baseHeaders
    merged ++ Map(
      "Result" -> result,
      "PlyCount" -> plyCount.toString
    )
  }
}
