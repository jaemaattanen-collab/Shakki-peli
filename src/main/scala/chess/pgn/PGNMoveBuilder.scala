package chess.pgn

import scala.collection.mutable.ListBuffer

/**
 * Shared helpers for constructing PGN move structures from SAN or notation lists.
 */
object PGNMoveBuilder {

  /**
   * Convert a flat list of half-move notations into structured PGN moves.
   * @param notations algebraic notations in half-move order (white, black, white, ...)
   * @param startingMoveNumber move number to assign to the first pair (defaults to 1)
   */
  def fromHalfMoveNotations(notations: Seq[String], startingMoveNumber: Int = 1): List[PGNMove] = {
    val sanitized = notations.toList.map(_.trim).filter(_.nonEmpty)
    sanitized.grouped(2).zipWithIndex.map { case (pair, index) =>
      val whiteMove = pair.headOption
      val blackMove = pair.lift(1)
      PGNMove(
        moveNumber = startingMoveNumber + index,
        whiteMove = whiteMove,
        blackMove = blackMove
      )
    }.toList
  }

  /**
   * Convert a SAN sequence (starting from an arbitrary colour) into PGN moves.
   * @param startingMoveNumber move number to assign to the first SAN entry
   * @param startingColor colour whose turn it is for the first SAN entry
   * @param sanMoves list of SAN moves in play order
   */
  def fromSanSequence(
      startingMoveNumber: Int,
      startingColor: String,
      sanMoves: Seq[String]
  ): List[PGNMove] = {
    val sanitized = sanMoves.toList.map(_.trim).filter(_.nonEmpty)
    if sanitized.isEmpty then return List.empty

    val buffer = ListBuffer[PGNMove]()
    var moveNumber = startingMoveNumber
    var colourToMove = startingColor.toLowerCase
    var index = 0

    while index < sanitized.length do
      colourToMove match
        case "white" =>
          val whiteSan = sanitized(index)
          index += 1
          val blackSan =
            if index < sanitized.length then
              val san = sanitized(index)
              index += 1
              Some(san)
            else None

          buffer += PGNMove(
            moveNumber = moveNumber,
            whiteMove = Some(whiteSan),
            blackMove = blackSan
          )
          moveNumber += 1
          colourToMove = "white"

        case "black" =>
          val blackSan = sanitized(index)
          index += 1
          buffer += PGNMove(
            moveNumber = moveNumber,
            whiteMove = None,
            blackMove = Some(blackSan)
          )
          moveNumber += 1
          colourToMove = "white"

        case _ =>
          // Fallback for unexpected inputs – treat as white to keep numbers progressing
          colourToMove = "white"

    buffer.toList
  }

  /**
   * Translate a winner indicator into a PGN result string.
   */
  def resultFromWinner(winner: Option[String]): String =
    winner match
      case Some("white") => "1-0"
      case Some("black") => "0-1"
      case Some("draw")  => "1/2-1/2"
      case _              => "*"
}
