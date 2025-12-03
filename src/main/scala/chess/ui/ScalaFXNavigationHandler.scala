package chess.ui

import chess.board.Board
import chess.controllers.{GameController, Move}
import chess.state.CastlingRights
import chess.types.PieceType

/**
 * Provides linear navigation over the move history using the time-of-birth (TOB)
 * metadata stored on each move. Navigation is stateless in the sense that every
 * jump rebuilds the visible board position from the starting position up to the
 * selected move.
 */
class ScalaFXNavigationHandler(
  controller: GameController,
  board: Board,
  onAfterRebuild: () => Unit = () => {}
) {

  import ScalaFXNavigationHandler.*

  private var cursor: NavigationCursor = StartingCursor

  /** Current navigation focus (None when at the starting position). */
  def currentMove: Option[Move] =
    if cursor.tob == StartingTob then None else controller.moveHistoryManager.getMoveByTob(cursor.tob)

  /** Expose the current navigation cursor (TOB + parent TOB). */
  def currentCursor: NavigationCursor = cursor

  /** Reset navigation back to the starting position. */
  def resetToStart(rebuildBoard: Boolean): Option[NavigationResult] =
    cursor = StartingCursor
    if rebuildBoard then Some(rebuildBoardState(None)) else None

  /**
   * Update the cursor to the latest move without touching the visible board.
   * Useful after a new move is played because the board is already up-to-date.
   */
  def syncToLatest(): Unit =
    // Use getLastMainlineMove to avoid syncing to variation moves
    cursor = controller.moveHistoryManager.getLastMainlineMove
      .map(move => NavigationCursor(move.tob, move.parentTob))
      .getOrElse(StartingCursor)

  def navigateToHalfMoveIndex(index: Int): Option[NavigationResult] =
    if index < 0 then
      val changed = cursor != StartingCursor
      cursor = StartingCursor
      if changed then Some(rebuildBoardState(None)) else None
    else
      controller.moveHistoryManager.getMainlineMoveAt(index) match
        case Some(targetMove) =>
          cursor = NavigationCursor(targetMove.tob, targetMove.parentTob)
          Some(rebuildBoardState(Some(targetMove)))
        case None => None

  /** Navigate to the parent of the current move, if available. */
  def navigateToParent(): Option[NavigationResult] =
    val current = currentMove
    val targetParentTob = current.map(_.parentTob).getOrElse(StartingParentTob)
    if targetParentTob == StartingParentTob then
      // Parent points to sentinel -> already at start
      None
    else if targetParentTob == StartingTob then
      cursor = StartingCursor
      Some(rebuildBoardState(None))
    else
      controller.moveHistoryManager.getMoveByTob(targetParentTob) match
        case Some(parentMove) =>
          cursor = NavigationCursor(parentMove.tob, parentMove.parentTob)
          Some(rebuildBoardState(Some(parentMove)))
        case None =>
          None

  /** Navigate to the first child (by history order) of the current move, if any. */
  def navigateToChild(): Option[NavigationResult] =
    val parentTob = cursor.tob
    val parentDepth = currentMove.map(_.halfmoveDistanceFromStart).getOrElse(0)
    val childDepth = parentDepth + 1
    controller.moveHistoryManager.findChildAtDepth(parentTob, childDepth) match
      case Some(child) =>
        cursor = NavigationCursor(child.tob, child.parentTob)
        Some(rebuildBoardState(Some(child)))
      case None =>
        None

  private def rebuildBoardState(targetMove: Option[Move]): NavigationResult =
    board.resetBoard()
    board.enPassantTarget = None

    controller.currentPlayer = "white"
    controller.winner = None
    controller.totalMoves = 0
    controller.halfmoveClock = 0
    controller.fullmoveNumber = 0
    controller.enPassantTarget = None
    controller.castlingRights = CastlingRights(whiteKingSide = true, whiteQueenSide = true, blackKingSide = true, blackQueenSide = true)

    val movesToReplay = targetMove match
      case Some(move) =>
        val path = controller.moveHistoryManager.collectLineTo(move)
        if path.nonEmpty then path
        else Nil
      case None =>
        Nil

    var halfmoveClock = 0
    var totalMoves = 0
    var fullmoveNumber = 0

    movesToReplay.foreach { mv =>
      val moved = controller.replayMove(mv)
      if moved then
        totalMoves += 1
        if mv.piece.color == "white" then
          fullmoveNumber += 1
        if mv.capturedPiece.nonEmpty || mv.piece.pieceType == PieceType.Pawn then
          halfmoveClock = 0
        else
          halfmoveClock += 1
    }

    controller.totalMoves = totalMoves
    controller.halfmoveClock = halfmoveClock
    controller.fullmoveNumber = fullmoveNumber
    controller.updateAllPossibleMoves()

    val result = NavigationResult(targetMove, cursor)
    onAfterRebuild()
    result
  end rebuildBoardState
}

object ScalaFXNavigationHandler {
  final val StartingTob: Long = -1L
  final val StartingParentTob: Long = 0L

  final case class NavigationCursor(tob: Long, parentTob: Long)
  final case class NavigationResult(move: Option[Move], cursor: NavigationCursor)

  val StartingCursor: NavigationCursor = NavigationCursor(StartingTob, StartingParentTob)
}
