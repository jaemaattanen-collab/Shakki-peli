package chess.ui

import chess.board.Board
import chess.controllers.{GameController, Move}

/**
 * Result of entering a variation - contains updated state
 */
case class VariationEntryResult(
  viewingVariationRootTob: Option[Long],
  currentVariationMove: Option[Move],
  variationMode: Boolean,
  variationRootTob: Option[Long],
  animationCoords: Option[(Int, Int, Int, Int, String, String)] = None  // fromRow, fromCol, toRow, toCol, color, pieceType
)

/**
 * Result of exiting a variation
 */
sealed trait VariationExitResult
case class ExitToParentVariation(
  parentRootTob: Long,
  parentIdx: Int
) extends VariationExitResult
case class ExitToMainline(anchorIndex: Int) extends VariationExitResult
case object ExitToStart extends VariationExitResult
case object NoExit extends VariationExitResult

/**
 * Handles navigation into and out of variations.
 * This is pure logic - UI updates are handled by the caller.
 */
class VariationNavigator(
  controller: GameController,
  boardModel: Board,
  moveReplayer: MoveReplayer
) {

  /**
   * Enter a selected variation from mainline.
   * 
   * @param variationRoot The Move representing the variation root
   * @param anchorIndex Current mainline anchor index
   * @return Some(result) with new state if successful, None if failed
   */
  def enterVariation(variationRoot: Move, anchorIndex: Int): Option[VariationEntryResult] = {
    println(s"[enterVariation] variationRoot.tob=${variationRoot.tob}, anchorIndex=$anchorIndex")
    val variationNotations = controller.moveHistoryManager.getVariationMoveNotations(variationRoot.tob)
    println(s"[enterVariation] variationNotations=$variationNotations")
    
    if variationNotations.nonEmpty then
      // Restore board to branch point
      boardModel.resetBoard()
      controller.currentPlayer = "white"
      controller.board.foreach(_.deselectAll())
      
      val mainlineMoves = controller.moveHistoryManager.getMoveHistory.filter(!_.isVariation)
      val actualAnchorIndex = controller.moveHistoryManager.getMainlineAnchorIndexForVariation(variationRoot.tob)
      val movesToReplay = mainlineMoves.take(actualAnchorIndex + 1)
      movesToReplay.foreach { move =>
        controller.replayMove(move)
      }
      controller.currentPlayer = if (actualAnchorIndex + 1) % 2 == 0 then "white" else "black"
      
      // Replay ONLY THE FIRST variation move
      val firstMoveNotation = variationNotations.head
      val startingColor = controller.moveHistoryManager.getVariationStartingColor(variationRoot.tob)
      val isWhiteMove = startingColor == "white"
      println(s"[enterVariation] Replaying first move: $firstMoveNotation, isWhiteMove=$isWhiteMove")
      val animCoords = moveReplayer.replayMoveFromSan(firstMoveNotation, isWhiteMove)
      
      // Update possible moves after entering variation
      controller.updateAllPossibleMoves()
      
      println(s"[enterVariation] Success - returning result with rootTob=${variationRoot.tob}")
      Some(VariationEntryResult(
        viewingVariationRootTob = Some(variationRoot.tob),
        currentVariationMove = Some(variationRoot),
        variationMode = true,
        variationRootTob = Some(variationRoot.tob),
        animationCoords = animCoords
      ))
    else
      None
  }

  /**
   * Enter a nested (child) variation from within a parent variation.
   * 
   * @param childVariationRoot The Move representing the child variation root
   * @param parentRootTob The parent variation's root TOB
   * @param currentPly Current position index in parent variation
   * @return Some(result) with new state if successful, None if failed
   */
  def enterChildVariation(
    childVariationRoot: Move,
    parentRootTob: Long,
    currentPly: Int
  ): Option[VariationEntryResult] = {
    // Verify this is actually a child of the parent at the current ply
    val parentLine = controller.moveHistoryManager.collectVariationLine(parentRootTob)
    val isChildOfParent = parentLine.lift(currentPly).exists(_.tob == childVariationRoot.parentTob)
    
    if isChildOfParent then
      val childNotations = controller.moveHistoryManager.getVariationMoveNotations(childVariationRoot.tob)
      if childNotations.nonEmpty then
        val firstMoveNotation = childNotations.head
        val startingColor = controller.moveHistoryManager.getVariationStartingColor(childVariationRoot.tob)
        val isWhiteMove = startingColor == "white"
        val animCoords = moveReplayer.replayMoveFromSan(firstMoveNotation, isWhiteMove)
        
        // Update possible moves after entering child variation
        controller.updateAllPossibleMoves()
        
        Some(VariationEntryResult(
          viewingVariationRootTob = Some(childVariationRoot.tob),
          currentVariationMove = Some(childVariationRoot),
          variationMode = true,
          variationRootTob = Some(childVariationRoot.tob),
          animationCoords = animCoords
        ))
      else None
    else None
  }

  /**
   * Determine how to exit the current variation.
   * 
   * @param currentRootTob The current variation's root TOB
   * @return The exit result indicating where to go
   */
  def determineExit(currentRootTob: Long): VariationExitResult = {
    val rootMove = controller.moveHistoryManager.getMoveByTob(currentRootTob)
    rootMove match
      case Some(rm) if rm.parentTob == -1L =>
        // Move 1 variation - exit to starting position
        ExitToStart
        
      case Some(rm) if rm.parentTob > 0 =>
        // Check if parent is a variation move or mainline
        controller.moveHistoryManager.getMoveByTob(rm.parentTob) match
          case Some(pm) if pm.isVariation =>
            // Parent is in a variation - go up to that variation
            pm.variationRootTob match
              case Some(parentVariationRootTob) =>
                val parentLine = controller.moveHistoryManager.collectVariationLine(parentVariationRootTob)
                val parentIdx = parentLine.indexWhere(_.tob == pm.tob)
                ExitToParentVariation(parentVariationRootTob, parentIdx)
              case None =>
                val anchorIndex = controller.moveHistoryManager.getMainlineAnchorIndexForVariation(currentRootTob)
                ExitToMainline(anchorIndex)
          case _ =>
            // Parent is mainline - exit to mainline
            val anchorIndex = controller.moveHistoryManager.getMainlineAnchorIndexForVariation(currentRootTob)
            ExitToMainline(anchorIndex)
            
      case _ =>
        // No valid parent - exit to mainline
        val anchorIndex = controller.moveHistoryManager.getMainlineAnchorIndexForVariation(currentRootTob)
        ExitToMainline(anchorIndex)
  }

  /**
   * Restore board to parent variation position.
   * 
   * @param parentRootTob Parent variation root TOB
   * @param parentIdx Index within parent variation
   * @return The Move at the restored position
   */
  def restoreToParentVariation(parentRootTob: Long, parentIdx: Int): Option[Move] = {
    moveReplayer.restoreVariationPosition(parentRootTob, parentIdx).move
  }

  /**
   * Reset board to starting position for exiting move 1 variations.
   */
  def resetToStart(): Unit = {
    boardModel.resetBoard()
    controller.currentPlayer = "white"
    controller.board.foreach(_.deselectAll())
    controller.updateAllPossibleMoves()
  }
}