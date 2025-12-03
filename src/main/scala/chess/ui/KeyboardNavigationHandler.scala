package chess.ui

import chess.controllers.{GameController, Move}

/**
 * Result of keyboard navigation action
 */
sealed trait KeyboardNavigationAction
case object NavigateLeft extends KeyboardNavigationAction
case object NavigateRight extends KeyboardNavigationAction
case object NavigateLeftInVariation extends KeyboardNavigationAction
case class NavigateRightInVariation(advanceToIdx: Int) extends KeyboardNavigationAction
case object ExitCurrentVariation extends KeyboardNavigationAction
case class ShowBranchOverlay(anchorIdx: Int, variations: Vector[Move]) extends KeyboardNavigationAction
case object EnterSelectedVariation extends KeyboardNavigationAction
case class EnterSelectedChildVariation(currentIdx: Int) extends KeyboardNavigationAction
case object CycleSelectionUp extends KeyboardNavigationAction
case object CycleSelectionDown extends KeyboardNavigationAction
case object NoAction extends KeyboardNavigationAction

/**
 * Determines keyboard navigation actions based on current state.
 * Pure logic - no side effects, UI updates handled by caller.
 */
class KeyboardNavigationHandler(controller: GameController) {

  /**
   * Determine action for Left arrow key.
   * 
   * @param viewingVariationRootTob Current variation root if viewing one
   * @param currentVariationMove Current move in variation
   * @return The navigation action to perform
   */
  def handleLeftKey(
    viewingVariationRootTob: Option[Long],
    currentVariationMove: Option[Move]
  ): KeyboardNavigationAction = {
    viewingVariationRootTob match
      case Some(parentRootTob) =>
        val variationLine = controller.moveHistoryManager.collectVariationLine(parentRootTob)
        val currentIdx = currentVariationMove.map(m => variationLine.indexWhere(_.tob == m.tob)).getOrElse(-1)
        
        if currentIdx > 0 then
          NavigateLeftInVariation
        else
          ExitCurrentVariation
          
      case None =>
        NavigateLeft
  }

  /**
   * Determine action for Right arrow key.
   * 
   * @param viewingVariationRootTob Current variation root if viewing one
   * @param currentVariationMove Current move in variation
   * @param currentAnchorIndex Current mainline anchor index
   * @param getSelectedIdx Function to get current selection state for an anchor
   * @return The navigation action to perform
   */
  def handleRightKey(
    viewingVariationRootTob: Option[Long],
    currentVariationMove: Option[Move],
    currentAnchorIndex: Int,
    getSelectedIdx: Int => Int  // Returns -99 if no selection, -1 for mainline, 0+ for variation
  ): KeyboardNavigationAction = {
    viewingVariationRootTob match
      case Some(parentRootTob) =>
        handleRightInVariation(parentRootTob, currentVariationMove, getSelectedIdx)
        
      case None =>
        handleRightInMainline(currentAnchorIndex, getSelectedIdx)
  }

  private def handleRightInVariation(
    parentRootTob: Long,
    currentVariationMove: Option[Move],
    getSelectedIdx: Int => Int
  ): KeyboardNavigationAction = {
    val parentLine = controller.moveHistoryManager.collectVariationLine(parentRootTob)
    val currentIdx = currentVariationMove.map(m => parentLine.indexWhere(_.tob == m.tob)).getOrElse(-1)
    
    val childVariations = if currentIdx >= 0 then
      controller.moveHistoryManager.getNestedVariationRootsAt(parentRootTob, currentIdx).toVector
    else
      Vector.empty
    
    if childVariations.nonEmpty then
      val selectedIdx = getSelectedIdx(currentIdx)
      
      if selectedIdx == -1 then
        // Continue in variation selected
        if currentIdx < parentLine.length - 1 then
          NavigateRightInVariation(currentIdx + 1)
        else
          NoAction
      else if selectedIdx >= 0 then
        // Child variation selected
        EnterSelectedChildVariation(currentIdx)
      else
        // No selection yet - show overlay
        ShowBranchOverlay(currentIdx, childVariations)
    else
      // No child variations - advance if possible
      if currentIdx < parentLine.length - 1 then
        NavigateRightInVariation(currentIdx + 1)
      else
        NoAction
  }

  private def handleRightInMainline(
    currentAnchorIndex: Int,
    getSelectedIdx: Int => Int
  ): KeyboardNavigationAction = {
    val variations = getVariationsAtAnchor(currentAnchorIndex)
    
    if variations.nonEmpty then
      val selectedIdx = getSelectedIdx(currentAnchorIndex)
      
      if selectedIdx == -1 then
        // Mainline selected
        NavigateRight
      else if selectedIdx >= 0 && selectedIdx < variations.size then
        // Variation selected
        EnterSelectedVariation
      else
        // No selection yet - show overlay
        ShowBranchOverlay(currentAnchorIndex, variations)
    else
      NavigateRight
  }

  private def getVariationsAtAnchor(anchorIndex: Int): Vector[Move] = {
    controller.moveHistoryManager.getVariationRootsAtMainlineIndex(anchorIndex).toVector
  }

  /**
   * Get current position index within a variation.
   */
  def getCurrentVariationIndex(
    parentRootTob: Long,
    currentVariationMove: Option[Move]
  ): Int = {
    val variationLine = controller.moveHistoryManager.collectVariationLine(parentRootTob)
    currentVariationMove.map(m => variationLine.indexWhere(_.tob == m.tob)).getOrElse(-1)
  }

  /**
   * Get variation line length.
   */
  def getVariationLineLength(parentRootTob: Long): Int = {
    controller.moveHistoryManager.collectVariationLine(parentRootTob).length
  }
}
