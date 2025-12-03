package chess.ui

import chess.controllers.{GameController, Move}
import scalafx.scene.control.Label
import scalafx.scene.layout.VBox
import scalafx.scene.input.MouseEvent
import scalafx.Includes._

/**
 * Builds the UI content for branch selection overlays.
 * Displays available variations at a branch point with selection highlighting.
 */
class BranchOverlayBuilder(controller: GameController) {

  /**
   * Build the overlay content showing available continuations.
   * 
   * @param anchorIndex The move index where branching occurs
   * @param variations The available variation roots
   * @param selectionIndex Currently selected option (-1 = mainline/continue, 0+ = variation index, -99 = no selection)
   * @param viewingVariationRootTob If viewing a variation, its root TOB
   * @param currentVariationMove Current move in the variation being viewed
   * @param onContinueClick Callback when "continue" option is clicked
   * @param onVariationClick Callback when a variation is clicked (receives variation root and index)
   * @return VBox containing the styled continuation options
   */
  def buildContent(
    anchorIndex: Int,
    variations: Vector[Move],
    selectionIndex: Int,
    viewingVariationRootTob: Option[Long],
    currentVariationMove: Option[Move],
    onContinueClick: () => Unit = () => (),
    onVariationClick: (Move, Int) => Unit = (_, _) => ()
  ): VBox = {
    val colourToMove = controller.currentPlayer.capitalize

    // Determine the "continue" option based on context
    val (continueLabel, continuePreview) = viewingVariationRootTob match {
      case Some(parentRootTob) =>
        // We're in a variation - show next move in this variation as the "continue" option
        val variationLine = controller.moveHistoryManager.collectVariationLine(parentRootTob)
        val currentPly = currentVariationMove.map(_.halfmoveDistanceFromStart).getOrElse(-1)
        val nextMoveInVariation = variationLine.find(_.halfmoveDistanceFromStart == currentPly + 1).flatMap(_.notation)
        val preview = nextMoveInVariation match {
          case Some(mv) => s"$colourToMove plays $mv"
          case None => "End of variation"
        }
        ("Continue in variation", preview)
      case None =>
        // We're in mainline
        val futureNotations = controller.moveHistoryManager.getMoveNotations
        val dropCount = math.max(anchorIndex + 1, 0)
        val futureMoves = if dropCount < futureNotations.length then futureNotations.drop(dropCount) else Nil
        val preview = futureMoves.headOption match {
          case Some(mv) => s"$colourToMove plays $mv"
          case None => "No recorded mainline continuation from this position."
        }
        ("Mainline continuation", preview)
    }

    val content = new VBox { spacing = 8 }

    // "Continue" option (mainline or continue in variation)
    content.children.add(buildContinueOption(continueLabel, continuePreview, selectionIndex == -1, onContinueClick))

    // Variation options
    val variationLabelPrefix = if viewingVariationRootTob.isDefined then "Sub-variation" else "Variation"
    
    variations.zipWithIndex.foreach { case (variationRoot, idx) =>
      content.children.add(
        buildVariationOption(variationRoot, idx, variationLabelPrefix, selectionIndex == idx, () => onVariationClick(variationRoot, idx))
      )
    }

    content
  }

  /**
   * Build title for the branch selection overlay.
   */
  def buildTitle(
    anchorIndex: Int,
    viewingVariationRootTob: Option[Long],
    currentVariationMove: Option[Move]
  ): String = {
    viewingVariationRootTob match {
      case Some(parentRootTob) =>
        val variationLine = controller.moveHistoryManager.collectVariationLine(parentRootTob)
        val currentPly = currentVariationMove.map(_.halfmoveDistanceFromStart).getOrElse(-1)
        val moveInVariation = variationLine.find(_.halfmoveDistanceFromStart == currentPly).flatMap(_.notation).getOrElse("start")
        s"Branch point in variation after $moveInVariation"
      case None =>
        if anchorIndex < 0 then "Choose continuation from the starting position"
        else
          val moveNo = startingMoveNumberFromAnchor(anchorIndex)
          val colourText = startingColorFromAnchor(anchorIndex).capitalize
          s"$colourText to move after move $moveNo"
    }
  }

  private def buildContinueOption(label: String, preview: String, isSelected: Boolean, onClick: () => Unit): VBox = {
    new VBox {
      spacing = 4
      style = entryStyle(isSelected, "rgba(255,202,40,0.18)") + " -fx-cursor: hand;"
      onMouseClicked = (_: MouseEvent) => onClick()
      children = Seq(
        new Label(label) {
          wrapText = true
          style = if isSelected then "-fx-text-fill: #ffca28; -fx-font-weight: bold;"
                  else "-fx-text-fill: #e0e0e0; -fx-font-weight: bold;"
        },
        new Label(preview) {
          wrapText = true
          style = "-fx-text-fill: #cfd8dc;"
        }
      )
    }
  }

  private def buildVariationOption(variationRoot: Move, idx: Int, labelPrefix: String, isSelected: Boolean, onClick: () => Unit): VBox = {
    val variationNotations = controller.moveHistoryManager.getVariationMoveNotations(variationRoot.tob)
    val startingColor = controller.moveHistoryManager.getVariationStartingColor(variationRoot.tob)
    
    val nextMoveText = variationNotations.headOption match
      case Some(move) => s"${startingColor.capitalize} plays $move"
      case None       => "Variation has no recorded moves yet."

    new VBox {
      spacing = 4
      style = entryStyle(isSelected, "rgba(100,181,246,0.2)") + " -fx-cursor: hand;"
      onMouseClicked = (_: MouseEvent) => onClick()
      children = Seq(
        new Label(s"$labelPrefix ${idx + 1}") {
          wrapText = true
          style = if isSelected then "-fx-text-fill: #64b5f6; -fx-font-weight: bold;"
                  else "-fx-text-fill: #e0e0e0; -fx-font-weight: bold;"
        },
        new Label(nextMoveText) {
          wrapText = true
          style = if isSelected then "-fx-text-fill: #ffffff;"
                  else "-fx-text-fill: #cfd8dc;"
        }
      )
    }
  }

  private def entryStyle(selected: Boolean, accent: String): String =
    if selected then s"-fx-background-color: ${accent}; -fx-padding: 10; -fx-background-radius: 6;"
    else "-fx-background-color: rgba(255,255,255,0.05); -fx-padding: 10; -fx-background-radius: 6;"

  private def startingMoveNumberFromAnchor(anchorIndex: Int): Int =
    ((anchorIndex + 1) / 2) + 1

  private def startingColorFromAnchor(anchorIndex: Int): String =
    if ((anchorIndex + 1) % 2 == 0) "white" else "black"
}
