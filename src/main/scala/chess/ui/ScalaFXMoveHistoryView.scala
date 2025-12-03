package chess.ui

import scalafx.geometry.Insets
import scalafx.scene.control.{Button, Tooltip}
import scalafx.scene.layout.{VBox, HBox}
import scalafx.scene.control.Label

/**
 * Move classification info for display
 */
final case class MoveClassificationInfo(
  symbol: String,
  color: String
)

/**
 * Immutable snapshot describing how the move history should be rendered.
 */
final case class MoveHistoryDisplayState(
  notations: List[String],
  highlightIndex: Option[Int] = None,
  variationIndicators: Map[Int, List[String]] = Map.empty,
  classifications: Map[Int, MoveClassificationInfo] = Map.empty,  // Analysis results
  windowedDisplay: Boolean = true,  // Show only a window of moves around current position
  halfMovesBefore: Int = 4,         // Number of half-moves to show before current
  halfMovesAfter: Int = 6           // Number of half-moves to show after current
)

/**
 * ScalaFX implementation of the modern move history list.
 *
 * Takes move notation display metadata and renders numbered rows. Variation
 * indicators can be supplied per half-move so the UI can surface alternate lines.
 *   1. [e4] [e5]
 *   2. [Nf3] [Nc6]
 */
class ScalaFXMoveHistoryView extends VBox {

  spacing = 6
  padding = Insets(4)

  // Optional callback when a specific move index is clicked
  var onMoveClick: Option[Int => Unit] = None
  var onVariationClick: Option[Int => Unit] = None

  def updateMoves(displayState: MoveHistoryDisplayState): Unit =
    updateMoves(displayState.notations, displayState.highlightIndex, displayState.variationIndicators, displayState.classifications)

  /**
   * Update the moves displayed.
   * @param notations Flat list of move notations (white, black, white, ...)
   * @param highlightIndex Optional zero-based index of move to highlight
   * @param variations Map of move index -> variation previews, used to render arrows
   *                   Index -1 means variations from starting position (before move 1)
   * @param classifications Map of move index -> classification info for color coding
   */
  def updateMoves(
    notations: List[String],
    highlightIndex: Option[Int] = None,
    variations: Map[Int, List[String]] = Map.empty,
    classifications: Map[Int, MoveClassificationInfo] = Map.empty
  ): Unit = {
    // Use default windowed display
    updateMovesWindowed(notations, highlightIndex, variations, classifications, 
      windowedDisplay = true, halfMovesBefore = 4, halfMovesAfter = 6)
  }
  
  /**
   * Update the moves displayed with windowed display support.
   */
  def updateMovesWindowed(
    notations: List[String],
    highlightIndex: Option[Int] = None,
    variations: Map[Int, List[String]] = Map.empty,
    classifications: Map[Int, MoveClassificationInfo] = Map.empty,
    windowedDisplay: Boolean = true,
    halfMovesBefore: Int = 4,
    halfMovesAfter: Int = 6
  ): Unit = {
    children.clear()
    
    if (notations.isEmpty) return
    
    // Calculate the visible window of half-moves
    val currentIdx = highlightIndex.getOrElse(notations.length - 1)
    val (startIdx, endIdx) = if (windowedDisplay && notations.length > (halfMovesBefore + halfMovesAfter + 1)) {
      val rawStart = currentIdx - halfMovesBefore
      val rawEnd = currentIdx + halfMovesAfter
      
      // Adjust window if near edges
      val (adjStart, adjEnd) = if (rawStart < 0) {
        (0, math.min(notations.length - 1, halfMovesBefore + halfMovesAfter))
      } else if (rawEnd >= notations.length) {
        (math.max(0, notations.length - 1 - halfMovesBefore - halfMovesAfter), notations.length - 1)
      } else {
        (rawStart, rawEnd)
      }
      (adjStart, adjEnd)
    } else {
      (0, notations.length - 1)
    }
    
    // Align startIdx to even (white's move) for proper move numbering
    val alignedStartIdx = if (startIdx % 2 == 1) math.max(0, startIdx - 1) else startIdx
    
    // Show ellipsis at top if not showing from beginning
    if (alignedStartIdx > 0) {
      val ellipsisRow = new HBox {
        spacing = 8
      }
      val ellipsisLabel = new Label("...") {
        style = "-fx-text-fill: #888888; -fx-font-style: italic;"
        prefWidth = 28
      }
      ellipsisRow.children.add(ellipsisLabel)
      children.add(ellipsisRow)
    }

    // Check for variations from starting position (index -1) only if visible
    if (alignedStartIdx == 0) {
      variations.get(-1).foreach { previews =>
        val startRow = new HBox {
          spacing = 8
        }
        val startLabel = new Label("Start") {
          style = "-fx-text-fill: #888888; -fx-font-style: italic;"
          prefWidth = 28
        }
        startRow.children.add(startLabel)
        val arrowBtn = createVariationButton(previews, -1)
        startRow.children.add(arrowBtn)
        children.add(startRow)
      }
    }

    // Filter notations to visible window
    val visibleNotations = notations.zipWithIndex.filter { case (_, idx) =>
      idx >= alignedStartIdx && idx <= endIdx
    }
    
    val movePairs: List[List[(String, Int)]] =
      visibleNotations.grouped(2).map(_.toList).toList

    for ((pair, pairIndex) <- movePairs.zipWithIndex) {
      // Calculate actual move number from original index
      val firstIdxInPair = pair.head._2
      val moveNumber = (firstIdxInPair / 2) + 1

      val row = new HBox {
        spacing = 8
      }

      val numberLabel = new Label(s"$moveNumber.") {
        style = "-fx-text-fill: #cccccc; -fx-font-weight: bold;"
        prefWidth = 28
      }
      row.children.add(numberLabel)

      // White move button (if present)
      if (pair.nonEmpty) {
        val (whiteNotation, whiteIndex) = pair.head
        val isHighlighted = highlightIndex.contains(whiteIndex)
        val classification = classifications.get(whiteIndex)
        val whiteBtn = createMoveButton(whiteNotation, isHighlighted, classification)
        whiteBtn.onAction = _ => onMoveClick.foreach(_(whiteIndex))
        row.children.add(whiteBtn)
        variations.get(whiteIndex).foreach { previews =>
          val arrowBtn = createVariationButton(previews, whiteIndex)
          row.children.add(arrowBtn)
        }
      }

      // Black move button (if present)
      if (pair.length > 1) {
        val (blackNotation, blackIndex) = pair(1)
        val isHighlighted = highlightIndex.contains(blackIndex)
        val classification = classifications.get(blackIndex)
        val blackBtn = createMoveButton(blackNotation, isHighlighted, classification)
        blackBtn.onAction = _ => onMoveClick.foreach(_(blackIndex))
        row.children.add(blackBtn)
        variations.get(blackIndex).foreach { previews =>
          val arrowBtn = createVariationButton(previews, blackIndex)
          row.children.add(arrowBtn)
        }
      }

      children.add(row)
    }
    
    // Show ellipsis at bottom if not showing to end
    if (endIdx < notations.length - 1) {
      val ellipsisRow = new HBox {
        spacing = 8
      }
      val ellipsisLabel = new Label("...") {
        style = "-fx-text-fill: #888888; -fx-font-style: italic;"
        prefWidth = 28
      }
      ellipsisRow.children.add(ellipsisLabel)
      children.add(ellipsisRow)
    }
  }

  private def createMoveButton(text: String, highlighted: Boolean, classification: Option[MoveClassificationInfo] = None): Button = {
    // Base colors based on classification
    val (bgColor, borderColor) = classification match {
      case Some(c) => 
        c.symbol match {
          case "!!" => ("#1a4a47", "#1baca6")  // Brilliant - teal
          case "!" => ("#2a4a60", "#5c8bb0")   // Great - blue
          case "★" => ("#3a5030", "#98bc4b")   // Best - star (green)
          case "○" => ("#3a5030", "#98bc4b")   // Excellent - circle (same green as Best)
          case "✓" => ("#3a5030", "#98bc4b")  // Good - checkmark (same green as Best)
          case "?!" => ("#4a4020", "#f7c631")  // Inaccuracy - yellow
          case "?" => ("#4a3020", "#e68a2e")   // Mistake - orange
          case "??" => ("#4a2020", "#ca3431")  // Blunder - red
          case _ => ("#404040", "#606060")
        }
      case None => ("#404040", "#606060")
    }
    
    val highlightOverlay = if (highlighted) {
      "-fx-background-color: #3a5f8f; -fx-border-color: #7fb3ff;"
    } else {
      s"-fx-background-color: $bgColor; -fx-border-color: $borderColor;"
    }
    
    val baseStyle =
      "-fx-text-fill: white;" +
        "-fx-background-radius: 5;" +
        "-fx-border-radius: 5;" +
        "-fx-border-width: 1;" +
        "-fx-padding: 2 8 2 8;"

    // Add classification symbol to button text if present
    val displayText = classification match {
      case Some(c) if c.symbol.nonEmpty => s"$text ${c.symbol}"
      case _ => text
    }

    new Button(displayText) {
      minWidth = 50
      prefHeight = 26
      style = baseStyle + highlightOverlay
      
      // Tooltip showing classification
      classification.foreach { c =>
        tooltip = new Tooltip(getClassificationName(c.symbol)) {
          style = "-fx-font-size: 11px;"
        }
      }
    }
  }
  
  private def getClassificationName(symbol: String): String = symbol match {
    case "!!" => "Brilliant"
    case "!" => "Great"
    case "★" => "Best"
    case "○" => "Excellent"
    case "✓" => "Good"
    case "?!" => "Inaccuracy"
    case "?" => "Mistake"
    case "??" => "Blunder"
    case "📖" => "Book"
    case _ => ""
  }

  private def createVariationButton(previews: List[String], moveIndex: Int): Button = {
    val tooltipText =
      if (previews.nonEmpty) previews.mkString("\n") else "Other variations"

    new Button("▾") {
      prefWidth = 26
      prefHeight = 26
      style =
        "-fx-background-color: #2f2f2f;" +
          "-fx-text-fill: white;" +
          "-fx-background-radius: 5;" +
          "-fx-border-radius: 5;" +
          "-fx-border-color: #606060;" +
          "-fx-border-width: 1;"

      tooltip = new Tooltip(tooltipText) {
        style = "-fx-font-size: 11px;"
      }

      onAction = _ => onVariationClick.foreach(_(moveIndex))
    }
  }
}
