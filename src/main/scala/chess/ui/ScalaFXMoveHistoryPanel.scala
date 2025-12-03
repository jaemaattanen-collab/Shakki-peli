package chess.ui

import scalafx.geometry.Insets
import scalafx.scene.control.{Label, ScrollPane}
import scalafx.scene.layout.VBox

/**
 * Combined move history panel housing the list and the status footer.
 */
final case class MoveHistoryPanelState(
  display: MoveHistoryDisplayState,
  statusMessage: String = ""
)

class ScalaFXMoveHistoryPanel extends VBox {

  private val moveHistoryView = new ScalaFXMoveHistoryView()
  private val variationStatusLabel = new Label("") {
    style = "-fx-text-fill: #cccccc; -fx-font-size: 12px;"
    wrapText = true
  }

  spacing = 8
  style = "-fx-background-color: #1a1a1a; -fx-background-radius: 5; -fx-padding: 10;"
  padding = Insets(10)

  private val titleLabel = new Label("Move History") {
    style = "-fx-text-fill: white; -fx-font-size: 14px; -fx-font-weight: bold;"
  }

  private val moveHistoryScroll = new ScrollPane {
    style = "-fx-background: #1a1a1a; -fx-control-inner-background: #1a1a1a;"
    fitToWidth = true
    vbarPolicy = ScrollPane.ScrollBarPolicy.AsNeeded
    hbarPolicy = ScrollPane.ScrollBarPolicy.Never
    content = moveHistoryView
  }

  children = Seq(titleLabel, moveHistoryScroll, variationStatusLabel)

  def setOnMoveSelected(handler: Int => Unit): Unit =
    moveHistoryView.onMoveClick = Some(handler)

  def clearOnMoveSelected(): Unit =
    moveHistoryView.onMoveClick = None

  def setOnVariationSelected(handler: Int => Unit): Unit =
    moveHistoryView.onVariationClick = Some(handler)

  def clearOnVariationSelected(): Unit =
    moveHistoryView.onVariationClick = None

  def updateDisplay(displayState: MoveHistoryDisplayState): Unit =
    moveHistoryView.updateMoves(displayState)

  def updateStatus(message: String): Unit =
    variationStatusLabel.text = message

  def render(state: MoveHistoryPanelState): Unit = {
    updateDisplay(state.display)
    updateStatus(state.statusMessage)
  }
}
