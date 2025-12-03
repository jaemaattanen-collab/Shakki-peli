package chess.ui

import scalafx.application.Platform
import scalafx.beans.property.DoubleProperty
import scalafx.geometry.Pos
import scalafx.scene.control.Label
import scalafx.scene.layout.{Pane, StackPane}
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Arc, ArcType, Circle}
import scalafx.scene.text.{Font, FontWeight}

/**
 * Circular progress indicator with percentage display.
 * Used for showing game analysis progress.
 */
class CircularProgressIndicator(size: Double = 100) extends StackPane {
  
  private val progress = DoubleProperty(0.0)
  private val radius = size / 2 - 8
  private val center = size / 2
  
  // Background circle (track) - centered in parent
  private val backgroundCircle = new Circle {
    centerX = CircularProgressIndicator.this.center
    centerY = CircularProgressIndicator.this.center
    this.radius = CircularProgressIndicator.this.radius
    fill = Color.Transparent
    stroke = Color.web("#3a3a3a")
    strokeWidth = 8
  }
  
  // Progress arc - centered at same position as background circle
  private val progressArc = new Arc {
    centerX = CircularProgressIndicator.this.center
    centerY = CircularProgressIndicator.this.center
    radiusX = CircularProgressIndicator.this.radius
    radiusY = CircularProgressIndicator.this.radius
    startAngle = 90  // Start from top
    length <== progress * -360  // Negative for clockwise
    fill = Color.Transparent
    stroke = Color.web("#81b64c")  // Green progress
    strokeWidth = 8
    `type` = ArcType.Open
  }
  
  // Container for circle and arc - uses fixed size Pane to prevent layout shifts
  private val circleContainer = new Pane {
    prefWidth = size
    prefHeight = size
    minWidth = size
    minHeight = size
    maxWidth = size
    maxHeight = size
    children = Seq(backgroundCircle, progressArc)
  }
  
  // Percentage label - slightly smaller font to prevent clipping
  private val percentLabel = new Label("0%") {
    font = Font.font("System", FontWeight.Bold, size * 0.20)
    style = "-fx-text-fill: #ffffff;"
    minWidth = size * 0.6  // Ensure enough width for "100%"
  }
  
  // Status text below percentage
  private val statusLabel = new Label("Analyzing...") {
    font = Font.font("System", size * 0.11)
    style = "-fx-text-fill: #888888;"
  }
  
  // Content container with padding to prevent clipping
  private val textContainer = new scalafx.scene.layout.VBox {
    spacing = 2
    alignment = Pos.Center
    padding = scalafx.geometry.Insets(4)
    children = Seq(percentLabel, statusLabel)
  }
  
  prefWidth = size
  prefHeight = size
  alignment = Pos.Center
  // Use circleContainer (fixed size Pane) to prevent arc movement during growth
  children = Seq(circleContainer, textContainer)
  
  // Update label when progress changes
  progress.onChange { (_, _, _) =>
    Platform.runLater {
      val percent = (progress.value * 100).toInt
      percentLabel.text = s"$percent%"
    }
  }
  
  /** Set progress value (0.0 to 1.0) */
  def setProgress(value: Double): Unit = {
    Platform.runLater {
      progress.value = math.max(0.0, math.min(1.0, value))
    }
  }
  
  /** Set status text */
  def setStatus(text: String): Unit = {
    Platform.runLater {
      statusLabel.text = text
    }
  }
  
  /** Get current progress (0.0 to 1.0) */
  def getProgress: Double = progress.value
}

/**
 * Overlay panel that shows during game analysis with circular progress.
 */
class AnalysisProgressOverlay extends StackPane {
  
  private val progressIndicator = new CircularProgressIndicator(120)
  
  private val titleLabel = new Label("Analyzing Game") {
    font = Font.font("System", FontWeight.Bold, 18)
    style = "-fx-text-fill: #ffffff;"
  }
  
  private val contentBox = new scalafx.scene.layout.VBox {
    spacing = 16
    alignment = Pos.Center
    padding = scalafx.geometry.Insets(30)
    style = "-fx-background-color: rgba(30, 35, 40, 0.95); -fx-background-radius: 12;"
    children = Seq(titleLabel, progressIndicator)
  }
  
  style = "-fx-background-color: rgba(0, 0, 0, 0.7);"
  alignment = Pos.Center
  visible = false
  managed <== visible
  children = Seq(contentBox)
  
  /** Show the overlay */
  def show(): Unit = {
    Platform.runLater {
      progressIndicator.setProgress(0.0)
      progressIndicator.setStatus("Starting...")
      visible = true
    }
  }
  
  /** Hide the overlay */
  def hide(): Unit = {
    Platform.runLater {
      visible = false
    }
  }
  
  /** Update progress (0.0 to 1.0) */
  def setProgress(value: Double): Unit = {
    progressIndicator.setProgress(value)
  }
  
  /** Update status text */
  def setStatus(text: String): Unit = {
    progressIndicator.setStatus(text)
  }
  
  /** Set title */
  def setTitle(text: String): Unit = {
    Platform.runLater {
      titleLabel.text = text
    }
  }
}
