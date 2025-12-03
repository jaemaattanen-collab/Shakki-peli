package chess.ui

import chess.analysis.{MoveAnalysis, MoveClassification}
import scalafx.Includes._
import scalafx.beans.property.BooleanProperty
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control._
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.layout._

import scalafx.scene.text.{Font, FontWeight}

/**
 * Chess.com-style Game Review Panel with two modes:
 * 1. Report View: Full-size report showing statistics after analysis
 * 2. Review Mode: Returns to normal view with color-coded move history
 * 3. Classification Browse Mode: Browse moves by type (Brilliant, Mistake, etc.)
 */
class GameReviewPanel {
  
  // State
  private var analysisResults: List[MoveAnalysis] = List.empty
  private val _showingReport = BooleanProperty(true)
  private val _isVisible = BooleanProperty(false)
  
  // Classification browsing state
  private var browseClassification: Option[String] = None  // Current classification being browsed
  private var browseIndices: List[Int] = List.empty        // Move indices matching classification
  private var browsePosition: Int = 0                       // Current position in browseIndices
  private val _isBrowsingByType = BooleanProperty(false)
  
  // Callbacks
  private var onStartReviewCallback: Option[() => Unit] = None
  private var onNavigateToMoveCallback: Option[Int => Unit] = None  // Navigate to move index
  private var onBestResponseCallback: Option[() => Unit] = None      // Play best response
  
  // Root pane that switches between report and review modes
  private val rootPane = new StackPane {
    visible <== _isVisible
    managed <== _isVisible
  }
  
  // Report view (shown after analysis)
  private val reportView = new ScrollPane {
    fitToWidth = true
    hbarPolicy = ScrollPane.ScrollBarPolicy.Never
    style = "-fx-background: #1e2328; -fx-background-color: #1e2328;"
    visible <== _showingReport
    managed <== _showingReport
  }
  
  private val reportContent = new VBox {
    spacing = 16
    padding = Insets(20)
    alignment = Pos.TopCenter
    style = "-fx-background-color: #1e2328;"
  }
  
  reportView.content = reportContent
  
  // Browse by classification panel (shown when browsing moves by type)
  private val browseTypeLabel = new Label("") {
    font = Font.font("System", FontWeight.Bold, 14)
    style = "-fx-text-fill: #ffffff;"
  }
  
  private val browsePositionLabel = new Label("") {
    font = Font.font("System", 12)
    style = "-fx-text-fill: #888888;"
  }
  
  private val nextTypeButton = new Button("Next") {
    style = "-fx-background-color: #81b64c; -fx-text-fill: white; -fx-font-size: 12px; -fx-padding: 6 16; -fx-background-radius: 4;"
    onAction = _ => navigateToNextOfType()
  }
  
  private val showReportButton = new Button("Show Report") {
    style = "-fx-background-color: #5c8bb0; -fx-text-fill: white; -fx-font-size: 12px; -fx-padding: 6 16; -fx-background-radius: 4;"
    onAction = _ => showReport()
  }
  
  // Best Response button for browse panel
  private val browseBestResponseButton = new Button("🎯") {
    style = "-fx-background-color: #e68a2e; -fx-text-fill: white; -fx-font-size: 12px; -fx-padding: 6 12; -fx-background-radius: 4;"
    onAction = _ => triggerBestResponse()
  }
  
  private val browsePanel = new VBox {
    spacing = 8
    padding = Insets(10)
    alignment = Pos.Center
    style = "-fx-background-color: #2a2f35; -fx-background-radius: 8;"
    visible <== _isBrowsingByType
    managed <== _isBrowsingByType
    
    private val infoRow = new HBox {
      spacing = 8
      alignment = Pos.Center
      children = Seq(browseTypeLabel, browsePositionLabel)
    }
    
    private val buttonRow = new HBox {
      spacing = 8
      alignment = Pos.Center
      children = Seq(nextTypeButton, browseBestResponseButton, showReportButton)
    }
    
    children = Seq(infoRow, buttonRow)
  }
  
  // "Best Response" button - shows engine's best line (for normal review mode)
  private val bestResponseButton = new Button("🎯 Best Response") {
    style = "-fx-background-color: #e68a2e; -fx-text-fill: white; -fx-font-size: 12px; -fx-padding: 8 16; -fx-background-radius: 4;"
    onAction = _ => triggerBestResponse()
    visible <== (!_showingReport && !_isBrowsingByType)
    managed <== (!_showingReport && !_isBrowsingByType)
  }
  
  // Collapsed "Show Report" button (shown when report is hidden but not browsing)
  private val collapsedShowReportButton = new Button("📊 Show Report") {
    style = "-fx-background-color: #3d444d; -fx-text-fill: white; -fx-font-size: 12px; -fx-padding: 8 16; -fx-background-radius: 4;"
    onAction = _ => showReport()
    visible <== (!_showingReport && !_isBrowsingByType)
    managed <== (!_showingReport && !_isBrowsingByType)
  }
  
  // Initialize UI
  buildUI()
  
  /** Get the root pane */
  def panel: StackPane = rootPane
  
  /** Get analysis results for move history integration */
  def getAnalysisResults: List[MoveAnalysis] = analysisResults
  
  /** Check if showing report view */
  def isShowingReport: Boolean = _showingReport.value
  
  /** Set analysis results and show report */
  def setAnalysisResults(results: List[MoveAnalysis]): Unit = {
    analysisResults = results
    _isVisible.value = results.nonEmpty
    _showingReport.value = true  // Always show report first
    updateReportView()
  }
  
  /** Clear the panel */
  def clear(): Unit = {
    analysisResults = List.empty
    _isVisible.value = false
    _showingReport.value = true
  }
  
  /** Check if panel has content */
  def hasContent: Boolean = analysisResults.nonEmpty
  
  /** Set callback for when "Start Review" is clicked */
  def setOnStartReview(callback: () => Unit): Unit = {
    onStartReviewCallback = Some(callback)
  }
  
  /** Set callback for navigating to a specific move index */
  def setOnNavigateToMove(callback: Int => Unit): Unit = {
    onNavigateToMoveCallback = Some(callback)
  }
  
  /** Set callback for playing best response */
  def setOnBestResponse(callback: () => Unit): Unit = {
    onBestResponseCallback = Some(callback)
  }
  
  /** Trigger best response preview */
  def triggerBestResponse(): Unit = {
    onBestResponseCallback.foreach(_())
  }
  
  /** Switch to review mode (called by Start Review button) */
  def startReviewMode(): Unit = {
    _showingReport.value = false
    _isBrowsingByType.value = false  // Exit classification browse mode
    browseClassification = None
    _isVisible.value = true  // Keep panel visible for "Show Report" button
    onStartReviewCallback.foreach(_())
  }
  
  /** Show the report again */
  def showReport(): Unit = {
    _showingReport.value = true
    _isBrowsingByType.value = false
    browseClassification = None
    _isVisible.value = true
  }
  
  /** Start browsing moves by classification type */
  def startBrowsingByClassification(classificationName: String, color: String): Unit = {
    // Find all move indices matching this classification
    val indices = analysisResults.zipWithIndex.collect {
      case (analysis, idx) if analysis.classification.description == classificationName => idx
    }
    
    if (indices.nonEmpty) {
      browseClassification = Some(classificationName)
      browseIndices = indices
      browsePosition = 0
      
      // Update UI
      browseTypeLabel.text = s"$classificationName"
      browseTypeLabel.style = s"-fx-text-fill: $color;"
      updateBrowsePositionLabel()
      
      // Hide report, show browse panel
      _showingReport.value = false
      _isBrowsingByType.value = true
      _isVisible.value = true
      
      // Navigate to first match
      onNavigateToMoveCallback.foreach(_(indices.head))
    }
  }
  
  /** Navigate to next move of current classification type */
  private def navigateToNextOfType(): Unit = {
    if (browseIndices.nonEmpty) {
      browsePosition = (browsePosition + 1) % browseIndices.size
      updateBrowsePositionLabel()
      onNavigateToMoveCallback.foreach(_(browseIndices(browsePosition)))
    }
  }
  
  /** Update the position label (e.g., "2 / 5") */
  private def updateBrowsePositionLabel(): Unit = {
    browsePositionLabel.text = s"${browsePosition + 1} / ${browseIndices.size}"
  }
  
  /** Get classification for a move index (for move history integration) */
  def getClassificationForMove(moveIndex: Int): Option[MoveClassification] = {
    analysisResults.lift(moveIndex).map(_.classification)
  }
  
  /** Get symbol and color for a classification */
  def getSymbolAndColor(classification: MoveClassification): (String, String) = {
    classification.description match {
      case "Uskomaton" => ("!!", "#1baca6")      // Brilliant - teal
      case "Paras" => ("★", "#98bc4b")           // Best - star (green)
      case "Loistava" => ("!", "#5c8bb0")        // Great - blue
      case "Erinomainen" => ("○", "#98bc4b")    // Excellent - circle (same green as Best)
      case "Hyvä" => ("✓", "#98bc4b")            // Good - checkmark (same green as Best)
      case "Epätarkkuus" => ("?!", "#f7c631")    // Inaccuracy - yellow
      case "Virhe" => ("?", "#e68a2e")           // Mistake - orange
      case "Munaus" => ("??", "#ca3431")         // Blunder - red
      case "Book" => ("📖", "#a88b5a")           // Book move
      case _ => ("", "#888888")
    }
  }
  
  private def buildUI(): Unit = {
    // Buttons row for when report is hidden
    val buttonsRow = new HBox {
      spacing = 8
      alignment = Pos.Center
      children = Seq(bestResponseButton, collapsedShowReportButton)
    }
    
    // Container for browse panel and show report button (only visible when report is hidden)
    val topControls = new VBox {
      spacing = 8
      alignment = Pos.TopCenter
      padding = Insets(10)
      // Only show when report is NOT showing
      visible <== !_showingReport
      managed <== !_showingReport
      // Make it not intercept mouse events when hidden
      mouseTransparent <== _showingReport
      children = Seq(browsePanel, buttonsRow)
    }
    
    rootPane.children = Seq(reportView, topControls)
    StackPane.setAlignment(topControls, Pos.TopCenter)
  }
  
  private def updateReportView(): Unit = {
    reportContent.children.clear()
    if (analysisResults.isEmpty) return
    
    // Split moves by color
    val whiteMoves = analysisResults.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)
    val blackMoves = analysisResults.zipWithIndex.filter(_._2 % 2 != 0).map(_._1)
    
    val whiteStats = PlayerStats.fromMoves(whiteMoves)
    val blackStats = PlayerStats.fromMoves(blackMoves)
    
    // Header
    val header = new Label("Game Review") {
      font = Font.font("System", FontWeight.Bold, 20)
      style = "-fx-text-fill: #ffffff;"
    }
    
    // Accuracy cards side by side
    val accuracySection = createAccuracySection(whiteStats, blackStats)
    
    // Move classification breakdown
    val classificationSection = createClassificationSection(whiteStats, blackStats)
    
    // Summary
    val summarySection = createSummarySection(whiteStats, blackStats)
    
    // Start Review button
    val startReviewButton = new Button("Start Review") {
      style = "-fx-background-color: #81b64c; -fx-text-fill: white; -fx-font-size: 16px; -fx-font-weight: bold; -fx-padding: 12 40; -fx-background-radius: 5;"
      prefWidth = 250
      onAction = _ => startReviewMode()
    }
    startReviewButton.onMouseEntered = _ => startReviewButton.style = "-fx-background-color: #9bc463; -fx-text-fill: white; -fx-font-size: 16px; -fx-font-weight: bold; -fx-padding: 12 40; -fx-background-radius: 5;"
    startReviewButton.onMouseExited = _ => startReviewButton.style = "-fx-background-color: #81b64c; -fx-text-fill: white; -fx-font-size: 16px; -fx-font-weight: bold; -fx-padding: 12 40; -fx-background-radius: 5;"
    
    val buttonBox = new HBox {
      alignment = Pos.Center
      padding = Insets(20, 0, 0, 0)
      children = Seq(startReviewButton)
    }
    
    reportContent.children.addAll(
      header,
      accuracySection,
      classificationSection,
      summarySection,
      buttonBox
    )
  }
  
  private def createAccuracySection(whiteStats: PlayerStats, blackStats: PlayerStats): HBox = {
    def accuracyCard(label: String, stats: PlayerStats, isWhite: Boolean): VBox = {
      val accuracyColor = if (stats.accuracy >= 90) "#00cc00"
                          else if (stats.accuracy >= 70) "#66cc00"
                          else if (stats.accuracy >= 50) "#ff9900"
                          else "#ff3300"
      
      // Load pawn image from resources
      val pawnImagePath = if (isWhite) "/white-pawn.png" else "/black-pawn.png"
      val pawnImage = new Image(getClass.getResourceAsStream(pawnImagePath))
      
      new VBox {
        spacing = 8
        alignment = Pos.Center
        padding = Insets(16)
        style = "-fx-background-color: #2a2f35; -fx-background-radius: 8;"
        prefWidth = 140
        children = Seq(
          new Label(label) {
            font = Font.font("System", FontWeight.Bold, 14)
            style = "-fx-text-fill: #ffffff;"
          },
          // Chess pawn image from resources
          new ImageView(pawnImage) {
            fitWidth = 36
            fitHeight = 36
            preserveRatio = true
          },
          new Label("Accuracy") {
            font = Font.font("System", 11)
            style = "-fx-text-fill: #888888;"
          },
          new Label(f"${stats.accuracy}%.1f") {
            font = Font.font("System", FontWeight.Bold, 28)
            style = s"-fx-text-fill: $accuracyColor;"
          }
        )
      }
    }
    
    new HBox {
      spacing = 16
      alignment = Pos.Center
      padding = Insets(10, 0, 10, 0)
      children = Seq(
        accuracyCard("White", whiteStats, isWhite = true),
        accuracyCard("Black", blackStats, isWhite = false)
      )
    }
  }
  
  private def createClassificationSection(whiteStats: PlayerStats, blackStats: PlayerStats): VBox = {
    // Map English label to Finnish classification description
    def labelToClassification(label: String): String = label match {
      case "Brilliant" => "Uskomaton"
      case "Great" => "Loistava"
      case "Best" => "Paras"
      case "Excellent" => "Erinomainen"
      case "Good" => "Hyvä"
      case "Inaccuracy" => "Epätarkkuus"
      case "Mistake" => "Virhe"
      case "Blunder" => "Munaus"
      case _ => label
    }
    
    def classRow(label: String, symbol: String, whiteCount: Int, blackCount: Int, color: String): HBox = {
      val totalCount = whiteCount + blackCount
      val isClickable = totalCount > 0
      val classificationName = labelToClassification(label)
      
      val row = new HBox {
        spacing = 8
        alignment = Pos.CenterLeft
        padding = Insets(4, 12, 4, 12)
        style = if (isClickable) "-fx-cursor: hand;" else ""
        children = Seq(
          new Label(label) {
            font = Font.font("System", 12)
            style = "-fx-text-fill: #cccccc;"
            prefWidth = 80
          },
          new Label(whiteCount.toString) {
            font = Font.font("System", FontWeight.Bold, 12)
            style = s"-fx-text-fill: $color;"
            prefWidth = 40
            alignment = Pos.CenterRight
          },
          new StackPane {
            prefWidth = 30
            children = Seq(
              new Label(symbol) {
                font = Font.font("System", 14)
                style = s"-fx-text-fill: $color;"
              }
            )
          },
          new Label(blackCount.toString) {
            font = Font.font("System", FontWeight.Bold, 12)
            style = s"-fx-text-fill: $color;"
            prefWidth = 40
            alignment = Pos.CenterLeft
          }
        )
      }
      
      // Add click handler and hover effects if there are moves of this type
      if (isClickable) {
        row.onMouseEntered = _ => row.style = "-fx-cursor: hand; -fx-background-color: #3d444d; -fx-background-radius: 4;"
        row.onMouseExited = _ => row.style = "-fx-cursor: hand;"
        row.onMouseClicked = _ => startBrowsingByClassification(classificationName, color)
      }
      
      row
    }
    
    new VBox {
      spacing = 2
      padding = Insets(10)
      style = "-fx-background-color: #2a2f35; -fx-background-radius: 8;"
      children = Seq(
        classRow("Brilliant", "!!", whiteStats.brilliantCount, blackStats.brilliantCount, "#1baca6"),
        classRow("Great", "!", whiteStats.greatCount, blackStats.greatCount, "#5c8bb0"),
        classRow("Best", "★", whiteStats.bestCount, blackStats.bestCount, "#98bc4b"),
        classRow("Excellent", "○", whiteStats.excellentCount, blackStats.excellentCount, "#98bc4b"),
        classRow("Good", "✓", whiteStats.goodCount, blackStats.goodCount, "#98bc4b"),
        classRow("Inaccuracy", "?!", whiteStats.inaccuracyCount, blackStats.inaccuracyCount, "#f7c631"),
        classRow("Mistake", "?", whiteStats.mistakeCount, blackStats.mistakeCount, "#e68a2e"),
        classRow("Blunder", "??", whiteStats.blunderCount, blackStats.blunderCount, "#ca3431")
      )
    }
  }
  
  private def createSummarySection(whiteStats: PlayerStats, blackStats: PlayerStats): VBox = {
    new VBox {
      spacing = 8
      padding = Insets(10)
      style = "-fx-background-color: #2a2f35; -fx-background-radius: 8;"
      alignment = Pos.Center
      children = Seq(
        new HBox {
          spacing = 20
          alignment = Pos.Center
          children = Seq(
            new VBox {
              alignment = Pos.Center
              children = Seq(
                new Label("ACPL") {
                  font = Font.font("System", 10)
                  style = "-fx-text-fill: #888888;"
                },
                new Label(f"${whiteStats.avgCpLoss}%.0f") {
                  font = Font.font("System", FontWeight.Bold, 14)
                  style = "-fx-text-fill: #ffffff;"
                }
              )
            },
            new Label("vs") {
              style = "-fx-text-fill: #666666;"
            },
            new VBox {
              alignment = Pos.Center
              children = Seq(
                new Label("ACPL") {
                  font = Font.font("System", 10)
                  style = "-fx-text-fill: #888888;"
                },
                new Label(f"${blackStats.avgCpLoss}%.0f") {
                  font = Font.font("System", FontWeight.Bold, 14)
                  style = "-fx-text-fill: #ffffff;"
                }
              )
            }
          )
        },
        new Label(s"${analysisResults.length} moves analyzed") {
          font = Font.font("System", 11)
          style = "-fx-text-fill: #666666;"
        }
      )
    }
  }
}
