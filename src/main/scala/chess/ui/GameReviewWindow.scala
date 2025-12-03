package chess.ui

import chess.analysis.{MoveAnalysis, MoveClassification}
import scalafx.Includes._
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, FontWeight}
import scalafx.stage.{Stage, Modality}

/**
 * Statistics for one player (White or Black)
 */
case class PlayerStats(
  totalMoves: Int,
  brilliantCount: Int,
  greatCount: Int,
  bestCount: Int,
  excellentCount: Int,
  goodCount: Int,
  inaccuracyCount: Int,
  mistakeCount: Int,
  blunderCount: Int,
  totalCpLoss: Double,
  accuracy: Double  // Percentage 0-100 (can go negative with many blunders)
) {
  def avgCpLoss: Double = if (totalMoves > 0) totalCpLoss / totalMoves else 0.0
}

object PlayerStats {
  /**
   * Calculate player statistics from move analyses
   * 
   * Simple ACPL-based accuracy formula:
   * - 0 ACPL = 100%
   * - 100 ACPL = 50%
   * - 200+ ACPL = 0%
   * 
   * Formula: accuracy = max(0, 100 - (ACPL / 2))
   */
  def fromMoves(moves: List[MoveAnalysis]): PlayerStats = {
    if (moves.isEmpty) {
      return PlayerStats(
        totalMoves = 0,
        brilliantCount = 0,
        greatCount = 0,
        bestCount = 0,
        excellentCount = 0,
        goodCount = 0,
        inaccuracyCount = 0,
        mistakeCount = 0,
        blunderCount = 0,
        totalCpLoss = 0.0,
        accuracy = 100.0
      )
    }
    
    var brilliantCount = 0
    var greatCount = 0
    var bestCount = 0
    var excellentCount = 0
    var goodCount = 0
    var inaccuracyCount = 0
    var mistakeCount = 0
    var blunderCount = 0
    var totalCpLoss = 0.0
    
    moves.foreach { analysis =>
      // Count by classification (Finnish names from MoveClassification)
      analysis.classification.description match {
        case "Uskomaton" => brilliantCount += 1    // Brilliant
        case "Loistava" => greatCount += 1         // Great
        case "Paras" => bestCount += 1             // Best
        case "Erinomainen" => excellentCount += 1  // Excellent
        case "Hyvä" => goodCount += 1              // Good
        case "Epätarkkuus" => inaccuracyCount += 1 // Inaccuracy
        case "Virhe" => mistakeCount += 1          // Mistake
        case "Munaus" => blunderCount += 1         // Blunder
        case _ => // Book or other
      }
      
      // Calculate cpLoss for this move (scaled for winning positions)
      val cpLoss = analysis.scaledCpLoss
      totalCpLoss += cpLoss
    }
    
    val totalMoves = moves.length
    
    // Simple ACPL-based accuracy: 0 ACPL = 100%, 100 ACPL = 50%, 200+ ACPL = 0%
    val avgCpLoss = totalCpLoss / totalMoves
    val accuracy = Math.max(0.0, 100.0 - (avgCpLoss / 2.0))
    
    PlayerStats(
      totalMoves = totalMoves,
      brilliantCount = brilliantCount,
      greatCount = greatCount,
      bestCount = bestCount,
      excellentCount = excellentCount,
      goodCount = goodCount,
      inaccuracyCount = inaccuracyCount,
      mistakeCount = mistakeCount,
      blunderCount = blunderCount,
      totalCpLoss = totalCpLoss,
      accuracy = accuracy
    )
  }
}

/**
 * Game Review Window - Shows move-by-move analysis with classifications
 * Displays symbols (!, ?, ⭐, ✓) for move quality
 * Includes navigation and statistics summary
 */
class GameReviewWindow(
  parentStage: Stage,
  analysisResults: List[MoveAnalysis],
  gameName: String
) {
  
  private var currentMoveIndex: Int = 0
  private val moveListView = new ListView[String]()
  private val detailsPane = new VBox()
  private val statsPane = new VBox()
  
  // Stage for the review window
  private val reviewStage = new Stage() {
    title = s"Game Review: $gameName"
    initModality(Modality.None)
    initOwner(parentStage)
    width = 900
    height = 700
  }
  
  // Build the UI
  private def buildUI(): Scene = {
    // Left panel: Move list with symbols
    val moveList = buildMoveList()
    
    // Center panel: Move details
    val details = buildDetailsPane()
    
    // Right panel: Statistics summary
    val stats = buildStatsPane()
    
    // Main layout
    val splitPane = new SplitPane() {
      items.addAll(moveList, details, stats)
    }
    splitPane.setDividerPositions(0.3, 0.7)
    
    val mainLayout = new BorderPane() {
      center = splitPane
      padding = Insets(10)
    }
    
    new Scene(mainLayout)
  }
  
  /** Build the move list panel with symbols */
  private def buildMoveList(): VBox = {
    val title = new Label("Move List") {
      font = Font.font("System", FontWeight.Bold, 16)
      padding = Insets(5)
    }
    
    // Populate move list with symbols
    val moveItems = analysisResults.zipWithIndex.map { case (analysis, idx) =>
      val moveNum = (idx / 2) + 1
      val isWhite = idx % 2 == 0
      val symbol = getSymbolForClassification(analysis.classification)
      val notation = analysis.move.notation.getOrElse("???")
      
      if (isWhite) {
        s"$moveNum. $notation $symbol"
      } else {
        s"${moveNum}... $notation $symbol"
      }
    }
    
    moveListView.items = javafx.collections.FXCollections.observableArrayList(moveItems*)
    
    // Select move on click
    moveListView.selectionModel().selectedIndex.onChange { (_, _, newIdx) =>
      if (newIdx.intValue() >= 0) {
        currentMoveIndex = newIdx.intValue()
        updateDetailsPane()
      }
    }
    
    // Select first move by default
    if (moveItems.nonEmpty) {
      moveListView.selectionModel().select(0)
    }
    
    new VBox(5) {
      children = Seq(title, moveListView)
      VBox.setVgrow(moveListView, Priority.Always)
      padding = Insets(5)
    }
  }
  
  /** Build the move details panel */
  private def buildDetailsPane(): VBox = {
    val title = new Label("Move Details") {
      font = Font.font("System", FontWeight.Bold, 16)
      padding = Insets(5)
    }
    
    detailsPane.children.clear()
    detailsPane.padding = Insets(10)
    detailsPane.spacing = 10
    
    val wrapper = new VBox(5) {
      children = Seq(title, detailsPane)
      VBox.setVgrow(detailsPane, Priority.Always)
      padding = Insets(5)
    }
    
    updateDetailsPane()
    wrapper
  }
  
  /** Build the statistics summary panel */
  private def buildStatsPane(): VBox = {
    val title = new Label("Game Statistics") {
      font = Font.font("System", FontWeight.Bold, 16)
      padding = Insets(5)
    }
    
    statsPane.children.clear()
    statsPane.padding = Insets(10)
    statsPane.spacing = 8
    
    // Split moves by color
    val whiteMoves = analysisResults.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)
    val blackMoves = analysisResults.zipWithIndex.filter(_._2 % 2 != 0).map(_._1)
    
    // Calculate statistics
    val whiteStats = PlayerStats.fromMoves(whiteMoves)
    val blackStats = PlayerStats.fromMoves(blackMoves)
    
    // White statistics
    statsPane.children.add(createPlayerStatsSection("White", whiteStats))
    
    // Separator
    statsPane.children.add(new Separator() { padding = Insets(5, 0, 5, 0) })
    
    // Black statistics
    statsPane.children.add(createPlayerStatsSection("Black", blackStats))
    
    // Overall summary
    statsPane.children.add(new Separator() { padding = Insets(5, 0, 5, 0) })
    val totalLabel = new Label(s"Total Moves: ${analysisResults.length}") {
      font = Font.font("System", FontWeight.Bold, 12)
    }
    statsPane.children.add(totalLabel)
    
    new VBox(5) {
      children = Seq(title, statsPane)
      VBox.setVgrow(statsPane, Priority.Always)
      padding = Insets(5)
    }
  }
  
  /** Create a statistics section for one player */
  private def createPlayerStatsSection(playerName: String, stats: PlayerStats): VBox = {
    val section = new VBox(4)
    
    // Player header with accuracy
    val accuracyColor = if (stats.accuracy >= 90) Color.DarkGreen
                        else if (stats.accuracy >= 70) Color.Green
                        else if (stats.accuracy >= 50) Color.Orange
                        else Color.Red
    
    val headerLabel = new Label(s"$playerName") {
      font = Font.font("System", FontWeight.Bold, 14)
    }
    section.children.add(headerLabel)
    
    // Accuracy percentage (main metric)
    val accuracyLabel = new Label(f"  Accuracy: ${stats.accuracy}%.1f%%") {
      font = Font.font("System", FontWeight.Bold, 13)
      textFill = accuracyColor
    }
    section.children.add(accuracyLabel)
    
    // Average centipawn loss
    val acplLabel = new Label(f"  Avg CPL: ${stats.avgCpLoss}%.1f") {
      font = Font.font("System", 11)
      textFill = Color.DarkGray
    }
    section.children.add(acplLabel)
    
    // Move counts by category
    val categoryBox = new VBox(2) { padding = Insets(5, 0, 0, 10) }
    
    if (stats.bestCount > 0) {
      categoryBox.children.add(new Label(s"✓ Best: ${stats.bestCount}") {
        font = Font.font("System", 10)
        textFill = Color.DarkGreen
      })
    }
    if (stats.excellentCount > 0) {
      categoryBox.children.add(new Label(s"✓✓ Excellent: ${stats.excellentCount}") {
        font = Font.font("System", 10)
        textFill = Color.Green
      })
    }
    if (stats.goodCount > 0) {
      categoryBox.children.add(new Label(s"○ Good: ${stats.goodCount}") {
        font = Font.font("System", 10)
        textFill = Color.LimeGreen
      })
    }
    if (stats.inaccuracyCount > 0) {
      categoryBox.children.add(new Label(s"?! Inaccuracy: ${stats.inaccuracyCount}") {
        font = Font.font("System", 10)
        textFill = Color.Orange
      })
    }
    if (stats.mistakeCount > 0) {
      categoryBox.children.add(new Label(s"? Mistake: ${stats.mistakeCount}") {
        font = Font.font("System", 10)
        textFill = Color.DarkOrange
      })
    }
    if (stats.blunderCount > 0) {
      categoryBox.children.add(new Label(s"?? Blunder: ${stats.blunderCount}") {
        font = Font.font("System", 10)
        textFill = Color.Red
      })
    }
    
    section.children.add(categoryBox)
    section
  }
  
  /** Update the details pane with current move info */
  private def updateDetailsPane(): Unit = {
    if (currentMoveIndex >= 0 && currentMoveIndex < analysisResults.length) {
      val analysis = analysisResults(currentMoveIndex)
      val moveNum = (currentMoveIndex / 2) + 1
      val isWhite = currentMoveIndex % 2 == 0
      val color = if (isWhite) "White" else "Black"
      
      detailsPane.children.clear()
      
      // Move header
      val moveHeader = new Label {
        text = s"Move $moveNum - $color"
        font = Font.font("System", FontWeight.Bold, 14)
      }
      
      // Move notation
      val notationLabel = new Label {
        text = s"Move: ${analysis.move.notation.getOrElse("???")}"
        font = Font.font("System", 12)
      }
      
      // Classification with symbol
      val symbol = getSymbolForClassification(analysis.classification)
      val classColor = getColorForClassification(analysis.classification)
      val classificationLabel = new Label {
        text = s"$symbol ${analysis.classification.description}"
        font = Font.font("System", FontWeight.Bold, 14)
        textFill = classColor
      }
      
      // Evaluation before
      val evalBeforeLabel = new Label {
        text = s"Position Before: ${formatEvaluation(analysis.evalBefore)}"
        font = Font.font("System", 12)
      }
      
      // Evaluation after
      val evalAfterLabel = new Label {
        text = s"Position After: ${formatEvaluation(analysis.evalAfter)}"
        font = Font.font("System", 12)
      }
      
      // Best move (if not played)
      analysis.bestMove.foreach { bestMove =>
        if (bestMove != analysis.move.notation.getOrElse("")) {
          val bestMoveLabel = new Label {
            text = s"Best Move: $bestMove"
            font = Font.font("System", FontWeight.Bold, 12)
            textFill = Color.Blue
          }
          detailsPane.children.add(bestMoveLabel)
        }
      }
      
      detailsPane.children.addAll(
        moveHeader,
        notationLabel,
        classificationLabel,
        new Separator(),
        evalBeforeLabel,
        evalAfterLabel
      )
    }
  }
  
  /** Get symbol for classification */
  private def getSymbolForClassification(classification: MoveClassification): String = {
    classification.symbol match {
      case "!!" => "⭐" // Brilliant
      case "!" => "✓✓" // Excellent/Great
      case "" if classification.description == "Best" => "✓" // Best
      case "" if classification.description == "Good" => "○" // Good
      case "" if classification.description == "Book" => "□" // Book
      case "?!" => "?!" // Inaccuracy
      case "?" => "?" // Mistake
      case "??" => "??" // Blunder
      case _ => ""
    }
  }
  
  /** Get color for classification */
  private def getColorForClassification(classification: MoveClassification): Color = {
    classification.symbol match {
      case "!!" => Color.Gold // Brilliant
      case "!" => Color.Green // Excellent/Great
      case "" if classification.description == "Best" => Color.DarkGreen // Best
      case "" if classification.description == "Good" => Color.LightGreen // Good
      case "?!" => Color.Orange // Inaccuracy
      case "?" => Color.DarkOrange // Mistake
      case "??" => Color.Red // Blunder
      case _ => Color.Black
    }
  }
  
  /** Format evaluation for display */
  private def formatEvaluation(eval: chess.analysis.PositionEvaluation): String = {
    eval match {
      case chess.analysis.PositionEvaluation.Centipawns(cp) =>
        // Stockfish always gives evaluation from White's perspective
        // Positive = White is better, Negative = Black is better
        if (cp > 0) {
          f"+${cp / 100.0}%.2f"
        } else {
          f"${cp / 100.0}%.2f"
        }
      case chess.analysis.PositionEvaluation.MateIn(moves) =>
        if (moves > 0) {
          s"Mate in $moves"
        } else {
          s"Mated in ${-moves}"
        }
    }
  }
  
  /** Show the review window */
  def show(): Unit = {
    reviewStage.scene = buildUI()
    reviewStage.show()
  }
}
