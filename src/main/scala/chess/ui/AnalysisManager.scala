package chess.ui

import scalafx.Includes._
import chess.analysis.{FENGenerator, MoveNotationConverter, StockfishEngine}
import chess.board.Board
import chess.controllers.GameController
import scalafx.application.Platform
import scalafx.beans.property.DoubleProperty
import scalafx.geometry.Pos
import scalafx.scene.control.{CheckMenuItem, Label, TextArea}
import scalafx.scene.layout.{StackPane, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import scala.io.Source
import scala.util.{Failure, Success, Try, Using}

/**
 * Encapsulates the Stockfish analysis lifecycle together with the ScalaFX UI
 * elements that visualise the evaluation bar and engine output.
 */
final class AnalysisManager(
  stockfishEngine: StockfishEngine,
  boardModel: Board,
  controller: GameController,
  notificationCenter: NotificationCenter
) {
  private var analysisActive = false
  private var analysisCallbackRegistered = false
  
  // Callback for live engine arrows - called with top moves (UCI notation) when analysis updates
  private var liveArrowsCallback: Option[List[String] => Unit] = None

  private val analysisEvalLabel = new Label("Evaluation: +0.00") {
    style = "-fx-text-fill: #4CAF50; -fx-font-size: 16px; -fx-font-weight: bold;"
  }

  private val analysisDepthLabel = new Label("Depth: --") {
    style = "-fx-text-fill: #cccccc;"
  }

  // Three variation lines - each is an HBox with colored eval + white moves
  private val (variationLine1, evalLabel1, movesLabel1) = createVariationLine()
  private val (variationLine2, evalLabel2, movesLabel2) = createVariationLine()
  private val (variationLine3, evalLabel3, movesLabel3) = createVariationLine()
  
  private def createVariationLine(): (javafx.scene.layout.HBox, Label, Label) = {
    val evalLabel = new Label("") {
      style = "-fx-font-size: 13px; -fx-font-weight: bold; -fx-padding: 0 8 0 0;"
      minWidth = 55
    }
    val movesLabel = new Label("") {
      style = "-fx-text-fill: #e0e0e0; -fx-font-size: 12px;"
      wrapText = true
    }
    val hbox = new javafx.scene.layout.HBox {
      setStyle("-fx-background-color: #2d333b; -fx-padding: 8 12; -fx-background-radius: 6;")
      setSpacing(4)
      getChildren.addAll(evalLabel, movesLabel)
    }
    (hbox, evalLabel, movesLabel)
  }
  
  private val variationLinesBox = new VBox {
    spacing = 6
  }
  variationLinesBox.children.addAll(variationLine1, variationLine2, variationLine3)

  // Keep TextArea for snapshot compatibility but hide it
  private val analysisVariationsArea = new TextArea {
    text = "Engine analysis pending..."
    prefHeight = 0
    maxHeight = 0
    visible = false
    managed = false
  }

  private val evalBarWhiteRatio: DoubleProperty = DoubleProperty(0.5)

  private val evalBarWhiteRect = new Rectangle {
    arcWidth = 8
    arcHeight = 8
    fill = Color.web("#f7f7f7")
    stroke = Color.Transparent
  }

  private val evalBarScoreLabel = new Label("0.00") {
    style = "-fx-text-fill: white; -fx-font-weight: bold; -fx-font-size: 10px; -fx-effect: dropshadow(one-pass-box, rgba(0, 0, 0, 0.7), 4, 0, 0, 1);"
  }

  val evalBar: StackPane = new StackPane {
    prefWidth = 32
    minWidth = 28
    maxWidth = 40
    style = "-fx-background-color: #111111; -fx-background-radius: 8; -fx-border-color: #303030; -fx-border-radius: 8;"
    visible = false
    managed <== visible
    children = Seq(evalBarWhiteRect, evalBarScoreLabel)
  }

  evalBarWhiteRect.width <== evalBar.widthProperty
  evalBarWhiteRect.height <== evalBar.heightProperty * evalBarWhiteRatio
  StackPane.setAlignment(evalBarWhiteRect, Pos.BottomCenter)
  StackPane.setAlignment(evalBarScoreLabel, Pos.Center)

  val engineAnalysisPanel: VBox = new VBox {
    spacing = 8
    style = "-fx-background-color: #1a1a1a; -fx-background-radius: 5; -fx-padding: 10;"
    visible = false
    managed <== visible
    children = Seq(
      new Label("Engine Analysis") {
        style = "-fx-text-fill: white; -fx-font-size: 14px; -fx-font-weight: bold;"
      },
      analysisEvalLabel,
      analysisDepthLabel,
      variationLinesBox
    )
  }

  val toggleMenuItem: CheckMenuItem = new CheckMenuItem("Enable Analysis") {
    selected = analysisActive
    onAction = _ => setAnalysisActive(selected())
  }

  private val analysisCallback: StockfishEngine#EngineAnalysis => Unit = analysis =>
    Platform.runLater(() => handleEngineAnalysis(analysis))

  def applyLoadedSnapshot(evalLine: String, depthLine: String, body: String): Unit = {
    analysisEvalLabel.text = evalLine
    analysisDepthLabel.text = depthLine
    analysisVariationsArea.text = body
    setAnalysisActive(enabled = true)
  }

  def snapshotLines: Seq[String] =
    Seq(analysisEvalLabel.text.value, analysisDepthLabel.text.value, analysisVariationsArea.text.value)

  def loadSnapshotFromFile(file: java.io.File): Unit =
    Using(Source.fromFile(file))(_.getLines().toList) match {
      case Success(lines) =>
        val evalText = lines.headOption.getOrElse("Evaluation: N/A")
        val depthText = lines.drop(1).headOption.getOrElse("Depth: N/A")
        val rest = lines.drop(2).mkString("\n")
        applyLoadedSnapshot(evalText, depthText, rest)
        notificationCenter.showInfo(
          key = "analysis-status",
          title = "Analysis loaded",
          message = s"Loaded analysis from ${file.getName}"
        )
      case Failure(ex) =>
        notificationCenter.showError(
          key = "analysis-status",
          title = "Analysis load failed",
          message = ex.getMessage
        )
    }

  def saveSnapshotToFile(file: java.io.File): Unit = {
    val content = snapshotLines.mkString("\n")
    val result = Try {
      Option(file.getParentFile).filterNot(_.exists()).foreach(_.mkdirs())
      Files.write(file.toPath, content.getBytes(StandardCharsets.UTF_8))
    }
    result match {
      case Success(_) =>
        notificationCenter.showInfo(
          key = "analysis-status",
          title = "Analysis saved",
          message = s"Analysis saved to ${file.getName}"
        )
      case Failure(ex) =>
        notificationCenter.showError(
          key = "analysis-status",
          title = "Analysis save failed",
          message = ex.getMessage
        )
    }
  }

  def setAnalysisActive(enabled: Boolean): Unit = {
    if enabled then
      val started = if stockfishEngine.isRunning then true else stockfishEngine.start()
      if !started then
        notificationCenter.showError(
          key = "analysis-error",
          title = "Engine error",
          message = "Could not start Stockfish. Make sure it's installed (try: brew install stockfish)."
        )
        toggleMenuItem.selected = false
        analysisActive = false
        return

      if !analysisCallbackRegistered then
        stockfishEngine.addAnalysisCallback(analysisCallback)
        analysisCallbackRegistered = true

      analysisActive = true
      engineAnalysisPanel.visible = true
      evalBar.visible = true
      triggerAnalysisIfActive()
    else
      analysisActive = false
      if analysisCallbackRegistered then
        stockfishEngine.removeAnalysisCallback(analysisCallback)
        analysisCallbackRegistered = false

      stockfishEngine.stop()
      updateEvaluationBar(Some(0), None)
      analysisDepthLabel.text = "Depth: --"
      analysisVariationsArea.text = "Engine analysis disabled."
      engineAnalysisPanel.visible = false
      evalBar.visible = false
      
      // Clear live arrows when analysis is disabled
      liveArrowsCallback.foreach(_(List.empty))

    toggleMenuItem.selected = analysisActive
  }

  def triggerAnalysisIfActive(): Unit =
    if analysisActive && stockfishEngine.isRunning then
      analysisDepthLabel.text = "Depth: --"
      analysisVariationsArea.text = "Engine thinking..."
      val fen = FENGenerator.generateFEN(boardModel, controller)
      stockfishEngine.analyzePosition(fen)

  def stop(): Unit =
    if analysisCallbackRegistered then
      stockfishEngine.removeAnalysisCallback(analysisCallback)
      analysisCallbackRegistered = false
    stockfishEngine.stop()
  
  /** Register a callback to receive live top moves (UCI notation) for arrow display */
  def setLiveArrowsCallback(callback: List[String] => Unit): Unit = {
    liveArrowsCallback = Some(callback)
  }
  
  /** Clear the live arrows callback */
  def clearLiveArrowsCallback(): Unit = {
    liveArrowsCallback = None
  }
  
  /** Check if live analysis is active */
  def isAnalysisActive: Boolean = analysisActive

  private def handleEngineAnalysis(analysis: StockfishEngine#EngineAnalysis): Unit = {
    updateEvaluationBar(analysis.score, analysis.mate)
    val depthInfo =
      if analysis.variations.nonEmpty then s"Depth: ${analysis.depth} • ${analysis.variations.length} lines"
      else s"Depth: ${analysis.depth}"
    analysisDepthLabel.text = depthInfo
    
    // Update variation lines (chess.com style: eval + 3 moves + ...)
    updateVariationLines(analysis)
    
    // Keep TextArea updated for snapshot compatibility
    analysisVariationsArea.text = renderAnalysisLines(analysis)
    
    // Notify live arrows callback with top moves from variations
    liveArrowsCallback.foreach { callback =>
      val topMoves = analysis.variations
        .sortBy(_.multiPvIndex)
        .flatMap(_.pv.headOption)
        .distinct
        .take(3)
      callback(topMoves)
    }
  }
  
  private def updateVariationLines(analysis: StockfishEngine#EngineAnalysis): Unit = {
    val sortedVariations = analysis.variations.sortBy(_.multiPvIndex).take(3)
    val lines = Seq(
      (variationLine1, evalLabel1, movesLabel1),
      (variationLine2, evalLabel2, movesLabel2),
      (variationLine3, evalLabel3, movesLabel3)
    )
    
    lines.zipWithIndex.foreach { case ((hbox, evalLabel, movesLabel), idx) =>
      if (idx < sortedVariations.length) {
        val variation = sortedVariations(idx)
        val evalText = formatEvalForLine(variation.score, variation.mate)
        val evalColor = getEvalColor(variation.score, variation.mate)
        val movesText = formatCompactPvMoves(variation.pv.take(6))  // 3 full moves = 6 half-moves
        val ellipsis = if (variation.pv.length > 6) " ..." else ""
        
        // Colored eval label
        evalLabel.text = evalText
        evalLabel.style = s"-fx-text-fill: $evalColor; -fx-font-size: 13px; -fx-font-weight: bold; -fx-padding: 0 8 0 0;"
        
        // White moves label
        movesLabel.text = s"$movesText$ellipsis"
        movesLabel.style = "-fx-text-fill: #e0e0e0; -fx-font-size: 12px;"
        
        hbox.setVisible(true)
        hbox.setManaged(true)
      } else {
        evalLabel.text = ""
        movesLabel.text = ""
        hbox.setVisible(false)
        hbox.setManaged(false)
      }
    }
  }
  
  private def getEvalColor(score: Option[Int], mate: Option[Int]): String = {
    // Always use green color for eval - position perspective depends on which side you're playing
    "#81b64c"
  }
  
  private def formatEvalForLine(score: Option[Int], mate: Option[Int]): String = {
    mate match
      case Some(m) if m != 0 =>
        val mateSign = if m > 0 then "M" else "M-"
        s"$mateSign${math.abs(m)}"
      case _ =>
        val value = score.getOrElse(0) / 100.0
        if value >= 0 then f"+$value%.2f" else f"$value%.2f"
  }
  
  private def formatCompactPvMoves(uciMoves: List[String]): String = {
    if uciMoves.isEmpty then return ""
    
    // Convert UCI moves to chess notation with move numbers
    val notations = uciMoves.zipWithIndex.map { case (uci, idx) =>
      val notation = MoveNotationConverter.uciToChessNotation(uci, boardModel)
      notation
    }
    
    // Group into full moves (white + black)
    notations.grouped(2).zipWithIndex.map { case (pair, moveNum) =>
      val num = moveNum + 1
      pair match {
        case Seq(white, black) => s"$num. $white $black"
        case Seq(white) => s"$num. $white"
        case _ => ""
      }
    }.mkString(" ")
  }

  private def updateEvaluationBar(score: Option[Int], mate: Option[Int]): Unit = {
    val ratio = mate match
      case Some(m) if m > 0 => 1.0
      case Some(m) if m < 0 => 0.0
      case _ =>
        val clamped = score.map(s => math.max(-1000, math.min(1000, s))).getOrElse(0)
        val base = 0.5 + (clamped / 2000.0)
        math.max(0.0, math.min(1.0, base))

    evalBarWhiteRatio.value = ratio
    val text = formatEvaluationText(score, mate)
    evalBarScoreLabel.text = text

    val color = mate match
      case Some(m) if m > 0 => "#4CAF50"
      case Some(m) if m < 0 => "#ff5252"
      case _ =>
        val cp = score.getOrElse(0)
        if cp > 30 then "#4CAF50"
        else if cp < -30 then "#ff5252"
        else "#cccccc"

    analysisEvalLabel.style = s"-fx-text-fill: $color; -fx-font-size: 16px; -fx-font-weight: bold;"
    analysisEvalLabel.text = s"Evaluation: $text"
  }

  private def renderAnalysisLines(analysis: StockfishEngine#EngineAnalysis): String =
    if analysis.variations.isEmpty then
      "Engine evaluating..."
    else
      analysis.variations.zipWithIndex
        .map { (variation, idx) =>
          val label = if idx == 0 then "Main line" else s"Line ${idx + 1}"
          val eval = formatEvaluationText(variation.score, variation.mate)
          val moves = formatPvMoves(variation.pv.take(12))
            .grouped(4)
            .map(_.mkString(" "))
            .mkString("\n    ")
          s"$label ($eval):\n    $moves"
        }
        .mkString("\n\n")

  private def formatEvaluationText(score: Option[Int], mate: Option[Int]): String =
    mate match
      case Some(m) if m != 0 =>
        val mateSign = if m > 0 then "M" else "M-"
        s"$mateSign${math.abs(m)}"
      case _ =>
        val value = score.getOrElse(0) / 100.0
        if value >= 0 then f"+$value%.2f" else f"$value%.2f"

  /**
   * Format a list of UCI moves into algebraic notation.
   * First move uses proper notation with board state,
   * subsequent moves use simplified notation.
   */
  private def formatPvMoves(uciMoves: List[String]): List[String] = {
    if uciMoves.isEmpty then return List.empty
    
    // First move can use MoveNotationConverter with current board state
    val firstNotation = MoveNotationConverter.uciToChessNotation(uciMoves.head, boardModel)
    
    // Subsequent moves use simplified notation (piece + destination)
    val restNotations = uciMoves.tail.map(formatUciMoveSimple)
    
    firstNotation :: restNotations
  }
  
  /**
   * Format UCI move to simplified algebraic notation without full board simulation.
   * Uses piece letter inference from move pattern.
   */
  private def formatUciMoveSimple(uci: String): String = {
    if uci == null || uci.length < 4 then return uci
    
    val from = uci.substring(0, 2)
    val to = uci.substring(2, 4)
    val promotion = if uci.length > 4 then s"=${uci.charAt(4).toUpper}" else ""
    
    // Check for castling patterns
    if (from == "e1" && to == "g1") || (from == "e8" && to == "g8") then return "O-O"
    if (from == "e1" && to == "c1") || (from == "e8" && to == "c8") then return "O-O-O"
    
    val fromFile = from.charAt(0)
    val toFile = to.charAt(0)
    val fromRank = from.charAt(1)
    val toRank = to.charAt(1)
    
    val fileDiff = math.abs(fromFile - toFile)
    val rankDiff = math.abs(fromRank - toRank)
    
    // Knight: L-shaped move pattern (must check first - unique pattern)
    val isKnightMove = (fileDiff == 1 && rankDiff == 2) || (fileDiff == 2 && rankDiff == 1)
    
    // Bishop: diagonal move (any distance > 1)
    val isBishopMove = fileDiff == rankDiff && fileDiff > 1
    
    // Rook: horizontal or vertical (any distance)
    val isRookMove = (fileDiff == 0 || rankDiff == 0) && (fileDiff + rankDiff > 0)
    
    // Pawn: forward 1-2 squares on same file, OR diagonal capture (1 square diagonally)
    // Diagonal pawn capture is exactly 1 file and 1 rank difference
    val isPawnForward = fileDiff == 0 && rankDiff >= 1 && rankDiff <= 2
    val isPawnCapture = fileDiff == 1 && rankDiff == 1
    val isPawnMove = isPawnForward || isPawnCapture
    
    // Determine piece prefix - order matters!
    val piecePrefix = 
      if isKnightMove then "N"
      else if isBishopMove then "B"
      else if isRookMove then "R"
      else if isPawnMove then ""  // Pawn has no prefix (or file for captures)
      else "Q"  // Queen can move like rook or bishop - fallback for long diagonals
    
    // For pawn captures, add source file prefix
    val prefix = if isPawnCapture then fromFile.toString else piecePrefix
    
    s"$prefix$to$promotion"
  }
}
