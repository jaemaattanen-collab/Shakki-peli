package chess.ui

import chess.analysis.{GameAnalyzer, MoveClassification, EngineConfig, StockfishEngine, PositionEvaluation}
import chess.training.{ErrorTrainingSession, ErrorPosition, TrainingDifficulty, ErrorPositionExtractor, GamePreparer, AnalyzableGame}
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.beans.property.{BooleanProperty, StringProperty}
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.scene.text.{Font, FontWeight}
import scalafx.stage.{FileChooser, Stage}

import java.io.File
import scala.concurrent.{Future, ExecutionContext, Promise}

/**
 * Error Training Panel - Integrates into main application side panel.
 * Allows loading games, analyzing for errors, and practicing from error positions.
 * 
 * Workflow:
 * 1. Load PGN files (5-20 games)
 * 2. Enter player name to detect color
 * 3. Analyze games to find errors
 * 4. Practice from each error position against Stockfish
 */
class ErrorTrainingPanel(
  ownerStage: () => Stage
)(implicit ec: ExecutionContext) {
  
  // === State ===
  private val _isVisible = BooleanProperty(false)
  private val _isAnalyzing = BooleanProperty(false)
  private val _hasSession = BooleanProperty(false)
  private val _currentPhase = StringProperty("setup") // setup, analyzing, training, complete
  
  private var trainingSession: Option[ErrorTrainingSession] = None
  private var loadedGames: Seq[AnalyzableGame] = Seq.empty
  
  // Callbacks to main app
  private var onSetupPosition: Option[String => Unit] = None  // FEN to set on board
  private var onMakeMove: Option[(String, String, Option[String]) => Unit] = None  // Make a move on board (from, to, promotion)
  private var onMakeMoveAnimated: Option[(String, String, Option[String], () => Unit) => Unit] = None  // Animated move
  private var onGetCurrentFEN: Option[() => String] = None  // Get current board FEN
  private var onExitTraining: Option[() => Unit] = None
  private var onFlipBoard: Option[Boolean => Unit] = None  // Flip board to given orientation (true = black at bottom)
  private var onShowArrow: Option[(String, String, String) => Unit] = None  // Show arrow (from, to, color)
  private var onClearArrows: Option[() => Unit] = None  // Clear arrows
  private var onShowBestMoveArrow: Option[String => Unit] = None  // Show best move arrow (UCI move)
  private var onUpdateAnalysisPanel: Option[() => Unit] = None  // Update engine analysis panel
  private var onPlayBestLine: Option[(List[String], Boolean, () => Unit) => Unit] = None  // Play PV line (moves, isWhiteToMove, onComplete)
  
  // Current training state
  private var selectedDifficulty: TrainingDifficulty = TrainingDifficulty.Medium
  private var playerName: String = ""
  private var isPlayerTurn: Boolean = true
  
  // Multi-move training state
  private var movesPlayedThisPosition: Int = 0
  private var startingWinProbability: Double = 0.5
  private var currentFEN: String = ""
  private var trainingAnalyzer: Option[GameAnalyzer] = None  // For game analysis during loading
  private var trainingEngine: Option[StockfishEngine] = None  // For evaluating training moves
  private var positionPlayerColor: String = "white"  // Which color player is playing in current position
  
  // === UI Components ===
  
  private val rootPane = new VBox {
    spacing = 12
    padding = Insets(16)
    style = "-fx-background-color: #1e2328;"
    visible <== _isVisible
    managed <== _isVisible
  }
  
  // Header
  private val headerLabel = new Label("Virheiden harjoittelu") {
    font = Font.font("System", FontWeight.Bold, 18)
    style = "-fx-text-fill: #ffffff;"
  }
  
  private val closeButton = new Button("✕") {
    style = "-fx-background-color: transparent; -fx-text-fill: #888888; -fx-font-size: 16px; -fx-cursor: hand;"
    onAction = _ => exitTrainingMode()
  }
  
  private val headerBox = new HBox {
    spacing = 10
    alignment = Pos.CenterLeft
    children = Seq(headerLabel, new Region { HBox.setHgrow(this, Priority.Always) }, closeButton)
  }
  
  // === Setup Phase UI ===
  
  private val playerNameField = new TextField {
    promptText = "Pelaajan nimi (esim. Chess.com käyttäjänimi)"
    style = "-fx-background-color: #2a2f35; -fx-text-fill: white; -fx-prompt-text-fill: #666666;"
  }
  
  private val loadGamesButton = new Button("Lataa PGN-pelit...") {
    style = "-fx-background-color: #81b64c; -fx-text-fill: white; -fx-font-weight: bold; -fx-padding: 10 20; -fx-background-radius: 4;"
    maxWidth = Double.MaxValue
    onAction = _ => loadGames()
  }
  
  private val loadedGamesLabel = new Label("Ei ladattuja pelejä") {
    style = "-fx-text-fill: #888888;"
  }
  
  private val difficultyLabel = new Label("Vaikeustaso:") {
    style = "-fx-text-fill: #cccccc;"
  }
  
  private val difficultyChoice = new ChoiceBox[String] {
    items = javafx.collections.FXCollections.observableArrayList(
      "Helppo (1 siirto)", 
      "Normaali (3 siirtoa)", 
      "Vaikea (5 siirtoa)", 
      "Ekspertti (7 siirtoa)"
    )
    value = "Normaali (3 siirtoa)"
    style = "-fx-background-color: #2a2f35;"
    maxWidth = Double.MaxValue
  }
  
  private val startAnalysisButton = new Button("Analysoi pelit ja etsi virheet") {
    style = "-fx-background-color: #5c8bb0; -fx-text-fill: white; -fx-font-weight: bold; -fx-padding: 10 20; -fx-background-radius: 4;"
    maxWidth = Double.MaxValue
    disable <== _isAnalyzing
    onAction = _ => startAnalysis()
  }
  
  private val setupPane = new VBox {
    spacing = 12
    children = Seq(
      playerNameField,
      loadGamesButton,
      loadedGamesLabel,
      new Separator(),
      difficultyLabel,
      difficultyChoice,
      startAnalysisButton
    )
    visible <== _currentPhase.isEqualTo("setup")
    managed <== _currentPhase.isEqualTo("setup")
  }
  
  // === Analysis Phase UI ===
  
  private val analysisProgressBar = new ProgressBar {
    maxWidth = Double.MaxValue
    progress = 0.0
  }
  
  private val analysisStatusLabel = new Label("Analysoidaan...") {
    style = "-fx-text-fill: #cccccc;"
  }
  
  private val analysisPane = new VBox {
    spacing = 12
    alignment = Pos.Center
    children = Seq(
      new Label("Analysoidaan pelejä...") {
        font = Font.font("System", FontWeight.Bold, 14)
        style = "-fx-text-fill: #ffffff;"
      },
      analysisProgressBar,
      analysisStatusLabel
    )
    visible <== _currentPhase.isEqualTo("analyzing")
    managed <== _currentPhase.isEqualTo("analyzing")
  }
  
  // === Training Phase UI ===
  
  private val positionCountLabel = new Label("") {
    font = Font.font("System", FontWeight.Bold, 14)
    style = "-fx-text-fill: #ffffff;"
  }
  
  private val classificationLabel = new Label("") {
    font = Font.font("System", 12)
  }
  
  private val instructionLabel = new Label("Tee parempi siirto kuin pelissä!") {
    style = "-fx-text-fill: #cccccc; -fx-wrap-text: true;"
    wrapText = true
  }
  
  private val feedbackLabel = new Label("") {
    font = Font.font("System", FontWeight.Bold, 14)
    style = "-fx-text-fill: #81b64c;"
  }
  
  private val hintButton = new Button("Näytä vihje") {
    style = "-fx-background-color: #666666; -fx-text-fill: white; -fx-padding: 8 16; -fx-background-radius: 4;"
    onAction = _ => showHint()
  }
  
  private val showSolutionButton = new Button("Näytä ratkaisu") {
    style = "-fx-background-color: #e6912c; -fx-text-fill: white; -fx-padding: 8 16; -fx-background-radius: 4;"
    onAction = _ => showSolution()
  }
  
  private val nextPositionButton = new Button("Seuraava virhe →") {
    style = "-fx-background-color: #81b64c; -fx-text-fill: white; -fx-font-weight: bold; -fx-padding: 10 20; -fx-background-radius: 4;"
    maxWidth = Double.MaxValue
    visible = false
    onAction = _ => nextPosition()
  }
  
  private val statsLabel = new Label("") {
    style = "-fx-text-fill: #888888; -fx-font-size: 11px;"
  }
  
  private val trainingPane = new VBox {
    spacing = 12
    children = Seq(
      positionCountLabel,
      classificationLabel,
      new Separator(),
      instructionLabel,
      feedbackLabel,
      new HBox {
        spacing = 8
        children = Seq(hintButton, showSolutionButton)
      },
      nextPositionButton,
      new Separator(),
      statsLabel
    )
    visible <== _currentPhase.isEqualTo("training")
    managed <== _currentPhase.isEqualTo("training")
  }
  
  // === Complete Phase UI ===
  
  private val completeStatsLabel = new Label("") {
    style = "-fx-text-fill: #ffffff; -fx-wrap-text: true;"
    wrapText = true
  }
  
  private val restartButton = new Button("Harjoittele uudelleen") {
    style = "-fx-background-color: #81b64c; -fx-text-fill: white; -fx-font-weight: bold; -fx-padding: 10 20; -fx-background-radius: 4;"
    maxWidth = Double.MaxValue
    onAction = _ => restartTraining()
  }
  
  private val newGamesButton = new Button("Lataa uudet pelit") {
    style = "-fx-background-color: #5c8bb0; -fx-text-fill: white; -fx-padding: 10 20; -fx-background-radius: 4;"
    maxWidth = Double.MaxValue
    onAction = _ => resetToSetup()
  }
  
  private val completePane = new VBox {
    spacing = 16
    alignment = Pos.Center
    children = Seq(
      new Label("Harjoittelu valmis!") {
        font = Font.font("System", FontWeight.Bold, 18)
        style = "-fx-text-fill: #81b64c;"
      },
      completeStatsLabel,
      restartButton,
      newGamesButton
    )
    visible <== _currentPhase.isEqualTo("complete")
    managed <== _currentPhase.isEqualTo("complete")
  }
  
  // Build root pane
  rootPane.children = Seq(
    headerBox,
    new Separator(),
    setupPane,
    analysisPane,
    trainingPane,
    completePane
  )
  
  // === Public API ===
  
  def panel: VBox = rootPane
  
  def isVisible: Boolean = _isVisible.value
  def isVisibleProperty: BooleanProperty = _isVisible
  
  def show(): Unit = {
    _isVisible.value = true
    _currentPhase.value = "setup"
  }
  
  def hide(): Unit = {
    _isVisible.value = false
    // Stop training analyzer if running
    trainingAnalyzer.foreach(_.stop())
    trainingAnalyzer = None
    // Stop training engine if running
    trainingEngine.foreach(_.stop())
    trainingEngine = None
  }
  
  def setOnSetupPosition(callback: String => Unit): Unit = {
    onSetupPosition = Some(callback)
  }
  
  def setOnMakeMove(callback: (String, String, Option[String]) => Unit): Unit = {
    onMakeMove = Some(callback)
  }
  
  def setOnGetCurrentFEN(callback: () => String): Unit = {
    onGetCurrentFEN = Some(callback)
  }
  
  def setOnExitTraining(callback: () => Unit): Unit = {
    onExitTraining = Some(callback)
  }
  
  def setOnFlipBoard(callback: Boolean => Unit): Unit = {
    onFlipBoard = Some(callback)
  }
  
  def setOnMakeMoveAnimated(callback: (String, String, Option[String], () => Unit) => Unit): Unit = {
    onMakeMoveAnimated = Some(callback)
  }
  
  def setOnShowArrow(callback: (String, String, String) => Unit): Unit = {
    onShowArrow = Some(callback)
  }
  
  def setOnClearArrows(callback: () => Unit): Unit = {
    onClearArrows = Some(callback)
  }
  
  def setOnShowBestMoveArrow(callback: String => Unit): Unit = {
    onShowBestMoveArrow = Some(callback)
  }
  
  def setOnUpdateAnalysisPanel(callback: () => Unit): Unit = {
    onUpdateAnalysisPanel = Some(callback)
  }
  
  def setOnPlayBestLine(callback: (List[String], Boolean, () => Unit) => Unit): Unit = {
    onPlayBestLine = Some(callback)
  }
  
  /**
   * Called when player makes a move on the board during training.
   * Evaluates the move and handles multi-move training logic.
   */
  def handlePlayerMove(fromSquare: String, toSquare: String, promotion: Option[String] = None): Unit = {
    if (_currentPhase.value != "training" || !isPlayerTurn) return
    
    val promotionSuffix = promotion.getOrElse("")
    val playerMove = s"${fromSquare.toLowerCase}${toSquare.toLowerCase}$promotionSuffix"
    isPlayerTurn = false
    
    // Evaluate this move
    evaluatePlayerMove(playerMove)
  }
  
  // === Private Methods ===
  
  private def loadGames(): Unit = {
    val fileChooser = new FileChooser {
      title = "Valitse PGN-tiedostot"
      extensionFilters.add(
        new FileChooser.ExtensionFilter("PGN Files", "*.pgn")
      )
    }
    
    val files = fileChooser.showOpenMultipleDialog(ownerStage())
    if (files != null && !files.isEmpty) {
      val fileList = files.toArray.toSeq.map(_.asInstanceOf[File])
      
      // Load and prepare games
      val preparer = new GamePreparer()
      loadedGames = preparer.prepareGamesFromFiles(fileList)
      
      val gameCount = loadedGames.size
      loadedGamesLabel.text = s"$gameCount peliä ladattu"
      loadedGamesLabel.style = "-fx-text-fill: #81b64c;"
    }
  }
  
  private def startAnalysis(): Unit = {
    if (loadedGames.isEmpty) {
      loadedGamesLabel.text = "Lataa ensin pelejä!"
      loadedGamesLabel.style = "-fx-text-fill: #e6912c;"
      return
    }
    
    // Get player name and detect color
    playerName = playerNameField.text.value.trim
    
    // Determine difficulty
    selectedDifficulty = difficultyChoice.value.value match {
      case s if s.contains("Helppo") => TrainingDifficulty.Easy
      case s if s.contains("Vaikea") => TrainingDifficulty.Hard
      case s if s.contains("Ekspertti") => TrainingDifficulty.Expert
      case _ => TrainingDifficulty.Medium
    }
    
    _currentPhase.value = "analyzing"
    _isAnalyzing.value = true
    
    // Detect player color from first game if name given
    val detectedColor = if (playerName.nonEmpty && loadedGames.nonEmpty) {
      loadedGames.head.playerColor(playerName).getOrElse("both")
    } else {
      "both"
    }
    
    // Show which color is being analyzed
    if (playerName.nonEmpty) {
      val colorText = detectedColor match {
        case "white" => "valkoisena"
        case "black" => "mustana"
        case _ => "molempina väreinä"
      }
      analysisStatusLabel.text = s"Analysoidaan pelejä ($colorText)..."
    } else {
      analysisStatusLabel.text = "Käynnistetään Stockfish..."
    }
    
    // Run analysis in background thread
    Future {
      // Create a fresh GameAnalyzer with properly started Stockfish engine
      val analyzerOpt = GameAnalyzer.create(EngineConfig(
        depth = 14,
        threads = 4,
        hashSizeMB = 512
      ))
      
      analyzerOpt match {
        case Some(analyzer) =>
          // Create extractor with the fresh analyzer
          val freshExtractor = new ErrorPositionExtractor(analyzer)
          
          freshExtractor.extractErrorsFromMultipleGames(
            loadedGames,
            playerColor = detectedColor,
            depth = 14,
            overallProgress = Some { (current, total) =>
              Platform.runLater {
                analysisProgressBar.progress = current.toDouble / total
                analysisStatusLabel.text = s"Peli $current / $total"
              }
            }
          ).foreach { errors =>
            // Stop the engine after analysis
            analyzer.stop()
            
            Platform.runLater {
              _isAnalyzing.value = false
              
              if (errors.isEmpty) {
                analysisStatusLabel.text = "Ei virheitä löydetty!"
                _currentPhase.value = "setup"
              } else {
                val session = new ErrorTrainingSession()
                session.setDifficulty(selectedDifficulty)
                session.addErrorPositions(errors)
                trainingSession = Some(session)
                _hasSession.value = true
                
                analysisStatusLabel.text = s"Löydettiin ${errors.size} virhettä!"
                startTrainingPhase()
              }
            }
          }
          
        case None =>
          Platform.runLater {
            _isAnalyzing.value = false
            _currentPhase.value = "setup"
            analysisStatusLabel.text = "Virhe: Stockfish ei käynnistynyt!"
            analysisStatusLabel.style = "-fx-text-fill: #fa412d;"
          }
      }
    }
  }
  
  private def startTrainingPhase(): Unit = {
    _currentPhase.value = "training"
    
    // Start/ensure training analyzer is running
    if (trainingAnalyzer.isEmpty) {
      trainingAnalyzer = GameAnalyzer.create(EngineConfig(
        depth = 14,
        threads = 4,
        hashSizeMB = 256
      ))
    }
    
    loadCurrentPosition()
  }
  
  private def loadCurrentPosition(): Unit = {
    trainingSession.flatMap(_.currentPosition) match {
      case Some(pos) =>
        val session = trainingSession.get
        val current = session.currentPositionIndex + 1
        val total = session.totalPositions
        
        positionCountLabel.text = s"Virhe $current / $total"
        
        // Set classification label with color
        val (classText, classColor) = pos.classification match {
          case MoveClassification.Blunder => ("Munaus", "#fa412d")
          case MoveClassification.Mistake => ("Virhe", "#ffa459")
          case MoveClassification.Inaccuracy => ("Epätarkkuus", "#f7c631")
          case _ => ("Virhe", "#cccccc")
        }
        classificationLabel.text = classText
        classificationLabel.style = s"-fx-text-fill: $classColor; -fx-font-weight: bold;"
        
        // Set instructions based on difficulty
        val movesText = if (selectedDifficulty.movesToPlay == 1) {
          "Tee vähintään 'Hyvä' siirto!"
        } else {
          s"Pelaa ${selectedDifficulty.movesToPlay} siirtoa pudottamatta voittotodennäköisyyttä yli 15%."
        }
        instructionLabel.text = movesText
        
        feedbackLabel.text = ""
        nextPositionButton.visible = false
        hintButton.disable = false
        showSolutionButton.disable = false
        isPlayerTurn = true
        
        // Reset multi-move state
        movesPlayedThisPosition = 0
        positionPlayerColor = pos.playerColor
        currentFEN = pos.fen
        
        // Calculate starting win probability using training engine
        val engine = trainingEngine.getOrElse {
          val config = EngineConfig(depth = 14, threads = 4, hashSizeMB = 256)
          val newEngine = new StockfishEngine(config)
          if (newEngine.start()) {
            trainingEngine = Some(newEngine)
            newEngine
          } else {
            null
          }
        }
        
        if (engine != null) {
          analyzePositionAsync(engine, pos.fen, 10).foreach { result =>
            Platform.runLater {
              startingWinProbability = evalToWinProbability(result.eval, positionPlayerColor == "white")
            }
          }
        }
        
        updateStats()
        
        // Flip board if player is black (so player plays from bottom)
        onFlipBoard.foreach(_(positionPlayerColor == "black"))
        
        // Clear any existing arrows
        onClearArrows.foreach(_())
        
        // Set position on board
        onSetupPosition.foreach(_(pos.fen))
        
        // Show previous move with animation and arrow if available
        pos.previousMove.foreach { prevMove =>
          if (prevMove.length >= 4) {
            val from = prevMove.substring(0, 2)
            val to = prevMove.substring(2, 4)
            val promotion = if (prevMove.length > 4) Some(prevMove.substring(4, 5)) else None
            
            // Show arrow for the previous move (yellow/orange color)
            onShowArrow.foreach(_(from, to, "#f7c631"))
            
            // Small delay to let board render, then animate
            Future {
              Thread.sleep(200)
              Platform.runLater {
                // Play the previous move with animation
                onMakeMoveAnimated match {
                  case Some(animatedMove) =>
                    animatedMove(from, to, promotion, () => {
                      // After animation, update analysis panel
                      onUpdateAnalysisPanel.foreach(_())
                    })
                  case None =>
                    onMakeMove.foreach(_(from, to, promotion))
                    onUpdateAnalysisPanel.foreach(_())
                }
              }
            }
          }
        }
        
        // If no previous move, just update analysis panel
        if (pos.previousMove.isEmpty) {
          onUpdateAnalysisPanel.foreach(_())
        }
        
      case None =>
        // No more positions
        showComplete()
    }
  }
  
  /**
   * Evaluate a player's move using Stockfish.
   * For Easy mode: Check if move is Good or better.
   * For other modes: Check win probability change.
   */
  private def evaluatePlayerMove(@annotation.unused playerMove: String): Unit = {
    feedbackLabel.text = "Arvioidaan siirtoa..."
    feedbackLabel.style = "-fx-text-fill: #888888;"
    
    // Get or create training engine
    val engine = trainingEngine.getOrElse {
      val config = EngineConfig(depth = 14, threads = 4, hashSizeMB = 256)
      val newEngine = new StockfishEngine(config)
      if (!newEngine.start()) {
        feedbackLabel.text = "Virhe: Stockfish ei käynnisty"
        feedbackLabel.style = "-fx-text-fill: #fa412d;"
        isPlayerTurn = true
        return
      }
      trainingEngine = Some(newEngine)
      newEngine
    }
    
    // Get FEN after the player's move
    val fenAfterMove = onGetCurrentFEN.map(_()).getOrElse(currentFEN)
    
    // Analyze position after player's move to get evaluation and Stockfish's best response
    analyzePositionAsync(engine, fenAfterMove, 14).foreach { result =>
      Platform.runLater {
        // Calculate win probability change - ALWAYS compare to starting position
        val wpAfter = evalToWinProbability(result.eval, positionPlayerColor == "white")
        val wpDrop = startingWinProbability - wpAfter
        
        movesPlayedThisPosition += 1
        currentFEN = fenAfterMove
        
        // Update analysis panel
        onUpdateAnalysisPanel.foreach(_())
        
        // Check if move is acceptable based on difficulty
        val isGoodMove = if (!selectedDifficulty.useWinProbability) {
          // Easy mode: classify the move
          val classification = classifyMove(wpDrop)
          isClassificationAcceptable(classification, selectedDifficulty.minClassification)
        } else {
          // Other modes: check win probability drop from STARTING position
          wpDrop <= selectedDifficulty.maxWinProbabilityDrop
        }
        
        if (!isGoodMove) {
          // Bad move - show opponent's best continuation to explain why
          // The PV from result shows the best continuation AFTER player's move
          handleBadMove(wpDrop, result.pv)
        } else {
          // Good move - check if we need more moves or if we're done
          handleGoodMove(wpDrop, result.bestMove)
        }
      }
    }
  }
  
  /**
   * Analyze a position asynchronously using the training engine.
   */
  private def analyzePositionAsync(engine: StockfishEngine, fen: String, depth: Int): Future[AnalysisResultSimple] = {
    val promise = Promise[AnalysisResultSimple]()
    
    // Set up callback for analysis result
    val callback: StockfishEngine#EngineAnalysis => Unit = analysis => {
      if (analysis.bestMove.isDefined) {
        val eval = if (analysis.mate.isDefined) {
          PositionEvaluation.MateIn(analysis.mate.get)
        } else {
          PositionEvaluation.Centipawns(analysis.score.getOrElse(0).toDouble)
        }
        // Include the PV (principal variation) for showing best line on bad moves
        promise.trySuccess(AnalysisResultSimple(eval, analysis.bestMove, analysis.pv))
      }
    }
    
    engine.addAnalysisCallback(callback)
    engine.analyzePosition(fen, depth, 1)
    
    // Timeout fallback
    Future {
      Thread.sleep(10000)  // 10 second timeout
      promise.trySuccess(AnalysisResultSimple(PositionEvaluation.Centipawns(0), None, List.empty))
    }
    
    promise.future
  }
  
  /** Simple case class for analysis results */
  private case class AnalysisResultSimple(eval: PositionEvaluation, bestMove: Option[String], pv: List[String] = List.empty)
  
  /**
   * Handle a good move - either continue or complete the position.
   */
  private def handleGoodMove(@annotation.unused wpDrop: Double, stockfishBestMove: Option[String]): Unit = {
    val movesRequired = selectedDifficulty.movesToPlay
    
    if (movesPlayedThisPosition >= movesRequired) {
      // Position completed successfully!
      trainingSession.foreach(_.recordSimpleAttempt(correct = true))
      
      feedbackLabel.text = s"Erinomaista! Suoritit $movesRequired siirtoa onnistuneesti!"
      feedbackLabel.style = "-fx-text-fill: #81b64c;"
      nextPositionButton.visible = true
      updateStats()
    } else {
      // Need more moves - Stockfish responds
      val remaining = movesRequired - movesPlayedThisPosition
      feedbackLabel.text = s"Hyvä siirto! Vielä $remaining siirtoa jäljellä."
      feedbackLabel.style = "-fx-text-fill: #81b64c;"
      
      // Stockfish makes its move
      stockfishBestMove match {
        case Some(sfMove) if sfMove.length >= 4 =>
          val from = sfMove.substring(0, 2)
          val to = sfMove.substring(2, 4)
          val promotion = if (sfMove.length > 4) Some(sfMove.substring(4, 5)) else None
          
          // Small delay before Stockfish moves with animation
          Future {
            Thread.sleep(500)
            Platform.runLater {
              onMakeMoveAnimated match {
                case Some(animatedMove) =>
                  animatedMove(from, to, promotion, () => {
                    // After animation completes
                    currentFEN = onGetCurrentFEN.map(_()).getOrElse(currentFEN)
                    feedbackLabel.text = s"Sinun vuorosi. Vielä $remaining siirtoa."
                    feedbackLabel.style = "-fx-text-fill: #cccccc;"
                    isPlayerTurn = true
                    onUpdateAnalysisPanel.foreach(_())
                  })
                case None =>
                  // Fallback to non-animated
                  onMakeMove.foreach(_(from, to, promotion))
                  Future {
                    Thread.sleep(300)
                    Platform.runLater {
                      currentFEN = onGetCurrentFEN.map(_()).getOrElse(currentFEN)
                      feedbackLabel.text = s"Sinun vuorosi. Vielä $remaining siirtoa."
                      feedbackLabel.style = "-fx-text-fill: #cccccc;"
                      isPlayerTurn = true
                      onUpdateAnalysisPanel.foreach(_())
                    }
                  }
              }
            }
          }
          
        case _ =>
          // No Stockfish move available - position might be finished
          feedbackLabel.text = "Sinun vuorosi."
          isPlayerTurn = true
      }
    }
  }
  
  /**
   * Handle a bad move - show why it was bad by playing opponent's best continuation.
   * Then reset position for retry.
   */
  private def handleBadMove(wpDrop: Double, bestResponse: List[String]): Unit = {
    val dropPercent = (wpDrop * 100).toInt
    feedbackLabel.text = s"Liian heikko siirto (pudotus ${dropPercent}%). Katsotaan miksi..."
    feedbackLabel.style = "-fx-text-fill: #e6912c;"
    isPlayerTurn = false
    
    // Determine who is to move (opponent responds to player's move)
    val isWhiteToMove = positionPlayerColor != "white"  // Opponent's turn
    
    // Play the opponent's best continuation to show why the move was bad
    if (bestResponse.nonEmpty) {
      val movesToShow = bestResponse.take(4)  // Show up to 4 half-moves
      
      onPlayBestLine match {
        case Some(playLine) =>
          feedbackLabel.text = s"Vastustajan paras jatko (${movesToShow.length} siirtoa)..."
          playLine(movesToShow, isWhiteToMove, () => {
            // After best line is shown, reset to original position
            Platform.runLater {
              feedbackLabel.text = "Yritä uudelleen!"
              feedbackLabel.style = "-fx-text-fill: #e6912c;"
              
              Future {
                Thread.sleep(1000)  // Brief pause before reset
                Platform.runLater {
                  movesPlayedThisPosition = 0
                  trainingSession.flatMap(_.currentPosition).foreach { pos =>
                    onClearArrows.foreach(_())
                    onSetupPosition.foreach(_(pos.fen))
                    currentFEN = pos.fen
                    onUpdateAnalysisPanel.foreach(_())
                  }
                  isPlayerTurn = true
                }
              }
            }
          })
        case None =>
          // Fallback: just show arrow and reset
          bestResponse.headOption.foreach { move =>
            if (move.length >= 4) {
              val from = move.substring(0, 2)
              val to = move.substring(2, 4)
              onShowArrow.foreach(_(from, to, "#5dade2"))
            }
          }
          
          Future {
            Thread.sleep(1500)
            Platform.runLater {
              movesPlayedThisPosition = 0
              trainingSession.flatMap(_.currentPosition).foreach { pos =>
                onClearArrows.foreach(_())
                onSetupPosition.foreach(_(pos.fen))
                currentFEN = pos.fen
              }
              isPlayerTurn = true
            }
          }
      }
    } else {
      // No best response available, just reset
      Future {
        Thread.sleep(1500)
        Platform.runLater {
          movesPlayedThisPosition = 0
          trainingSession.flatMap(_.currentPosition).foreach { pos =>
            onClearArrows.foreach(_())
            onSetupPosition.foreach(_(pos.fen))
            currentFEN = pos.fen
          }
          isPlayerTurn = true
        }
      }
    }
  }
  
  /**
   * Convert centipawn evaluation to win probability (0.0 to 1.0).
   * Uses the standard formula: wp = 1 / (1 + 10^(-cp/400))
   */
  private def evalToWinProbability(eval: chess.analysis.PositionEvaluation, isWhite: Boolean): Double = {
    val cp = eval.toNumeric * 100  // Convert to centipawns
    val adjustedCp = if (isWhite) cp else -cp
    1.0 / (1.0 + Math.pow(10, -adjustedCp / 400.0))
  }
  
  /**
   * Classify a move based on win probability drop.
   */
  private def classifyMove(wpDrop: Double): MoveClassification = {
    if (wpDrop <= 0.02) MoveClassification.Best
    else if (wpDrop <= 0.05) MoveClassification.Excellent
    else if (wpDrop <= 0.10) MoveClassification.Good
    else if (wpDrop <= 0.15) MoveClassification.Inaccuracy
    else if (wpDrop <= 0.25) MoveClassification.Mistake
    else MoveClassification.Blunder
  }
  
  /**
   * Check if a move classification meets the minimum requirement.
   */
  private def isClassificationAcceptable(actual: MoveClassification, minimum: MoveClassification): Boolean = {
    val order = List(
      MoveClassification.Best,
      MoveClassification.Excellent, 
      MoveClassification.Good,
      MoveClassification.Inaccuracy,
      MoveClassification.Mistake,
      MoveClassification.Blunder
    )
    order.indexOf(actual) <= order.indexOf(minimum)
  }
  
  private def showHint(): Unit = {
    trainingSession.flatMap(_.currentPosition) match {
      case Some(pos) =>
        val bestMove = pos.bestMove
        if (bestMove.length >= 2) {
          val startSquare = bestMove.take(2).toUpperCase
          feedbackLabel.text = s"Vihje: Katso ruutua $startSquare"
          feedbackLabel.style = "-fx-text-fill: #5c8bb0;"
        }
        hintButton.disable = true
        
      case None =>
    }
  }
  
  private def showSolution(): Unit = {
    trainingSession.foreach { session =>
      session.recordSimpleAttempt(correct = false)
      
      session.currentPosition match {
        case Some(pos) =>
          val bestMove = pos.bestMove
          feedbackLabel.text = s"Paras siirto oli: ${formatMove(bestMove)}"
          feedbackLabel.style = "-fx-text-fill: #fa412d;"
          nextPositionButton.visible = true
          showSolutionButton.disable = true
          hintButton.disable = true
          isPlayerTurn = false
          updateStats()
          
        case None =>
      }
    }
  }
  
  private def nextPosition(): Unit = {
    trainingSession.foreach(_.nextPosition())
    
    if (trainingSession.exists(_.currentPosition.isEmpty)) {
      showComplete()
    } else {
      loadCurrentPosition()
    }
  }
  
  private def updateStats(): Unit = {
    trainingSession.foreach { session =>
      val stats = session.getStats
      statsLabel.text = s"Oikein: ${stats.correctCount} / ${stats.totalAttempts} (${stats.successRate.toInt}%)"
    }
  }
  
  private def showComplete(): Unit = {
    _currentPhase.value = "complete"
    
    trainingSession.foreach { session =>
      val stats = session.getStats
      completeStatsLabel.text = 
        s"""Tulokset:
           |
           |Oikein: ${stats.correctCount} / ${stats.totalAttempts}
           |Onnistumisprosentti: ${stats.successRate.toInt}%
           |
           |Virheitä harjoiteltu: ${stats.positionsCompleted}""".stripMargin
    }
  }
  
  private def restartTraining(): Unit = {
    trainingSession.foreach(_.reset())
    startTrainingPhase()
  }
  
  private def resetToSetup(): Unit = {
    loadedGames = Seq.empty
    trainingSession = None
    _hasSession.value = false
    loadedGamesLabel.text = "Ei ladattuja pelejä"
    loadedGamesLabel.style = "-fx-text-fill: #888888;"
    _currentPhase.value = "setup"
  }
  
  private def exitTrainingMode(): Unit = {
    hide()
    onExitTraining.foreach(_())
  }
  
  private def formatMove(uciMove: String): String = {
    if (uciMove.length >= 4) {
      val from = uciMove.take(2).toUpperCase
      val to = uciMove.slice(2, 4).toUpperCase
      s"$from → $to"
    } else {
      uciMove
    }
  }
}
