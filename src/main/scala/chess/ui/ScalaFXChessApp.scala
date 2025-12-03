package chess.ui

import chess.analysis.{StockfishEngine, GameAnalyzer, EngineConfig, FENGenerator}
import chess.board.{Board, Square}
import chess.controllers.{GameController, Move}
import chess.pgn.{PGNManager, PGNMove}
import chess.pieces.{Pawn => PawnPiece, Queen => QueenPiece, Rook => RookPiece, Bishop => BishopPiece, Knight => KnightPiece, Piece}
import chess.types.PieceType
import scalafx.Includes._
import scalafx.application.{JFXApp3, Platform}
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.input.{KeyCode, KeyEvent}
import scalafx.scene.control.{Button, CheckMenuItem, Label, Menu, MenuBar, MenuItem, ScrollPane, SeparatorMenuItem}
import scalafx.scene.layout.{BorderPane, HBox, StackPane, VBox}
import scalafx.stage.FileChooser
import javafx.scene.control.{Label => JfxLabel}
import javafx.scene.layout.{Region => JfxRegion}
import java.io.File
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import scala.collection.mutable.Map as MutableMap

/**
 * ScalaFX chess application with automatic variation support.
 * Variations are created automatically when making moves while viewing history.
 * Visual arrows show available continuations at branch points.
 */
object ScalaFXChessApp extends JFXApp3 {

  // --- Core chess state -----------------------------------------------------
  private val controller = new GameController()
  private val boardModel = new Board(controller)
  controller.board = Some(boardModel)

  // --- Variation tracking (Move-based) --------------------------------------
  // All variations are stored in MoveHistoryManager as Move objects with isVariation=true.
  // Navigation uses viewingVariationRootTob to track which variation we're viewing.

  // Move-based variation selection state
  private case class VariationSelectionState(
    anchorIndex: Int,
    variations: Vector[Move],  // Variation root moves
    selectedIndex: Int
  )

  private var variationSelectionState: Option[VariationSelectionState] = None
  private var showMoveArrows: Boolean = true
  
  // === Move-based variation tracking (migration in progress) ===
  // The TOB of the variation root we're currently viewing (replaces viewingVariation)
  private var viewingVariationRootTob: Option[Long] = None
  // Current Move within the variation we're viewing
  private var currentVariationMove: Option[Move] = None

  // Board flip state
  private var isBoardFlipped: Boolean = false

  // Menu items (initialized in buildMenuBar)
  private var showMoveArrowsMenuItem: CheckMenuItem = scala.compiletime.uninitialized
  private var flipBoardMenuItem: CheckMenuItem = scala.compiletime.uninitialized

  // Navigation state for viewing history
  private var currentMoveIndex: Int = -1
  private var isViewingHistory: Boolean = false
  
  // Analysis classifications for move history color coding
  private var moveClassifications: Map[Int, MoveClassificationInfo] = Map.empty
  
  // Store analysis results for engine move arrows in review mode
  private var gameAnalysisResults: List[chess.analysis.MoveAnalysis] = List.empty
  private var gameAnalysisFens: List[String] = List.empty  // FENs for navigation in review mode
  private var gameAnalysisMoves: List[Move] = List.empty  // Moves for review mode
  private var isInReviewMode: Boolean = false
  
  // Best Response preview state - shows engine's best line without creating variations
  private var bestResponsePreviewActive: Boolean = false
  private var bestResponsePreviewIndex: Int = -1  // Original move index before preview
  
  // Live engine arrows (stored separately so they can be combined with variation arrows)
  private var currentEngineArrows: List[ScalaFXBoardView.MoveArrow] = List.empty

  // Notification keys
  private val VariationPickerKey = "variation-picker"
  private val BranchSelectionKey = "branch-selection"

  // --- UI building blocks ---------------------------------------------------
  private lazy val boardView = new ScalaFXBoardView()
  private lazy val moveHistoryPanel = new ScalaFXMoveHistoryPanel()
  private lazy val notificationCenter = new NotificationCenter()
  private lazy val stockfishEngine = new StockfishEngine()
  private lazy val analysisManager = new AnalysisManager(stockfishEngine, boardModel, controller, notificationCenter)
  private lazy val gameReviewPanel = new GameReviewPanel()
  private lazy val analysisProgressOverlay = new AnalysisProgressOverlay()
  private lazy val navigationHandler = new ScalaFXNavigationHandler(
    controller = controller,
    board = boardModel,
    onAfterRebuild = () => boardView.refreshFromModel()
  )
  private lazy val pgnManager = new PGNManager(controller, boardModel)
  private lazy val moveArrowManager = new MoveArrowManager(controller, boardModel, boardView)
  private lazy val moveReplayer = new MoveReplayer(controller, boardModel, moveArrowManager)
  private lazy val branchOverlayBuilder = new BranchOverlayBuilder(controller)
  private lazy val fileOperations = new FileOperationsHandler(
    pgnManager, analysisManager, notificationCenter, () => stage
  )
  private lazy val variationNavigator = new VariationNavigator(controller, boardModel, moveReplayer)
  private lazy val keyboardNavHandler = new KeyboardNavigationHandler(controller)
  private lazy val errorTrainingPanel = new ErrorTrainingPanel(
    () => stage
  )
  
  // Training mode state
  private var isInTrainingMode: Boolean = false
  private var trainingSelectedSquare: Option[(Int, Int)] = None

  private lazy val statusLabel = new Label("White to move") {
    style = "-fx-text-fill: #e0e0e0; -fx-font-size: 16px; -fx-font-weight: bold;"
  }

  // --- Mutable UI state -----------------------------------------------------
  private var selectedSquareCoord: Option[(Int, Int)] = None

  // -------------------------------------------------------------------------
  // Variation helper methods
  // -------------------------------------------------------------------------

  private def startingMoveNumberFromAnchor(anchorIndex: Int): Int =
    ((anchorIndex + 1) / 2) + 1

  private def startingColorFromAnchor(anchorIndex: Int): String =
    if ((anchorIndex + 1) % 2 == 0) "white" else "black"

  /** Get variation root Moves for a given anchor index (Move-based) */
  private def variationsForAnchorMoves(anchorIndex: Int): Vector[Move] = {
    // When viewing mainline, only show root-level variations
    // When viewing a variation, show its children at the branch point
    val result = viewingVariationRootTob match
      case None =>
        // In mainline: get variation roots at this mainline index
        val roots = controller.moveHistoryManager.getVariationRootsAtMainlineIndex(anchorIndex).toVector
        roots
      case Some(parentRootTob) =>
        // In variation: calculate index within current variation from global anchorIndex
        // We need to account for ALL ancestor moves in the combined display
        
        // Helper to collect ancestor chain
        def collectAncestorChain(varRootTob: Long): List[(Long, Int)] =
          controller.moveHistoryManager.getParentVariationRootTob(varRootTob) match
            case Some(ancestorRootTob) =>
              val branchIdx = controller.moveHistoryManager.getParentVariationBranchIndex(varRootTob)
              collectAncestorChain(ancestorRootTob) :+ (varRootTob, branchIdx)
            case None =>
              List((varRootTob, -1))
        
        val ancestorChain = collectAncestorChain(parentRootTob)
        val mainlineAnchorIdx = controller.moveHistoryManager.getMainlineAnchorIndexForVariation(parentRootTob)
        
        // Calculate currentOffset - how many moves precede the current variation in combined display
        // Start with mainline moves up to anchor
        var currentOffset = mainlineAnchorIdx + 1
        
        // Add ancestor variation moves up to their branch points
        ancestorChain.dropRight(1).foreach { case (ancestorRootTob, _) =>
          // Find where the next level branches from this ancestor
          val nextAncestorIdx = ancestorChain.indexWhere(_._1 == ancestorRootTob) + 1
          val branchIdxInAncestor = if nextAncestorIdx < ancestorChain.size then
            ancestorChain(nextAncestorIdx)._2
          else 0
          
          currentOffset += branchIdxInAncestor + 1
        }
        
        val parentLine = controller.moveHistoryManager.collectVariationLine(parentRootTob)
        
        // Calculate index within parent variation: anchorIndex - currentOffset
        val idxInParent = anchorIndex - currentOffset
        
        if idxInParent >= 0 && idxInParent < parentLine.size then
          controller.moveHistoryManager.getNestedVariationRootsAt(parentRootTob, idxInParent).toVector
        else
          Vector.empty
    result
  }

  private def variationIndicatorsForDisplay(): Map[Int, List[String]] = {
    val combined = MutableMap.empty[Int, List[String]]

    // Get variation indicators - use nested variations if viewing a variation
    val indicators = viewingVariationRootTob match {
      case Some(parentRootTob) =>
        // Inside a variation - get nested variation indicators
        controller.moveHistoryManager.childVariationIndicatorsFromMoves(parentRootTob)
      case None =>
        // In mainline - use Move tree for root variation indicators
        controller.moveHistoryManager.variationIndicatorsFromMoves()
    }
    
    // Keep index -1 for starting position variations, don't normalize to 0
    indicators.foreach { case (key, previews) =>
      combined.update(key, previews)
    }

    combined.toMap
  }

  private def currentAnchorIndex: Int = {
    val historySize = controller.moveHistoryManager.getMoveHistory.size
    if isViewingHistory then currentMoveIndex
    else historySize - 1
  }

  // -------------------------------------------------------------------------
  // Move arrows for visualizing variations (delegated to MoveArrowManager)
  // -------------------------------------------------------------------------

  private def updateMoveArrows(): Unit = {
    val anchorIndex = currentAnchorIndex

    variationSelectionState match
      case Some(state) if state.anchorIndex != anchorIndex =>
        clearVariationSelectionState()
      case _ =>

    val variations = refreshBranchSelectionForAnchor(anchorIndex)

    val selectedIndexOpt = variationSelectionState.collect {
      case state if state.anchorIndex == anchorIndex => state.selectedIndex
    }

    moveArrowManager.updateArrows(
      viewingVariationRootTob = viewingVariationRootTob,
      currentVariationMove = currentVariationMove,
      anchorIndex = anchorIndex,
      variations = variations,
      selectedIndex = selectedIndexOpt,
      engineArrows = currentEngineArrows
    )

    updateVariationStatusLabel()
  }

  private def setMoveArrowsEnabled(enabled: Boolean): Unit = {
    showMoveArrows = enabled
    moveArrowManager.setEnabled(enabled)
    if showMoveArrowsMenuItem != null then showMoveArrowsMenuItem.selected = enabled
    updateMoveArrows()
  }
  
  /**
   * Update engine arrows from LIVE analysis.
   * Called by AnalysisManager callback when engine produces new analysis.
   * Shows top 3 moves with varying thickness.
   * Works in both normal mode and review mode when analysis is enabled.
   * Stores arrows in currentEngineArrows and triggers updateMoveArrows to combine them.
   */
  private def updateLiveEngineArrows(topMoves: List[String]): Unit = {
    // Only show if analysis is enabled
    if !analysisManager.isAnalysisActive then
      currentEngineArrows = List.empty
      updateMoveArrows()  // Refresh to clear engine arrows
      return
    
    val effectiveCell = if boardView.cellSize > 0 then boardView.cellSize else 64.0
    
    import ScalaFXBoardView.MoveArrow
    
    // Engine arrow colors - different shades for each rank
    val arrowColors = List(
      scalafx.scene.paint.Color.web("#f5a623"),  // Orange - best move
      scalafx.scene.paint.Color.web("#e88912"),  // Darker orange - 2nd best
      scalafx.scene.paint.Color.web("#d66d00")   // Even darker - 3rd best
    )
    
    // Arrow widths: thick (best), medium (2nd), thin (3rd)
    val arrowWidths = List(
      math.max(effectiveCell * 0.16, 7.0),
      math.max(effectiveCell * 0.12, 5.0),
      math.max(effectiveCell * 0.08, 3.5)
    )
    
    val arrowOpacities = List(0.95, 0.85, 0.70)
    
    currentEngineArrows = topMoves.take(3).zipWithIndex.flatMap { case (moveUCI, idx) =>
      parseUCIMove(moveUCI).map { case (fromRow, fromCol, toRow, toCol) =>
        MoveArrow(
          fromRow = fromRow, fromCol = fromCol,
          toRow = toRow, toCol = toCol,
          color = arrowColors.lift(idx).getOrElse(arrowColors.last),
          opacity = arrowOpacities.lift(idx).getOrElse(arrowOpacities.last),
          width = arrowWidths.lift(idx).getOrElse(arrowWidths.last)
        )
      }
    }
    
    updateMoveArrows()  // Refresh to include engine arrows
  }
  
  /**
   * Parse UCI move notation (e.g., "e2e4") to board coordinates.
   * Returns (fromRow, fromCol, toRow, toCol) in 0-7 indices.
   */
  private def parseUCIMove(uci: String): Option[(Int, Int, Int, Int)] = {
    if uci.length < 4 then return None
    val fromFile = uci.charAt(0) - 'a'
    val fromRank = uci.charAt(1) - '1'
    val toFile = uci.charAt(2) - 'a'
    val toRank = uci.charAt(3) - '1'
    
    if fromFile < 0 || fromFile > 7 || fromRank < 0 || fromRank > 7 ||
       toFile < 0 || toFile > 7 || toRank < 0 || toRank > 7 then
      None
    else
      // Convert to row/col (row 0 = rank 8, row 7 = rank 1)
      Some((7 - fromRank, fromFile, 7 - toRank, toFile))
  }

  // -------------------------------------------------------------------------
  // Variation selection state management
  // -------------------------------------------------------------------------

  private def clearVariationSelectionState(): Unit = {
    variationSelectionState = None
    notificationCenter.clear(BranchSelectionKey)
    updateVariationStatusLabel()
  }

  private def refreshVariationSelectionStateMoves(anchorIndex: Int, variations: Vector[Move]): Unit = {
    if variations.isEmpty then
      variationSelectionState match
        case Some(_) =>
          clearVariationSelectionState()
        case None =>
          updateVariationStatusLabel()
    else
      val updatedState = variationSelectionState match
        case Some(state) if state.anchorIndex == anchorIndex =>
          val clamped =
            if state.selectedIndex < 0 then state.selectedIndex  // Keep -1 or -99 as is
            else math.min(state.selectedIndex, variations.size - 1)
          state.copy(variations = variations, selectedIndex = clamped)
        case _ =>
          // No existing selection for this anchor - start with -99 (no selection yet)
          // This means user must press ↑/↓ to select before → will act
          VariationSelectionState(anchorIndex, variations, -99)
      variationSelectionState = Some(updatedState)
      updateVariationStatusLabel()
  }

  private def refreshBranchSelectionForAnchor(anchorIndex: Int): Vector[Move] = {
    val variations = variationsForAnchorMoves(anchorIndex)
    refreshVariationSelectionStateMoves(anchorIndex, variations)
    updateBranchSelectionOverlayMoves(anchorIndex, variations)
    variations
  }

  private def cycleVariationSelection(delta: Int): Boolean = {
    // Determine the correct anchor based on context (using Move-based tracking)
    val (anchor, variations) = viewingVariationRootTob match {
      case Some(parentRootTob) =>
        // Inside a variation - use index in parent line and get nested variation roots
        val parentLine = controller.moveHistoryManager.collectVariationLine(parentRootTob)
        val currentIdx = currentVariationMove.map(m => parentLine.indexWhere(_.tob == m.tob)).getOrElse(-1)
        val nestedVariations = if currentIdx >= 0 then
          controller.moveHistoryManager.getNestedVariationRootsAt(parentRootTob, currentIdx).toVector
        else
          Vector.empty
        (currentIdx, nestedVariations)
      case None =>
        // In mainline - use currentAnchorIndex
        (currentAnchorIndex, variationsForAnchorMoves(currentAnchorIndex))
    }
    
    if variations.isEmpty then
      clearVariationSelectionState()
      false
    else
      val options = -1 +: (0 until variations.size)
      val rawSelection = variationSelectionState
        .filter(_.anchorIndex == anchor)
        .map(_.selectedIndex)
        .getOrElse(-99)
      
      // If no selection yet (-99), start at the beginning based on direction
      // ↓ (delta >= 0) -> start at first option (-1 = mainline)
      // ↑ (delta < 0) -> start at last option (last variation)
      val currentSelection = if rawSelection == -99 then
        if delta >= 0 then -1 else options.last  // Start cycling from appropriate end
      else if options.contains(rawSelection) then
        rawSelection
      else
        -1  // Fallback

      val direction = if delta >= 0 then 1 else -1
      val currentPos = options.indexOf(currentSelection)
      val nextPos = (currentPos + direction + options.length) % options.length
      val nextSelection = options(nextPos)

      variationSelectionState = Some(VariationSelectionState(anchor, variations, nextSelection))
      updateVariationStatusLabel()
      // Update the overlay to reflect the new selection (don't call updateMoveArrows which clears it)
      showBranchSelectionOverlayMoves(anchor, variations)
      true
  }

  private def updateVariationStatusLabel(): Unit = {
    val anchor = currentAnchorIndex
    val variationsAtAnchor =
      if isViewingHistory then variationsForAnchorMoves(anchor)
      else Vector.empty

    val selectionOpt = variationSelectionState.filter(_.anchorIndex == anchor)

    val message =
      if !isViewingHistory || variationsAtAnchor.isEmpty then ""
      else selectionOpt match
        case Some(state) if state.selectedIndex >= 0 && state.selectedIndex < state.variations.size =>
          val selection = state.variations(state.selectedIndex)
          val preview = controller.moveHistoryManager.getVariationPreview(selection.tob, 5)
          val indexText = s"${state.selectedIndex + 1}/${state.variations.size}"
          s"Variation $indexText selected: $preview\n(↑/↓ to browse, → to resume editing)"
        case _ =>
          val count = variationsAtAnchor.size
          val plural = if count == 1 then "" else "s"
          s"Mainline selected – $count saved variation$plural available (↑/↓ to browse, → to continue)."

    moveHistoryPanel.updateStatus(message)
  }

  // -------------------------------------------------------------------------
  // Variation picker prompt window
  // -------------------------------------------------------------------------

  private def showVariationPicker(moveIndex: Int): Unit = {
    println(s"DEBUG showVariationPicker called: moveIndex=$moveIndex")
    val anchorsToCheck =
      if moveIndex == 0 then Set(moveIndex, -1)
      else Set(moveIndex)

    // Use Move-based API for variations
    val variationsByAnchor: Seq[(Int, Vector[Move])] = {
      import scala.collection.mutable.LinkedHashMap
      val ordered = LinkedHashMap.empty[Int, Vector[Move]]

      anchorsToCheck.toSeq.sorted.foreach { anchor =>
        val anchorVariations = variationsForAnchorMoves(anchor)
        println(s"DEBUG showVariationPicker: anchor=$anchor, anchorVariations.size=${anchorVariations.size}")
        if anchorVariations.nonEmpty then
          val existing = ordered.getOrElse(anchor, Vector.empty)
          ordered.update(anchor, existing ++ anchorVariations)
      }

      ordered.toSeq
    }
    println(s"DEBUG showVariationPicker: variationsByAnchor.size=${variationsByAnchor.size}")

    if variationsByAnchor.isEmpty then
      notificationCenter.showInfo(
        key = VariationPickerKey,
        title = "Variations",
        message = "No stored variations for this move yet."
      )
    else
      val anchorDescription =
        if moveIndex <= 0 then "Starting position"
        else
          val moveNumber = ((moveIndex + 1) / 2) + 1
          val colourText = if ((moveIndex + 1) % 2 == 0) "White" else "Black"
          s"$colourText to move after move $moveNumber"

      val content = new VBox {
        spacing = 10
      }

      // Show saved variations
      content.children.add(new Label("Saved variations:") {
        style = "-fx-text-fill: #f5f5f5; -fx-font-weight: bold;"
      })

      variationsByAnchor.foreach { (anchor, anchorVariations) =>
        val anchorHeading =
          if anchor < 0 then "• Start position"
          else
            val moveNo = startingMoveNumberFromAnchor(anchor)
            val colour = startingColorFromAnchor(anchor)
            val colourText = colour.capitalize
            s"• $colourText to move after move $moveNo"

        content.children.add(new Label(anchorHeading) {
          style = "-fx-text-fill: #cfd8dc; -fx-font-size: 12px;"
        })

        anchorVariations.zipWithIndex.foreach { (variationRoot, idx) =>
          val preview = controller.moveHistoryManager.getVariationPreview(variationRoot.tob, 8)
          val isSelected = variationSelectionState.exists(state =>
            state.anchorIndex == anchor && state.selectedIndex == idx
          )

            val headerLabel = new Label(preview) {
              wrapText = true
              style =
                if isSelected then "-fx-text-fill: #ffffff; -fx-font-weight: bold;"
                else "-fx-text-fill: #e0e0e0;"
            }

            // Comments not supported in Move-based system yet
            val commentLabelOpt: Option[Label] = None

            val buttonRow = new HBox {
              spacing = 8
              children = Seq(
                new Button("Enter") {
                  styleClass += "notification-inline-action"
                  style = "-fx-text-fill: #4fc3f7;"
                  onAction = _ =>
                    // Navigate to this variation
                    println(s"DEBUG Enter button clicked: variationRoot.tob=${variationRoot.tob}, anchor=$anchor")
                    notificationCenter.clear(VariationPickerKey)
                    val result = variationNavigator.enterVariation(variationRoot, anchor)
                    println(s"DEBUG enterVariation result: $result")
                    result match
                      case Some(res) =>
                        println(s"DEBUG applying result: $res")
                        applyVariationEntryResult(res)
                        currentMoveIndex = -1
                        isViewingHistory = false
                        refreshMoveHistory()
                        updateStatusLabel()
                        analysisManager.triggerAnalysisIfActive()
                        clearVariationSelectionState()
                        boardView.refreshFromModel()
                      case None =>
                        println("DEBUG enterVariation returned None!")
                        notificationCenter.showWarning(
                          key = "variation-enter-error",
                          title = "Navigation Error",
                          message = "Could not enter variation."
                        )
                },
                new Button(if isSelected then "Highlighted" else "Highlight") {
                  disable = isSelected
                  styleClass += "notification-inline-action"
                  onAction = _ =>
                    variationSelectionState = Some(VariationSelectionState(anchor, anchorVariations, idx))
                    updateVariationStatusLabel()
                    updateMoveArrows()
                },
                new Button("Delete") {
                  styleClass += "notification-inline-action"
                  style = "-fx-text-fill: #ff6b6b;"
                  onAction = _ =>
                    deleteVariationByTob(variationRoot.tob)
                    notificationCenter.clear(VariationPickerKey)
                    // Reopen picker to show updated list
                    Platform.runLater {
                      showVariationPicker(moveIndex)
                    }
                }
              )
            }

            val entryBox = new VBox {
              spacing = 6
              style = "-fx-background-color: rgba(255,255,255,0.04); -fx-padding: 10; -fx-background-radius: 6;"
              children = commentLabelOpt match
                case Some(commentLabel) => Seq(headerLabel, commentLabel, buttonRow)
                case None => Seq(headerLabel, buttonRow)
            }

            content.children.add(entryBox)
          }
        }

      val actions = Seq(
        notificationCenter.NotificationAction(
          label = "Close",
          onTrigger = () => notificationCenter.clear(VariationPickerKey),
          closeOnTrigger = true,
          styleClass = "secondary"
        )
      )

      notificationCenter.showCustom(
        key = VariationPickerKey,
        level = notificationCenter.InfoLevel,
        title = anchorDescription,
        body = None,
        content = content,
        actions = actions,
        dismissible = true
      )
  }

  /** Delete a variation by its root TOB (Move-based) */
  private def deleteVariationByTob(variationRootTob: Long): Unit = {
    // Delete from Move tree
    controller.moveHistoryManager.removeVariation(variationRootTob)
    
    clearVariationSelectionState()
    refreshMoveHistory()
    updateMoveArrows()
  }

  // -------------------------------------------------------------------------
  // Branch selection overlay for navigation
  // -------------------------------------------------------------------------
  
  private def updateBranchSelectionOverlayMoves(anchorIndex: Int, variations: Vector[Move]): Unit = {
    // In review mode, automatically show overlay when variations exist
    if isInReviewMode && variations.nonEmpty then
      showBranchSelectionOverlayMoves(anchorIndex, variations)
    else
      notificationCenter.clear(BranchSelectionKey)
  }
  
  /** Show branch selection overlay when user presses → at a branch point (Move-based) */
  private def showBranchSelectionOverlayMoves(anchorIndex: Int, variations: Vector[Move]): Unit = {
    if variations.isEmpty then return

    val selectionIndex = variationSelectionState
      .filter(_.anchorIndex == anchorIndex)
      .map(_.selectedIndex)
      .getOrElse(-99)

    val content = branchOverlayBuilder.buildContent(
      anchorIndex = anchorIndex,
      variations = variations,
      selectionIndex = selectionIndex,
      viewingVariationRootTob = viewingVariationRootTob,
      currentVariationMove = currentVariationMove,
      onContinueClick = () => {
        // Continue in mainline or current variation
        notificationCenter.clear(BranchSelectionKey)
        variationSelectionState = Some(VariationSelectionState(anchorIndex, variations, -1))
        executeNavigationAction(NavigateRight)
      },
      onVariationClick = (variationRoot, idx) => {
        // Enter the clicked variation
        notificationCenter.clear(BranchSelectionKey)
        variationNavigator.enterVariation(variationRoot, anchorIndex) match
          case Some(result) =>
            applyVariationEntryResult(result)
            currentMoveIndex = -1
            isViewingHistory = false
            refreshMoveHistory()
            updateStatusLabel()
            analysisManager.triggerAnalysisIfActive()
            clearVariationSelectionState()
            boardView.refreshFromModel()
          case None =>
            notificationCenter.showWarning(
              key = "variation-enter-error",
              title = "Navigation Error",
              message = "Could not enter variation."
            )
      }
    )

    val title = branchOverlayBuilder.buildTitle(
      anchorIndex = anchorIndex,
      viewingVariationRootTob = viewingVariationRootTob,
      currentVariationMove = currentVariationMove
    )

    notificationCenter.showCustom(
      key = BranchSelectionKey,
      level = notificationCenter.InfoLevel,
      title = title,
      body = Some("Click an option or use ↑/↓ and → to navigate."),
      content = content,
      actions = Nil,
      dismissible = true
    )
  }

  // -------------------------------------------------------------------------
  /**
   * Follows a selected variation by replaying all its moves.
   * Uses Move-based variation tracking (variationRootTob).
   * Returns true if we progressed into a variation.
   */
  private def followSelectedVariation(): Boolean = {
    println(s"DEBUG followSelectedVariation: variationSelectionState=$variationSelectionState, currentAnchorIndex=$currentAnchorIndex")
    variationSelectionState match
      case Some(state) if state.anchorIndex == currentAnchorIndex && 
                          state.selectedIndex >= 0 && 
                          state.selectedIndex < state.variations.size =>
        println(s"DEBUG followSelectedVariation: entering variation at index ${state.selectedIndex}")
        val variationRoot = state.variations(state.selectedIndex)
        
        variationNavigator.enterVariation(variationRoot, currentAnchorIndex) match
          case Some(result) =>
            println(s"DEBUG followSelectedVariation: success!")
            applyVariationEntryResult(result)
            currentMoveIndex = -1
            isViewingHistory = false
            refreshMoveHistory()
            updateStatusLabel()
            analysisManager.triggerAnalysisIfActive()
            clearVariationSelectionState()
            boardView.refreshFromModel()
            true
          case None =>
            println(s"DEBUG followSelectedVariation: enterVariation returned None")
            false
          
      case Some(state) if state.anchorIndex == currentAnchorIndex && state.selectedIndex == -1 =>
        println(s"DEBUG followSelectedVariation: mainline selected")
        // Mainline selected - clear active variation
        controller.variationMode = false
        controller.variationRootTob = None
        viewingVariationRootTob = None
        currentVariationMove = None
        false
      case _ => false
  }
  
  /**
   * Follow a selected child variation when inside a parent variation.
   * Uses Move-based tracking.
   * Returns true if a child variation was entered.
   */
  private def followSelectedVariationInContext(currentPly: Int): Boolean = {
    variationSelectionState match
      case Some(state) if state.selectedIndex >= 0 && state.selectedIndex < state.variations.size =>
        val childVariationRoot = state.variations(state.selectedIndex)
        
        viewingVariationRootTob match
          case Some(parentRootTob) =>
            variationNavigator.enterChildVariation(childVariationRoot, parentRootTob, currentPly) match
              case Some(result) =>
                applyVariationEntryResult(result)
                refreshMoveHistory()
                updateStatusLabel()
                analysisManager.triggerAnalysisIfActive()
                clearVariationSelectionState()
                boardView.refreshFromModel()
                true
              case None => false
          case None => false
      case _ => false
  }
  
  /** Apply variation entry result to update state */
  private def applyVariationEntryResult(result: VariationEntryResult): Unit = {
    viewingVariationRootTob = result.viewingVariationRootTob
    currentVariationMove = result.currentVariationMove
    controller.variationMode = result.variationMode
    controller.variationRootTob = result.variationRootTob
    
    // Animate the entry move if animation coords are available
    result.animationCoords.foreach { case (fromRow, fromCol, toRow, toCol, pieceColor, pieceType) =>
      boardView.setLastMoveCoords(fromRow, fromCol, toRow, toCol)
      boardView.animatePieceMove(fromRow, fromCol, toRow, toCol, pieceColor, pieceType, () => {
        updateMoveArrows()
      })
    }
  }
  
  /**
   * Replay a move from SAN notation visually (without logging to history).
   * Delegated to MoveReplayer.
   */
  private def replayMoveFromSan(notation: String, isWhiteMove: Boolean): Unit = {
    moveReplayer.replayMoveFromSan(notation, isWhiteMove)
  }

  override def start(): Unit = {
    initialiseController()
    initialiseBoardView()
    initialiseMoveHistoryPanel()
    navigationHandler.resetToStart(rebuildBoard = false)
    
    // Register live engine arrows callback (runs on background thread, must use Platform.runLater)
    analysisManager.setLiveArrowsCallback(topMoves => Platform.runLater { () => updateLiveEngineArrows(topMoves) })

    stage = buildPrimaryStage()
    refreshEntireUI()
    
    // Sync coordinate labels after scene is fully laid out
    Platform.runLater { () => syncCoordinateLabelSizes() }
  }

  override def stopApp(): Unit = {
    analysisManager.stop()
    stockfishEngine.stop()
  }

  // -------------------------------------------------------------------------
  // Initialisation helpers
  // -------------------------------------------------------------------------

  private def initialiseController(): Unit = {
    controller.onPawnPromotion = Some(handlePawnPromotion)
    controller.updateAllPossibleMoves()
  }

  // -------------------------------------------------------------------------
  // Pawn promotion handling
  // -------------------------------------------------------------------------

  // Promotion state for deferred promotion (when using visual menu)
  private var pendingPromotion: Option[(PawnPiece, Square, Option[Piece])] = None
  private val promotionLoopKey = new Object()

  private def handlePawnPromotion(pawn: PawnPiece, @annotation.unused from: Square, to: Square, capturedPiece: Option[Piece]): PieceType = {
    // Get the row/col for the promotion square
    val (col, row) = to.getCoordinates
    
    // Store pending promotion info
    pendingPromotion = Some((pawn, to, capturedPiece))
    
    // Show the visual promotion menu on the board
    boardView.showPromotionMenu(row, col, pawn.color, { selectedPiece =>
      // This callback is called when user clicks a piece
      val pieceType = selectedPiece match {
        case "rook"   => PieceType.Rook
        case "bishop" => PieceType.Bishop
        case "knight" => PieceType.Knight
        case _        => PieceType.Queen
      }
      // Exit the nested event loop with the selected piece type
      javafx.application.Platform.exitNestedEventLoop(promotionLoopKey, pieceType)
    })
    
    // Enter a nested event loop - this blocks until exitNestedEventLoop is called
    // but still processes UI events (so the menu is interactive)
    val selectedType = javafx.application.Platform.enterNestedEventLoop(promotionLoopKey).asInstanceOf[PieceType]
    
    pendingPromotion = None
    boardView.refreshFromModel()
    applyPromotion(pawn, to, capturedPiece, selectedType)
  }

  private[chess] def applyPromotion(
      pawn: PawnPiece,
      to: Square,
      capturedPiece: Option[Piece],
      promotedType: PieceType
  ): PieceType = {
    pawn.capture()
    to.occupiedBy = None
    capturedPiece.foreach(_.capture())

    val board = pawn.board
    val promotedPiece = promotedType match
      case PieceType.Rook   => new RookPiece(pawn.color, to, board)
      case PieceType.Bishop => new BishopPiece(pawn.color, to, board)
      case PieceType.Knight => new KnightPiece(pawn.color, to, board)
      case _                => new QueenPiece(pawn.color, to, board)

    promotedPiece.hasMoved = true
    to.occupiedBy = Some(promotedPiece)

    promotedType
  }

  /** Show promotion menu during training mode - callback-based without nested loop */
  private def showPromotionMenuForTraining(
    color: String, 
    row: Int, 
    col: Int
  )(callback: PieceType => Unit): Unit = {
    boardView.showPromotionMenu(row, col, color, { selectedPiece =>
      val pieceType = selectedPiece match {
        case "rook"   => PieceType.Rook
        case "bishop" => PieceType.Bishop
        case "knight" => PieceType.Knight
        case _        => PieceType.Queen
      }
      callback(pieceType)
    })
  }

  private def initialiseBoardView(): Unit = {
    boardView.setModel(boardModel)
    boardView.onSquareClick = Some { (row, col) =>
      Platform.runLater(handleBoardClick(row, col))
    }
    boardView.onDragMove = Some { (fromRow, fromCol, toRow, toCol) =>
      Platform.runLater(attemptDragMove(fromRow, fromCol, toRow, toCol))
    }
    boardView.onGetLegalMoves = Some { (row, col) =>
      getLegalMovesForSquare(row, col)
    }
  }

  /** Get legal move target coordinates for piece at given square */
  private def getLegalMovesForSquare(row: Int, col: Int): Seq[(Int, Int)] = {
    val square = boardModel.squares(row)(col)
    square.occupiedBy match
      case Some(piece) if piece.color == controller.currentPlayer =>
        piece.possibleMoves().flatMap(toIndices)
      case _ => Seq.empty
  }

  private def initialiseMoveHistoryPanel(): Unit = {
    moveHistoryPanel.render(
      MoveHistoryPanelState(
        display = MoveHistoryDisplayState(Nil),
        statusMessage = ""
      )
    )
    moveHistoryPanel.setOnMoveSelected(handleMoveHistorySelection)
    moveHistoryPanel.setOnVariationSelected(showVariationPicker)
  }

  private def buildPrimaryStage(): PrimaryStage =
    val primaryScene = new Scene {
      stylesheets ++= Seq("/styles.css")
      root = buildRootPane()
    }
    primaryScene.addEventFilter(KeyEvent.KeyPressed, (event: KeyEvent) => handleNavigationKey(event))

    new PrimaryStage {
      title = "ScalaFX Chess"
      scene = primaryScene
      width = 1280
      height = 860
    }

  // Store references to coordinate labels for flip support
  private var topFileRow: HBox = scala.compiletime.uninitialized
  private var bottomFileRow: HBox = scala.compiletime.uninitialized
  private var leftRankCol: VBox = scala.compiletime.uninitialized
  private var rightRankCol: VBox = scala.compiletime.uninitialized
  
  // Fixed border thickness in pixels for coordinate labels
  private val borderThickness = 24.0

  /** Sync coordinate label sizes with board cell size */
  private def syncCoordinateLabelSizes(): Unit =
    // Use same default as BoardView when not yet laid out
    val s = if boardView.cellSize > 0 then boardView.cellSize else 80.0

    // Top/bottom thickness is constant; each label spans one file
    Seq(topFileRow, bottomFileRow).foreach { row =>
      if row != null then
        row.minHeight = borderThickness
        row.prefHeight = borderThickness
        row.maxHeight = borderThickness
        row.children.foreach {
          case l: JfxLabel =>
            l.setPrefWidth(s)
            l.setPrefHeight(borderThickness)
            l.setMinHeight(borderThickness)
            l.setMaxHeight(borderThickness)
          case region: JfxRegion =>
            region.setPrefWidth(s)
            region.setMinWidth(s)
            region.setMaxWidth(s)
            region.setPrefHeight(borderThickness)
            region.setMinHeight(borderThickness)
            region.setMaxHeight(borderThickness)
          case _ =>
        }
    }

    // Left/right thickness is constant; each rank cell spans one rank
    Seq(leftRankCol, rightRankCol).foreach { col =>
      if col != null then
        col.minWidth = borderThickness
        col.prefWidth = borderThickness
        col.maxWidth = borderThickness
        col.children.foreach {
          case region: JfxRegion =>
            region.setPrefHeight(s)
            region.setMinHeight(s)
            region.setMaxHeight(s)
            region.setPrefWidth(borderThickness)
            region.setMinWidth(borderThickness)
            region.setMaxWidth(borderThickness)
          case _ =>
        }
    }

  /**
   * Create a BorderPane that wraps the board with coordinate gutters.
   * Supports flipping the board orientation.
   */
  private def createLabeledBoard(): BorderPane = {
    def getFiles: Seq[String] = if isBoardFlipped then Seq("H","G","F","E","D","C","B","A") else Seq("A","B","C","D","E","F","G","H")
    def getRanks: Seq[String] = if isBoardFlipped then Seq("1","2","3","4","5","6","7","8") else Seq("8","7","6","5","4","3","2","1")

    def fileLabel(text: String): Label = new Label(text) {
      style = "-fx-text-fill: #b0b0b0; -fx-font-size: 13px; -fx-font-weight: bold;"
      alignment = Pos.Center
    }

    def rankCell(text: String, align: Pos, pad: Insets): StackPane = new StackPane {
      alignment = align
      padding = pad
      children = Seq(new Label(text) {
        style = "-fx-text-fill: #b0b0b0; -fx-font-size: 13px; -fx-font-weight: bold;"
        alignment = Pos.Center
      })
    }

    topFileRow = new HBox {
      alignment = Pos.Center
      spacing = 0
      children = getFiles.map(fileLabel)
    }

    bottomFileRow = new HBox {
      alignment = Pos.Center
      spacing = 0
      children = getFiles.map(fileLabel)
    }

    leftRankCol = new VBox {
      alignment = Pos.Center
      spacing = 0
      children = getRanks.map(r => rankCell(r, Pos.CenterLeft, Insets(0, 0, 0, 4)))
    }

    rightRankCol = new VBox {
      alignment = Pos.Center
      spacing = 0
      children = getRanks.map(r => rankCell(r, Pos.CenterRight, Insets(0, 4, 0, 0)))
    }

    // Recompute geometry when the board resizes and once initially
    boardView.height.onChange { (_, _, _) => syncCoordinateLabelSizes() }
    boardView.width.onChange  { (_, _, _) => syncCoordinateLabelSizes() }
    syncCoordinateLabelSizes()

    new BorderPane {
      style = "-fx-background-color: #252525; -fx-border-color: #555555; -fx-border-width: 1;"
      padding = Insets(6)
      center = boardView
      top = topFileRow
      bottom = bottomFileRow
      left = leftRankCol
      right = rightRankCol
    }
  }

  /** Update coordinate labels when board is flipped */
  private def updateCoordinateLabels(): Unit = {
    def getFiles: Seq[String] = if isBoardFlipped then Seq("H","G","F","E","D","C","B","A") else Seq("A","B","C","D","E","F","G","H")
    def getRanks: Seq[String] = if isBoardFlipped then Seq("1","2","3","4","5","6","7","8") else Seq("8","7","6","5","4","3","2","1")

    def fileLabel(text: String): Label = new Label(text) {
      style = "-fx-text-fill: #b0b0b0; -fx-font-size: 13px; -fx-font-weight: bold;"
      alignment = Pos.Center
    }

    def rankCell(text: String, align: Pos, pad: Insets): StackPane = new StackPane {
      alignment = align
      padding = pad
      children = Seq(new Label(text) {
        style = "-fx-text-fill: #b0b0b0; -fx-font-size: 13px; -fx-font-weight: bold;"
        alignment = Pos.Center
      })
    }

    topFileRow.children = getFiles.map(fileLabel)
    bottomFileRow.children = getFiles.map(fileLabel)
    leftRankCol.children = getRanks.map(r => rankCell(r, Pos.CenterLeft, Insets(0, 0, 0, 4)))
    rightRankCol.children = getRanks.map(r => rankCell(r, Pos.CenterRight, Insets(0, 4, 0, 0)))
    // Re-sync sizes after updating children
    syncCoordinateLabelSizes()
  }

  /** Toggle board flip state */
  private def flipBoard(): Unit = {
    isBoardFlipped = !isBoardFlipped
    boardView.setFlipped(isBoardFlipped)
    updateCoordinateLabels()
    if flipBoardMenuItem != null then flipBoardMenuItem.selected = isBoardFlipped
  }

  private def buildRootPane(): BorderPane = {
    val labeledBoard = createLabeledBoard()
    
    // Bind evalBar height to boardView height so it doesn't cause layout issues
    analysisManager.evalBar.prefHeight <== boardView.heightProperty
    analysisManager.evalBar.minHeight <== boardView.heightProperty
    analysisManager.evalBar.maxHeight <== boardView.heightProperty
    
    // Board area with eval bar
    val boardArea = new HBox {
      alignment = Pos.Center
      spacing = 8
      padding = Insets(16)
      children = Seq(
        analysisManager.evalBar,
        labeledBoard
      )
    }
    
    // Stack board area with progress overlay
    val boardContainer = new StackPane {
      children = Seq(boardArea, analysisProgressOverlay)
    }

    // Section header builder
    def sectionHeader(title: String): Label = new Label(title) {
      style = "-fx-text-fill: #ffffff; -fx-font-size: 14px; -fx-font-weight: bold; -fx-padding: 8 0 4 0;"
    }
    
    // Separator line
    def separator(): HBox = new HBox {
      style = "-fx-background-color: #444444;"
      minHeight = 1
      maxHeight = 1
      prefHeight = 1
    }

    // Side panel content (scrollable)
    val sidePanelContent = new VBox {
      spacing = 12
      padding = Insets(16)
      style = "-fx-background-color: #2a2f35;"
      children = Seq(
        // Error Training Panel (shown when training mode active)
        errorTrainingPanel.panel,
        // Game Review Section (shown after analysis) - TOP PRIORITY
        gameReviewPanel.panel,
        separator(),
        // Engine Analysis Section
        sectionHeader("Engine Analysis"),
        analysisManager.engineAnalysisPanel,
        separator(),
        // Notifications Section (moved above Move History)
        sectionHeader("Notifications"),
        notificationCenter,
        separator(),
        // Move History Section (at bottom)
        sectionHeader("Move History"),
        moveHistoryPanel
      )
    }
    
    // Scrollable side panel
    val sidePanel = new ScrollPane {
      content = sidePanelContent
      fitToWidth = true
      hbarPolicy = ScrollPane.ScrollBarPolicy.Never
      vbarPolicy = ScrollPane.ScrollBarPolicy.AsNeeded
      prefWidth = 380
      minWidth = 320
      style = "-fx-background: #2a2f35; -fx-background-color: #2a2f35; -fx-border-color: #444444; -fx-border-width: 0 0 0 1;"
    }

    // New Game button for bottom bar
    val newGameButton = new Button("New Game") {
      style = "-fx-background-color: #3a5f8f; -fx-text-fill: white; -fx-background-radius: 4; -fx-padding: 6 12 6 12;"
      onAction = _ => startNewGame()
    }

    new BorderPane {
      top = buildMenuBar()
      center = boardContainer
      right = sidePanel
      bottom = new HBox {
        alignment = Pos.CenterLeft
        spacing = 16
        padding = Insets(12, 16, 12, 16)
        children = Seq(newGameButton, statusLabel)
      }
      style = "-fx-background-color: #20252b;"
    }
  }

  private def buildMenuBar(): MenuBar = {
    val newGameItem = new MenuItem("New Game") {
      onAction = _ => startNewGame()
    }

    val loadGameItem = new MenuItem("Load PGN…") {
      onAction = _ => fileOperations.loadGameFromPGN(() => handlePGNLoadSuccess())
    }

    val saveGameItem = new MenuItem("Save Game as PGN…") {
      onAction = _ => fileOperations.saveGameAsPGN()
    }

    val clearNotificationsItem = new MenuItem("Clear Notifications") {
      onAction = _ => notificationCenter.clearAll()
    }

    val gameMenu = new Menu("Game") {
      items = Seq(newGameItem, loadGameItem, saveGameItem, new SeparatorMenuItem, clearNotificationsItem)
    }

    // View menu - board flip
    flipBoardMenuItem = new CheckMenuItem("Flip Board") {
      selected = isBoardFlipped
      onAction = _ => flipBoard()
    }

    val viewMenu = new Menu("View") {
      items = Seq(flipBoardMenuItem)
    }

    // Variations menu - only show arrows toggle
    showMoveArrowsMenuItem = new CheckMenuItem("Show Move Arrows") {
      selected = showMoveArrows
      onAction = _ => setMoveArrowsEnabled(selected())
    }

    val variationsMenu = new Menu("Variations") {
      items = Seq(showMoveArrowsMenuItem)
    }

    // Analysis menu
    val toggleAnalysisItem: CheckMenuItem = analysisManager.toggleMenuItem

    val loadSnapshotItem = new MenuItem("Load Analysis Snapshot…") {
      onAction = _ => fileOperations.loadAnalysisSnapshot()
    }

    val saveSnapshotItem = new MenuItem("Save Analysis Snapshot…") {
      onAction = _ => fileOperations.saveAnalysisSnapshot()
    }
    
    val analyzeGameItem = new MenuItem("Analyze Game with Stockfish…") {
      onAction = _ => analyzeCurrentGame()
    }

    val analysisMenu = new Menu("Analysis") {
      items = Seq(toggleAnalysisItem, new SeparatorMenuItem, analyzeGameItem, new SeparatorMenuItem, loadSnapshotItem, saveSnapshotItem)
    }

    // PGN Tools menu
    val splitMultiGameItem = new MenuItem("Split Multi-Game PGN…") {
      onAction = _ => splitMultiGamePGN()
    }

    val pgnToolsMenu = new Menu("PGN Tools") {
      items = Seq(splitMultiGameItem)
    }

    // Training menu
    val errorTrainingItem = new MenuItem("Virheiden harjoittelu…") {
      onAction = _ => startErrorTraining()
    }
    
    val trainingMenu = new Menu("Training") {
      items = Seq(errorTrainingItem)
    }

    new MenuBar {
      useSystemMenuBar = false
      menus = Seq(gameMenu, viewMenu, variationsMenu, analysisMenu, pgnToolsMenu, trainingMenu)
    }
  }

  // -------------------------------------------------------------------------
  // UI event handling
  // -------------------------------------------------------------------------

  /** Handle board clicks during training mode - separate selection/move logic */
  private def handleTrainingModeClick(row: Int, col: Int): Unit = {
    val targetSquare = boardModel.squares(row)(col)
    
    // Convert row/col to chess notation (e.g., "e2")
    def toSquareName(r: Int, c: Int): String = {
      val file = ('a' + c).toChar
      val rank = 8 - r
      s"$file$rank"
    }
    
    trainingSelectedSquare match
      case None =>
        // Try to select a piece
        targetSquare.occupiedBy match
          case Some(piece) if piece.color == controller.currentPlayer && !piece.isCaptured =>
            trainingSelectedSquare = Some(row -> col)
            boardView.setSelectedSquare(row, col)
            val legalTargets = piece.possibleMoves().flatMap(toIndices)
            boardView.setLegalMoveTargets(legalTargets)
          case _ =>
            notificationCenter.showWarning(
              key = "training-select",
              title = "Invalid selection",
              message = "Select a piece that belongs to the side to move."
            )
      case Some((selRow, selCol)) if selRow == row && selCol == col =>
        // Clicked same square - deselect
        trainingSelectedSquare = None
        boardView.clearSelectedSquare()
        boardView.clearLegalMoveTargets()
      case Some((selRow, selCol)) =>
        val selectedSquare = boardModel.squares(selRow)(selCol)
        targetSquare.occupiedBy match
          case Some(piece) if piece.color == controller.currentPlayer && selectedSquare != targetSquare =>
            // Clicked another own piece - reselect
            trainingSelectedSquare = Some(row -> col)
            boardView.setSelectedSquare(row, col)
            val legalTargets = piece.possibleMoves().flatMap(toIndices)
            boardView.setLegalMoveTargets(legalTargets)
          case _ =>
            // Attempt move
            val from = toSquareName(selRow, selCol)
            val to = toSquareName(row, col)
            
            // Check if this is a valid move
            selectedSquare.occupiedBy match
              case Some(piece) =>
                val legalMoves = piece.possibleMoves()
                val isLegal = legalMoves.exists { sq =>
                  val indices = toIndices(sq)
                  indices.contains((row, col))
                }
                
                if isLegal then
                  // Check for pawn promotion
                  val isPawnPromotion = piece.pieceType == PieceType.Pawn && (row == 0 || row == 7)
                  
                  if isPawnPromotion then
                    // Show promotion menu, then handle move
                    showPromotionMenuForTraining(piece.color, row, col) { promotedType =>
                      val promotionChar = promotedType.toString.toLowerCase.head match {
                        case 'q' => 'q'
                        case 'r' => 'r'
                        case 'b' => 'b'
                        case 'n' => 'n'
                        case _ => 'q'
                      }
                      // First execute the move on the board
                      controller.handleAction(piece, boardModel.squares(row)(col))
                      boardView.refreshFromModel()
                      
                      trainingSelectedSquare = None
                      boardView.clearSelectedSquare()
                      boardView.clearLegalMoveTargets()
                      // Then let ErrorTrainingPanel evaluate it
                      errorTrainingPanel.handlePlayerMove(from, to, Some(promotionChar.toString))
                    }
                  else
                    // First execute the move on the board
                    controller.handleAction(piece, boardModel.squares(row)(col))
                    boardView.refreshFromModel()
                    
                    trainingSelectedSquare = None
                    boardView.clearSelectedSquare()
                    boardView.clearLegalMoveTargets()
                    // Then let ErrorTrainingPanel evaluate it
                    errorTrainingPanel.handlePlayerMove(from, to, None)
                else
                  notificationCenter.showWarning(
                    key = "training-illegal",
                    title = "Illegal move",
                    message = "That move is not legal."
                  )
              case None =>
                ()
  }

  private def handleBoardClick(row: Int, col: Int): Unit = {
    // In training mode, route clicks to training handler
    if isInTrainingMode then
      handleTrainingModeClick(row, col)
      return
      
    val targetSquare = boardModel.squares(row)(col)

    selectedSquareCoord match
      case None =>
        attemptSelection(row, col, targetSquare)
      case Some((selRow, selCol)) if selRow == row && selCol == col =>
        clearSelection()
      case Some((selRow, selCol)) =>
        val selectedSquare = boardModel.squares(selRow)(selCol)
        targetSquare.occupiedBy match
          case Some(piece) if piece.color == controller.currentPlayer && selectedSquare != targetSquare =>
            attemptSelection(row, col, targetSquare)
          case _ =>
            attemptMove(selRow, selCol, row, col)
  }

  private def attemptSelection(row: Int, col: Int, square: Square): Unit = {
    square.occupiedBy match
      case Some(piece) if piece.color == controller.currentPlayer && !piece.isCaptured =>
        controller.selectPiece(square)
        selectedSquareCoord = Some(row -> col)
        boardView.setSelectedSquare(row, col)
        val legalTargets = piece.possibleMoves().flatMap(toIndices)
        boardView.setLegalMoveTargets(legalTargets)
        notificationCenter.clear("illegal-move")
      case _ =>
        notificationCenter.showWarning(
          key = "illegal-move",
          title = "Invalid selection",
          message = "Select a piece that belongs to the side to move."
        )
  }

  private def attemptMove(fromRow: Int, fromCol: Int, toRow: Int, toCol: Int): Unit = {
    val sourceSquare = boardModel.squares(fromRow)(fromCol)
    val destinationSquare = boardModel.squares(toRow)(toCol)

    sourceSquare.occupiedBy match
      case Some(piece) =>
        // Store piece info for animation before the move
        val pieceColor = piece.color
        val pieceType = piece.pieceType.toString.toLowerCase
        
        // Use mainline moves count (not all moves including variations)
        val mainlineMoves = controller.moveHistoryManager.getMainlineMoves
        val historySize = mainlineMoves.size
        
        // Capture the move notation before applying (for variation creation)
        // If viewing history at start position (currentMoveIndex == -1), anchorIndex should be -1
        // Otherwise, use currentMoveIndex if viewing history, or the last move index
        val anchorIndex = if isViewingHistory then currentMoveIndex else historySize - 1
        
        def performMove(): Boolean = {
          val moved = controller.handleAction(piece, destinationSquare)
          if moved then
            // Set last move coords without refresh, then animate
            boardView.setLastMoveCoords(fromRow, fromCol, toRow, toCol)
            // Animate the piece move - this will refresh before and after animation
            boardView.animatePieceMove(fromRow, fromCol, toRow, toCol, pieceColor, pieceType, () => {
              updateMoveArrows()
            })
          moved
        }

        // Check if we're extending an active variation (Move-based)
        // We only append if we're at the END of the variation
        val atEndOfVariation = viewingVariationRootTob.exists { rootTob =>
          val variationLine = controller.moveHistoryManager.collectVariationLine(rootTob)
          // Use list index, not halfmoveDistanceFromStart
          val currentIdx = currentVariationMove.map(m => variationLine.indexWhere(_.tob == m.tob)).getOrElse(-1)
          currentIdx >= variationLine.length - 1
        }
        
        // Are we in the middle of a variation? (viewing variation but not at end)
        val inMiddleOfVariation = viewingVariationRootTob.isDefined && !atEndOfVariation
        
        // atLiveEdge means we're at the end of mainline AND not inside a variation
        // Special case: at start position (currentMoveIndex == -1) with existing moves is NOT at live edge
        // But only if we're VIEWING history - if isViewingHistory is false, we're at live edge
        // IMPORTANT: In game review mode, we NEVER want to add to mainline - always create variations
        val atStartWithHistory = isViewingHistory && currentMoveIndex == -1 && historySize > 0
        val atLiveEdge = !isInReviewMode && !inMiddleOfVariation && !atStartWithHistory && 
                         (!isViewingHistory || (historySize > 0 && currentMoveIndex >= historySize - 1))
        
        if viewingVariationRootTob.isDefined && atEndOfVariation then
          // We're at the end of a variation - append to it
          controller.variationParentTob = currentVariationMove.map(_.tob)
          controller.variationRootTob = viewingVariationRootTob
          controller.variationMode = true
          
          if performMove() then
            // Get the Move we just added - it's a child of currentVariationMove
            val newMoveOpt = currentVariationMove.flatMap { parentMove =>
              controller.moveHistoryManager.getChildrenOf(parentMove.tob)
                .filter(_.isVariation)
                .lastOption
            }
            // Update Move-based state
            currentVariationMove = newMoveOpt
            controller.variationParentTob = None
            currentMoveIndex = -1
            isViewingHistory = false
            refreshMoveHistory()
            updateMoveArrows()
            updateStatusLabel()
            analysisManager.triggerAnalysisIfActive()
            showWinnerIfDecided()
          else
            controller.variationParentTob = None
            showIllegalMoveAttempt()
        
        else if atLiveEdge then
          // Normal move at the end of mainline
          controller.variationMode = false
          controller.variationRootTob = None
          val currentMainlineMove = navigationHandler.currentMove.filter(!_.isVariation)
          controller.variationParentTob = currentMainlineMove.map(_.tob)
          if performMove() then
            controller.variationParentTob = None
            currentMoveIndex = -1
            isViewingHistory = false
            navigationHandler.syncToLatest()
            refreshMoveHistory()
            updateStatusLabel()
            analysisManager.triggerAnalysisIfActive()
            showWinnerIfDecided()
          else
            controller.variationParentTob = None
            showIllegalMoveAttempt()
        
        else
          // Making a move while viewing history or in the MIDDLE of a variation
          // This creates a variation (nested if inside another variation)
          
          // Calculate starting color for the NEW move
          val startingColor: String = viewingVariationRootTob match
            case Some(parentRootTob) =>
              // We're inside a variation - calculate based on position within parent variation
              val parentLine = controller.moveHistoryManager.collectVariationLine(parentRootTob)
              val currentIdxInParent = currentVariationMove match
                case Some(curMove) => parentLine.indexWhere(_.tob == curMove.tob)
                case None => -1
              val parentStartColor = controller.moveHistoryManager.getVariationStartingColor(parentRootTob)
              val parentStartsWhite = parentStartColor == "white"
              // After currentIdxInParent moves, the next move color
              val movesPlayedInParent = currentIdxInParent + 1
              val nextMoveIsWhite = if parentStartsWhite then (movesPlayedInParent % 2 == 0) else (movesPlayedInParent % 2 == 1)
              if nextMoveIsWhite then "white" else "black"
            case None =>
              startingColorFromAnchor(anchorIndex)
          
          // Enable variation mode
          controller.variationMode = true
          controller.variationRootTob = None
          
          // Set the correct parent for the variation Move
          // For move 1 variations (anchorIndex = -1), use -1L as parent
          val parentTob: Option[Long] = viewingVariationRootTob match
            case Some(_) =>
              // Nested variation: parent is the current move in the parent variation
              currentVariationMove.map(_.tob)
            case None =>
              // First-level variation: parent is the mainline move at anchorIndex
              // For starting position (anchorIndex < 0), use -1L as sentinel
              if anchorIndex < 0 then Some(-1L)
              else controller.moveHistoryManager.getMainlineMoveAt(anchorIndex).map(_.tob)
          controller.variationParentTob = parentTob
          
          val moved = controller.handleAction(piece, destinationSquare)
          
          if moved then
            // Get the Move object using variationRootTob
            val newMoveOpt = controller.variationRootTob.flatMap(controller.moveHistoryManager.getMoveByTob)
            val newMoveNotation = newMoveOpt.flatMap(_.notation)
            val newVariationRootTob = controller.variationRootTob
            
            newMoveNotation.foreach { notation =>
              // Check for duplicate variation with same first move
              // Get existing variations at the correct anchor point
              val existingVariations: Vector[Move] = viewingVariationRootTob match
                case Some(parentRootTob) =>
                  // Nested variation: get variations at current position in parent
                  val parentLine = controller.moveHistoryManager.collectVariationLine(parentRootTob)
                  val currentIdxInParent = currentVariationMove match
                    case Some(curMove) => parentLine.indexWhere(_.tob == curMove.tob)
                    case None => -1
                  if currentIdxInParent >= 0 then
                    controller.moveHistoryManager.getNestedVariationRootsAt(parentRootTob, currentIdxInParent).toVector
                  else
                    Vector.empty
                case None =>
                  // First-level variation: get variations at mainline anchor
                  variationsForAnchorMoves(anchorIndex)
              
              val existingDuplicate = existingVariations.find { rootMove =>
                rootMove.tob != newVariationRootTob.getOrElse(-1L) &&  // Not the one we just created
                controller.moveHistoryManager.getVariationMoveNotations(rootMove.tob).headOption.contains(notation)
              }
              
              // Determine which variation to use
              val (actualRootTob, actualRootMove) = existingDuplicate match
                case Some(existing) =>
                  // Remove the duplicate Move we just created, use existing
                  newVariationRootTob.foreach { rootTob =>
                    controller.moveHistoryManager.removeVariation(rootTob)
                  }
                  (existing.tob, Some(existing))
                case None =>
                  // Use the new variation we created
                  (newVariationRootTob.getOrElse(-1L), newMoveOpt)
              
              // Enter the variation by replaying to the branch point and showing the move
              // Restore board to branch point
              boardModel.resetBoard()
              controller.currentPlayer = "white"
              controller.board.foreach(_.deselectAll())
              
              val mainlineMoves = controller.moveHistoryManager.getMoveHistory.filter(!_.isVariation)
              val effectiveAnchor = viewingVariationRootTob match
                case Some(parentRootTob) =>
                  controller.moveHistoryManager.getMainlineAnchorIndexForVariation(parentRootTob)
                case None =>
                  anchorIndex
              
              // Replay mainline to anchor
              mainlineMoves.take(effectiveAnchor + 1).foreach { move =>
                controller.replayMove(move)
              }
              var colorToMove = if (effectiveAnchor + 1) % 2 == 0 then "white" else "black"
              
              // If we were in a parent variation, replay ALL ancestor variations up to branch point
              viewingVariationRootTob.foreach { parentRootTob =>
                // Helper to collect ancestor chain from root-level to current
                def collectAncestorChain(varRootTob: Long): List[(Long, Int)] =
                  controller.moveHistoryManager.getParentVariationRootTob(varRootTob) match
                    case Some(ancestorRootTob) =>
                      val branchIdx = controller.moveHistoryManager.getParentVariationBranchIndex(varRootTob)
                      collectAncestorChain(ancestorRootTob) :+ (varRootTob, branchIdx)
                    case None =>
                      List((varRootTob, -1))
                
                val ancestorChain = collectAncestorChain(parentRootTob)
                
                // Replay all ancestor variations up to their branch points (except the current one)
                ancestorChain.dropRight(1).zipWithIndex.foreach { case ((ancestorRootTob, _), chainIdx) =>
                  val ancestorLine = controller.moveHistoryManager.collectVariationLine(ancestorRootTob)
                  // Find where the next variation branches from this ancestor
                  val nextBranchIdx = if chainIdx + 1 < ancestorChain.size then
                    ancestorChain(chainIdx + 1)._2
                  else
                    ancestorLine.size - 1
                  
                  val ancestorStartColor = controller.moveHistoryManager.getVariationStartingColor(ancestorRootTob)
                  colorToMove = ancestorStartColor
                  
                  ancestorLine.take(nextBranchIdx + 1).foreach { move =>
                    move.notation.foreach { san =>
                      val isWhite = colorToMove == "white"
                      replayMoveFromSan(san, isWhite)
                      colorToMove = if isWhite then "black" else "white"
                    }
                  }
                }
                
                // Now replay the immediate parent variation up to the branch point
                val parentLine = controller.moveHistoryManager.collectVariationLine(parentRootTob)
                val currentIdxInParent = currentVariationMove match
                  case Some(curMove) => parentLine.indexWhere(_.tob == curMove.tob)
                  case None => -1
                  
                if currentIdxInParent >= 0 then
                  val parentStartColor = controller.moveHistoryManager.getVariationStartingColor(parentRootTob)
                  colorToMove = parentStartColor
                  parentLine.take(currentIdxInParent + 1).foreach { move =>
                    move.notation.foreach { san =>
                      val isWhite = colorToMove == "white"
                      replayMoveFromSan(san, isWhite)
                      colorToMove = if isWhite then "black" else "white"
                    }
                  }
              }
              
              // Play the new variation move
              val isWhiteMove = startingColor == "white"
              replayMoveFromSan(notation, isWhiteMove)
              
              // Set up variation tracking with the correct tob
              if actualRootTob != -1L then
                viewingVariationRootTob = Some(actualRootTob)
                currentVariationMove = actualRootMove
                controller.variationMode = true
                controller.variationRootTob = Some(actualRootTob)
            }
            
            controller.variationParentTob = None
            currentMoveIndex = -1
            isViewingHistory = false
            refreshMoveHistory()
            updateStatusLabel()
            updateMoveArrows()
            analysisManager.triggerAnalysisIfActive()
            boardView.refreshFromModel()
            showWinnerIfDecided()
          else
            controller.variationParentTob = None
            controller.variationMode = false
            showIllegalMoveAttempt()
        
        clearSelection()
      case None =>
        clearSelection()
  }

  /** Handle drag-and-drop move attempt - validates that source has a moveable piece */
  private def attemptDragMove(fromRow: Int, fromCol: Int, toRow: Int, toCol: Int): Unit = {
    val sourceSquare = boardModel.squares(fromRow)(fromCol)
    
    sourceSquare.occupiedBy match
      case Some(piece) if piece.color == controller.currentPlayer =>
        // Valid piece to move - use existing move logic
        attemptMove(fromRow, fromCol, toRow, toCol)
      case Some(_) =>
        notificationCenter.showWarning(
          key = "illegal-move",
          title = "Invalid move",
          message = "You can only move your own pieces."
        )
      case None =>
        // No piece on source square (shouldn't happen with D&D but handle gracefully)
        ()
  }

  private def showIllegalMoveAttempt(): Unit = {
    notificationCenter.showWarning(
      key = "illegal-move",
      title = "Illegal move",
      message = "That move is not legal from the current position."
    )
    clearSelection()
  }

  // -------------------------------------------------------------------------
  // Navigation helpers
  // -------------------------------------------------------------------------

  private def handleNavigationKey(event: KeyEvent): Unit = {
    val action = event.code match
      case KeyCode.Left =>
        keyboardNavHandler.handleLeftKey(viewingVariationRootTob, currentVariationMove)
      case KeyCode.Right =>
        val getSelectedIdx: Int => Int = anchorIdx =>
          variationSelectionState
            .filter(_.anchorIndex == anchorIdx)
            .map(_.selectedIndex)
            .getOrElse(-99)
        keyboardNavHandler.handleRightKey(
          viewingVariationRootTob, currentVariationMove, currentAnchorIndex, getSelectedIdx
        )
      case KeyCode.Up => CycleSelectionUp
      case KeyCode.Down => CycleSelectionDown
      case _ => NoAction

    executeNavigationAction(action)
    if action != NoAction then event.consume()
  }

  private def executeNavigationAction(action: KeyboardNavigationAction): Unit = action match
    case NavigateLeft =>
      navigationHandler.navigateToParent().foreach(applyNavigationResult)
      
    case NavigateRight =>
      // Animate forward navigation
      navigationHandler.navigateToChild().foreach(r => applyNavigationResultWithAnimation(r, animate = true))
      
    case NavigateLeftInVariation =>
      viewingVariationRootTob.foreach { parentRootTob =>
        val currentIdx = keyboardNavHandler.getCurrentVariationIndex(parentRootTob, currentVariationMove)
        if currentIdx > 0 then
          val result = moveReplayer.restoreVariationPosition(parentRootTob, currentIdx - 1)
          currentVariationMove = result.move
          refreshAfterVariationNavigation()
      }
      
    case NavigateRightInVariation(advanceToIdx) =>
      viewingVariationRootTob.foreach { parentRootTob =>
        val result = moveReplayer.restoreVariationPosition(parentRootTob, advanceToIdx)
        currentVariationMove = result.move
        
        // Animate the last move
        result.lastMoveCoords.foreach { case (fromRow, fromCol, toRow, toCol, pieceColor, pieceType) =>
          boardView.setLastMoveCoords(fromRow, fromCol, toRow, toCol)
          boardView.animatePieceMove(fromRow, fromCol, toRow, toCol, pieceColor, pieceType, () => {
            updateMoveArrows()
          })
        }
        
        refreshAfterVariationNavigation()
      }
      
    case ExitCurrentVariation =>
      exitVariation()
      
    case ShowBranchOverlay(anchorIdx, variations) =>
      variationSelectionState = Some(VariationSelectionState(anchorIdx, variations, -1))
      showBranchSelectionOverlayMoves(anchorIdx, variations)
      
    case EnterSelectedVariation =>
      // Clear notification but keep state for followSelectedVariation
      notificationCenter.clear(BranchSelectionKey)
      val _ = followSelectedVariation()
      
    case EnterSelectedChildVariation(currentIdx) =>
      val _ = followSelectedVariationInContext(currentIdx)
      
    case CycleSelectionUp =>
      cycleVariationSelection(-1)
      
    case CycleSelectionDown =>
      cycleVariationSelection(1)
      
    case NoAction => ()

  private def refreshAfterVariationNavigation(): Unit = {
    controller.updateAllPossibleMoves()
    boardView.refreshFromModel()
    refreshMoveHistory()
    updateMoveArrows()
    updateStatusLabel()
    analysisManager.triggerAnalysisIfActive()
  }

  /** Exit variation mode - go to parent variation or mainline at the branch point (Move-based) */
  private def exitVariation(): Unit = {
    viewingVariationRootTob match
      case Some(rootTob) =>
        variationNavigator.determineExit(rootTob) match
          case ExitToParentVariation(parentRootTob, parentIdx) =>
            viewingVariationRootTob = Some(parentRootTob)
            currentVariationMove = variationNavigator.restoreToParentVariation(parentRootTob, parentIdx)
            boardView.refreshFromModel()
            refreshMoveHistory()
            updateMoveArrows()
            updateStatusLabel()
            analysisManager.triggerAnalysisIfActive()
            
          case ExitToMainline(anchorIndex) =>
            clearVariationState()
            navigationHandler.navigateToHalfMoveIndex(anchorIndex) match
              case Some(result) => applyNavigationResult(result)
              case None => ()
            refreshMoveHistory()
            
          case ExitToStart =>
            clearVariationState()
            variationNavigator.resetToStart()
            currentMoveIndex = -1
            isViewingHistory = true
            refreshMoveHistory()
            boardView.refreshFromModel()
            updateStatusLabel()
            
          case NoExit => ()
      case None => ()
  }
  
  /** Clear all variation-related state */
  private def clearVariationState(): Unit = {
    viewingVariationRootTob = None
    currentVariationMove = None
    controller.variationMode = false
    controller.variationRootTob = None
  }
  
  /** 
   * Play "Best Response" - shows 4 half-moves of engine's best line as preview.
   * Uses the NEXT move's analysis PV, because that analysis was done on the position
   * AFTER the current move was played - which is what we need for "best response".
   * Does NOT create a variation. Arrow key navigation returns to actual game position.
   */
  private def playBestResponse(): Unit = {
    if (!isInReviewMode || gameAnalysisResults.isEmpty) return
    
    // We need the analysis from the NEXT position (after current move was played)
    // Because analysis[i] is done BEFORE move[i] was played
    // So analysis[i+1] gives us the engine's view of the position AFTER move[i]
    val nextMoveIndex = currentMoveIndex + 1
    
    val analysisOpt = if (nextMoveIndex >= 0 && nextMoveIndex < gameAnalysisResults.length) {
      Some(gameAnalysisResults(nextMoveIndex))
    } else None
    
    analysisOpt match {
      case Some(analysis) if analysis.principalVariation.nonEmpty =>
        // Save original position
        bestResponsePreviewIndex = currentMoveIndex
        bestResponsePreviewActive = true
        
        // The PV from analysis[i+1] starts with the best move for the opponent
        // (the side that played move[i+1] in the actual game)
        // This is exactly what we want - the opponent's best response!
        val pvMoves = analysis.principalVariation.take(4)  // Take up to 4 half-moves
        
        if (pvMoves.nonEmpty) {
          // analysis.isWhiteMove tells us who played the analyzed move (move[i+1])
          // So the PV starts with that same color's best move
          val startingWhiteToMove = analysis.isWhiteMove
          val boardInstance = controller.board.get
          
          // Show notification about preview mode
          notificationCenter.showInfo(
            key = "best-response-preview",
            title = "Best Response",
            message = s"Playing ${pvMoves.length} moves..."
          )
          
          // Play moves with 750ms delay between each
          import java.util.{Timer, TimerTask}
          val timer = new Timer()
          var moveIndex = 0
          var isWhiteToMove = startingWhiteToMove
          
          def playNextMove(): Unit = {
            if (moveIndex < pvMoves.length && bestResponsePreviewActive) {
              val uciMove = pvMoves(moveIndex)
              
              Platform.runLater {
                // Convert UCI (e.g., "e2e4") to squares
                if (uciMove.length >= 4) {
                  val fromName = uciMove.substring(0, 2)
                  val toName = uciMove.substring(2, 4)
                  val promotionPiece = if (uciMove.length > 4) Some(uciMove.charAt(4)) else None
                  
                  boardInstance.getSquareByName(fromName).foreach { fromSquare =>
                    boardInstance.getSquareByName(toName).foreach { toSquare =>
                      fromSquare.occupiedBy.foreach { piece =>
                        // Move piece visually
                        val captured = toSquare.occupiedBy
                        captured.foreach(_.capture())
                        piece.moveTo(toSquare)
                        piece.hasMoved = true
                        
                        // Handle pawn promotion
                        promotionPiece.foreach { promChar =>
                          val newType = promChar.toLower match {
                            case 'q' => "Queen"
                            case 'r' => "Rook"
                            case 'b' => "Bishop"
                            case 'n' => "Knight"
                            case _ => "Queen"
                          }
                          piece.promoteTo(newType)
                        }
                        
                        // Handle castling
                        if (piece.pieceType == chess.types.PieceType.King) {
                          val colDiff = toSquare.getCoordinates._1 - fromSquare.getCoordinates._1
                          if (Math.abs(colDiff) == 2) {
                            // Castling - move the rook too
                            val (rookFromCol, rookToCol) = if (colDiff > 0) (7, 5) else (0, 3)
                            val row = fromSquare.getCoordinates._2
                            val rookFromSquare = boardInstance.squares(row)(rookFromCol)
                            val rookToSquare = boardInstance.squares(row)(rookToCol)
                            rookFromSquare.occupiedBy.foreach { rook =>
                              rook.moveTo(rookToSquare)
                              rook.hasMoved = true
                            }
                          }
                        }
                        
                        // Handle en passant
                        if (piece.pieceType == chess.types.PieceType.Pawn) {
                          val fromCol = fromSquare.getCoordinates._1
                          val toCol = toSquare.getCoordinates._1
                          val toRow = toSquare.getCoordinates._2
                          if (fromCol != toCol && captured.isEmpty) {
                            // Diagonal move with no capture = en passant
                            val capturedPawnRow = if (isWhiteToMove) toRow + 1 else toRow - 1
                            val capturedPawnSquare = boardInstance.squares(capturedPawnRow)(toCol)
                            capturedPawnSquare.occupiedBy.foreach(_.capture())
                          }
                        }
                      }
                    }
                  }
                  
                  // Refresh board view and highlight the move
                  boardView.refreshFromModel()
                  
                  // Highlight the moved piece's destination
                  indicesFromSquareName(fromName).foreach { case (fromRow, fromCol) =>
                    indicesFromSquareName(toName).foreach { case (toRow, toCol) =>
                      boardView.setLastMove(fromRow, fromCol, toRow, toCol)
                    }
                  }
                }
                
                isWhiteToMove = !isWhiteToMove
                moveIndex += 1
                
                // Update notification
                notificationCenter.showInfo(
                  key = "best-response-preview",
                  title = "Best Response",
                  message = s"Move ${moveIndex}/${pvMoves.length}. Press arrow keys to exit."
                )
              }
              
              // Schedule next move
              if (moveIndex < pvMoves.length) {
                timer.schedule(new TimerTask {
                  override def run(): Unit = playNextMove()
                }, 750)
              }
            } else {
              timer.cancel()
            }
          }
          
          // Start playing moves
          playNextMove()
        }
        
      case _ =>
        notificationCenter.showInfo(
          key = "no-best-response",
          title = "Best Response",
          message = "No engine analysis available for this position"
        )
    }
  }

  private def applyNavigationResult(result: ScalaFXNavigationHandler.NavigationResult): Unit = {
    applyNavigationResultWithAnimation(result, animate = false)
  }
  
  /** Apply navigation result with optional animation for the move */
  private def applyNavigationResultWithAnimation(result: ScalaFXNavigationHandler.NavigationResult, animate: Boolean): Unit = {
    // Exit best response preview if active
    if (bestResponsePreviewActive) {
      bestResponsePreviewActive = false
      bestResponsePreviewIndex = -1
      notificationCenter.clear("best-response-preview")
    }
    
    clearSelection()
    boardView.clearLegalMoveTargets()
    boardView.clearMoveClassificationHighlight()
    // Navigating in mainline clears active variation context
    viewingVariationRootTob = None
    currentVariationMove = None
    controller.variationMode = false
    controller.variationRootTob = None

    // Update viewing history state based on navigation
    val mainline = controller.moveHistoryManager.getMainlineMoves
    val mainlineSize = mainline.size
    result.move match
      case Some(move) =>
        val idx = mainline.indexWhere(_.tob == move.tob)
        if idx >= 0 then
          currentMoveIndex = idx
          // In review mode, always treat as viewing history to enable variation UI
          isViewingHistory = isInReviewMode || idx < mainlineSize - 1
          
          // Show classification highlight in review mode
          if isInReviewMode then
            moveClassifications.get(idx).foreach { classInfo =>
              indicesFromSquareName(move.to.name).foreach { case (toRow, toCol) =>
                boardView.setMoveClassificationHighlight(toRow, toCol, classInfo.symbol)
              }
            }
        else
          currentMoveIndex = -1
          isViewingHistory = isInReviewMode

        val maybeCoords = for
          from <- indicesFromSquareName(move.from.name)
          to   <- indicesFromSquareName(move.to.name)
        yield (from, to)

        maybeCoords match
          case Some(((fromRow, fromCol), (toRow, toCol))) =>
            if animate then
              // Set coords without refresh, then animate
              boardView.setLastMoveCoords(fromRow, fromCol, toRow, toCol)
              val pieceColor = move.piece.color
              val pieceType = move.piece.pieceType.toString.toLowerCase
              boardView.animatePieceMove(fromRow, fromCol, toRow, toCol, pieceColor, pieceType, () => {
                updateMoveArrows()
              })
            else
              boardView.setLastMove(fromRow, fromCol, toRow, toCol)
          case None =>
            boardView.clearLastMove()
      case None =>
        currentMoveIndex = -1
        // In review mode, always treat as viewing history to enable variation UI
        isViewingHistory = isInReviewMode || mainlineSize > 0
        boardView.clearLastMove()

    updateStatusLabel()
    refreshMoveHistory()
    if !animate then updateMoveArrows()  // If animated, arrows are updated after animation
    analysisManager.triggerAnalysisIfActive()
  }

  private def handleMoveHistorySelection(clickedIndex: Int): Unit = {
    // When viewing a variation, clicking on mainline exits variation
    // Otherwise, standard mainline navigation
    
    viewingVariationRootTob match
      case Some(rootTob) =>
        val anchorIdx = controller.moveHistoryManager.getMainlineAnchorIndexForVariation(rootTob)
        
        if clickedIndex <= anchorIdx then
          // Clicking on mainline - exit variation and navigate there
          viewingVariationRootTob = None
          currentVariationMove = None
          controller.variationMode = false
          navigationHandler.navigateToHalfMoveIndex(clickedIndex) match
            case Some(result) => applyNavigationResult(result)
            case None => () // Starting position
        else
          // Clicking within variation - navigate to that position
          val variationLine = controller.moveHistoryManager.collectVariationLine(rootTob)
          val posInVariation = clickedIndex - anchorIdx - 1
          if posInVariation >= 0 && posInVariation < variationLine.length then
            val result = moveReplayer.restoreVariationPosition(rootTob, posInVariation)
            currentVariationMove = result.move
            controller.updateAllPossibleMoves()
            boardView.refreshFromModel()
            refreshMoveHistory()
            updateMoveArrows()
            updateStatusLabel()
            analysisManager.triggerAnalysisIfActive()
      
      case None =>
        // Not viewing a variation - standard mainline navigation
        navigationHandler.navigateToHalfMoveIndex(clickedIndex) match
          case Some(result) =>
            applyNavigationResult(result)
          case None =>
            if clickedIndex >= 0 then
              notificationCenter.showWarning(
                key = s"navigation-move-$clickedIndex",
                title = "Navigation unavailable",
                message = "Could not navigate to the selected move."
              )
            else
              notificationCenter.showWarning(
                key = "navigation-start",
                title = "Navigation unavailable",
                message = "Starting position is already displayed."
              )
  }

  private def indicesFromSquareName(name: String): Option[(Int, Int)] =
    if name.length != 2 then return None
    val fileChar = name.charAt(0).toLower
    val rankChar = name.charAt(1)
    if fileChar < 'a' || fileChar > 'h' then return None
    if rankChar < '1' || rankChar > '8' then return None
    val col = fileChar - 'a'
    val row = 8 - (rankChar - '0')
    Some(row -> col)

  // -------------------------------------------------------------------------
  // Refresh helpers
  // -------------------------------------------------------------------------

  private def refreshEntireUI(): Unit = {
    boardView.refreshFromModel()
    boardView.clearLastMove()
    boardView.clearLegalMoveTargets()
    navigationHandler.syncToLatest()
    refreshMoveHistory()
    updateStatusLabel()
    notificationCenter.clearAll()
    updateMoveArrows()
  }

  private def refreshMoveHistory(): Unit = {
    // Check if we're viewing a variation (Move-based)
    viewingVariationRootTob match
      case Some(rootTob) =>
        // Get variation line and mainline
        val variationLine = controller.moveHistoryManager.collectVariationLine(rootTob)
        val mainlineNotations = controller.moveHistoryManager.getMoveNotations
        val mainlineAnchorIdx = controller.moveHistoryManager.getMainlineAnchorIndexForVariation(rootTob)
        
        // Check if this is a nested variation and calculate depth
        val parentVariationRootOpt = controller.moveHistoryManager.getParentVariationRootTob(rootTob)
        
        // Calculate variation depth (1 = first level, 2 = nested, etc.)
        def calculateVariationDepth(varRootTob: Long): Int =
          controller.moveHistoryManager.getParentVariationRootTob(varRootTob) match
            case Some(parentRoot) => 1 + calculateVariationDepth(parentRoot)
            case None => 1
        
        val variationDepth = calculateVariationDepth(rootTob)
        // Unicode subscript digits: ₀₁₂₃₄₅₆₇₈₉
        val subscriptDigits = "₀₁₂₃₄₅₆₇₈₉"
        val depthSubscript = if variationDepth > 1 then 
          variationDepth.toString.map(c => subscriptDigits(c - '0')).mkString
        else ""
        
        // Helper to collect all ancestor variations from current to mainline
        def collectAncestorChain(varRootTob: Long): List[(Long, Int)] =
          // Returns list of (variationRootTob, branchIndexInParent) from root-level to deepest
          controller.moveHistoryManager.getParentVariationRootTob(varRootTob) match
            case Some(parentRootTob) =>
              val branchIdx = controller.moveHistoryManager.getParentVariationBranchIndex(varRootTob)
              collectAncestorChain(parentRootTob) :+ (varRootTob, branchIdx)
            case None =>
              // This is root-level variation
              List((varRootTob, -1))  // -1 means branches from mainline
        
        // Build combined display including all ancestor variations
        val (combinedNotations, currentOffset, branchIndicatorIdx) = parentVariationRootOpt match
          case Some(_) =>
            // Nested variation: need to show mainline + all ancestor variations + current variation
            val ancestorChain = collectAncestorChain(rootTob)
            
            // Start with mainline to anchor
            var combined = mainlineNotations.take(mainlineAnchorIdx + 1).toList
            var lastBranchIdx = mainlineAnchorIdx
            
            // Add each ancestor variation up to its branch point
            ancestorChain.dropRight(1).foreach { case (ancestorRootTob, _) =>
              val ancestorLine = controller.moveHistoryManager.collectVariationLine(ancestorRootTob)
              // Find where the next level branches from this ancestor
              val nextAncestorIdx = ancestorChain.indexWhere(_._1 == ancestorRootTob) + 1
              val branchIdxInAncestor = if nextAncestorIdx < ancestorChain.size then
                ancestorChain(nextAncestorIdx)._2
              else 0
              
              val ancestorDepth = ancestorChain.indexWhere(_._1 == ancestorRootTob) + 1
              val ancestorSubscript = if ancestorDepth > 1 then
                ancestorDepth.toString.map(c => subscriptDigits(c - '0')).mkString
              else ""
              
              val movesToBranch = ancestorLine.take(branchIdxInAncestor + 1).flatMap(_.notation).map(san => s"[$san]$ancestorSubscript")
              combined = combined ++ movesToBranch
              lastBranchIdx = combined.size - 1
            }
            
            val branchIdx = lastBranchIdx
            val offset = combined.size
            
            // Add current variation moves with depth subscript
            val nestedMoves = variationLine.flatMap(_.notation).map(san => s"[$san]$depthSubscript")
            combined = combined ++ nestedMoves
            
            (combined, offset, branchIdx)
            
          case None =>
            // Root-level variation: mainline to anchor + variation
            var combined = mainlineNotations.take(mainlineAnchorIdx + 1).toList
            val branchIdx = mainlineAnchorIdx
            val offset = mainlineAnchorIdx + 1
            
            // Add variation moves
            val variationMoves = variationLine.flatMap(_.notation).map(san => s"[$san]")
            combined = combined ++ variationMoves
            
            (combined, offset, branchIdx)
        
        // Add branch indicator for current variation
        val branchIndicators = scala.collection.mutable.Map[Int, List[String]](
          branchIndicatorIdx -> List(s"↳ ${variationLine.headOption.flatMap(_.notation).getOrElse("...")}")
        )
        
        // When viewing a variation, show nested variation indicators for current variation
        for (i <- variationLine.indices) do
          val nestedRoots = controller.moveHistoryManager.getNestedVariationRootsAt(rootTob, i)
          if nestedRoots.nonEmpty then
            val displayIdx = currentOffset + i  // Position in combined display
            val nestedPreviews = nestedRoots.map(m => m.notation.getOrElse("...")).toList
            branchIndicators.update(displayIdx, nestedPreviews)
        
        // Calculate highlight position based on currentVariationMove
        val highlight = currentVariationMove match
          case Some(move) =>
            val moveIdx = variationLine.indexWhere(_.tob == move.tob)
            if moveIdx >= 0 then Some(currentOffset + moveIdx) else None
          case None =>
            if currentOffset > 0 then Some(currentOffset - 1) else None
        
        val variationLabel = if variationDepth > 1 then s"(variation level $variationDepth)" else "(variation)"
        
        val displayState = MoveHistoryDisplayState(
          notations = combinedNotations,
          highlightIndex = highlight,
          variationIndicators = branchIndicators.toMap
        )
        val panelState = MoveHistoryPanelState(displayState, variationLabel)
        moveHistoryPanel.render(panelState)
        
      case None =>
        // Normal mainline display
        val notations = controller.moveHistoryManager.getMoveNotations
        val mainline = controller.moveHistoryManager.getMainlineMoves
        val navigationHighlight = navigationHandler.currentMove.flatMap { move =>
          val idx = mainline.indexWhere(_.tob == move.tob)
          if idx >= 0 then Some(idx) else None
        }
        val highlight = navigationHighlight
        val indicators = variationIndicatorsForDisplay()
        // Include classifications if we have them (for review mode)
        val displayState = MoveHistoryDisplayState(
          notations = notations,
          highlightIndex = highlight,
          variationIndicators = indicators,
          classifications = moveClassifications
        )
        val statusText = if moveClassifications.nonEmpty then "Review Mode" else ""
        val panelState = MoveHistoryPanelState(displayState, statusText)
        moveHistoryPanel.render(panelState)
  }
  
  /** Refresh move history with analysis color-coding */
  private def refreshMoveHistoryWithClassifications(): Unit = {
    val notations = controller.moveHistoryManager.getMoveNotations
    val mainline = controller.moveHistoryManager.getMainlineMoves
    val navigationHighlight = navigationHandler.currentMove.flatMap { move =>
      val idx = mainline.indexWhere(_.tob == move.tob)
      if idx >= 0 then Some(idx) else None
    }
    val highlight = navigationHighlight
    val indicators = variationIndicatorsForDisplay()
    
    val displayState = MoveHistoryDisplayState(
      notations = notations,
      highlightIndex = highlight,
      variationIndicators = indicators,
      classifications = moveClassifications  // Add classifications for color coding
    )
    val panelState = MoveHistoryPanelState(displayState, "Review Mode")
    moveHistoryPanel.render(panelState)
  }

  private def updateStatusLabel(): Unit = {
    controller.winner match
      case Some("draw") =>
        statusLabel.text = "Game drawn"
      case Some(color) =>
        statusLabel.text = s"${color.capitalize} wins"
      case None =>
        statusLabel.text = s"${controller.currentPlayer.capitalize} to move"
  }

  /** Extract mainline SAN moves from PGN */
  private def extractMainlineMoves(pgnMoves: List[PGNMove]): List[String] = {
    pgnMoves.flatMap { move =>
      List(move.whiteMove, move.blackMove).flatten
    }
  }

  /** 
   * Generate FEN positions and Move objects for a game by replaying moves.
   * Returns (FENs, Moves) where both lists correspond to each move.
   * NOTE: This method should be called from a background thread as it can be slow for long games.
   */
  private def generateFENsAndMoves(pgnGame: chess.pgn.PGNGame): (List[String], List[Move]) = {
    println("DEBUG [FEN-GEN]: Starting generateFENsAndMoves")
    // Create a temporary board and controller for replay
    val tempController = new GameController()
    val tempBoard = new Board(tempController)
    tempController.board = Some(tempBoard)
    tempController.fullmoveNumber = 1  // Standard starting position
    println("DEBUG [FEN-GEN]: Temporary board/controller created")
    
    // Create minimal dependencies for MoveReplayer - no UI components needed
    val tempArrowManager = new MoveArrowManager(tempController, tempBoard, null)
    val tempReplayer = new MoveReplayer(tempController, tempBoard, tempArrowManager)
    println("DEBUG [FEN-GEN]: MoveReplayer created")
    
    // Generate starting position FEN
    val startingFEN = FENGenerator.generateFEN(tempBoard, tempController)
    println(s"DEBUG [FEN-GEN]: Starting FEN: $startingFEN")
    
    // Replay each move and collect FENs and Move objects
    val fens = scala.collection.mutable.ListBuffer(startingFEN)
    val moves = scala.collection.mutable.ListBuffer[Move]()
    
    // Extract mainline moves
    val sanMoves = extractMainlineMoves(pgnGame.moves)
    println(s"DEBUG [FEN-GEN]: Extracted ${sanMoves.length} SAN moves")
    
    var isWhiteMove = true
    var moveCounter = 0
    var successCount = 0
    var failCount = 0
    
    sanMoves.foreach { sanMove =>
      // Find the piece and squares involved in this move using the arrow manager
      tempArrowManager.parseSanMoveForArrow(sanMove, isWhiteMove) match {
        case Some((fromSquare, toSquare)) =>
          val piece = fromSquare.occupiedBy.get  // Should always exist if parsing succeeded
          val capturedPiece = toSquare.occupiedBy
          
          // Create the Move object before replaying
          val move = Move(
            piece = piece,
            from = fromSquare,
            to = toSquare,
            capturedPiece = capturedPiece,
            promotedPieceType = None, // TODO: Parse promotion from SAN
            tob = System.nanoTime(),
            parentTob = -1L,
            halfmoveDistanceFromStart = moveCounter,
            notation = Some(sanMove),
            isVariation = false,
            variationRootTob = None
          )
          moves += move
          
          // Now replay the move
          tempReplayer.replayMoveFromSan(sanMove, isWhiteMove)
          
          // Generate FEN for this position
          val fen = FENGenerator.generateFEN(tempBoard, tempController)
          fens += fen
          successCount += 1
          
          // Progress indicator every 20 moves
          if (moveCounter % 20 == 0) {
            println(s"DEBUG [FEN-GEN]: Processed $moveCounter/${sanMoves.length} moves...")
          }
          
        case None =>
          println(s"DEBUG [FEN-GEN-WARN]: Could not parse move #$moveCounter: $sanMove")
          failCount += 1
      }
      
      // Alternate color
      isWhiteMove = !isWhiteMove
      moveCounter += 1
    }
    
    println(s"DEBUG [FEN-GEN]: Complete! Generated ${fens.size} FENs, ${moves.size} moves (success=$successCount, fail=$failCount)")
    (fens.toList, moves.toList)
  }

  /** Split a multi-game PGN file into individual game files */
  private def splitMultiGamePGN(): Unit = {
    import chess.tools.PgnSplitter
    
    val fileChooser = new FileChooser {
      title = "Select Multi-Game PGN File"
      extensionFilters.add(
        new FileChooser.ExtensionFilter("PGN Files", "*.pgn")
      )
      // Set initial directory to Downloads
      val downloadsDir = new File(System.getProperty("user.home"), "Downloads")
      initialDirectory = if (downloadsDir.exists() && downloadsDir.isDirectory) {
        downloadsDir
      } else {
        new File(System.getProperty("user.home"))
      }
    }
    
    Option(fileChooser.showOpenDialog(stage)) match {
      case Some(file) =>
        // Run in background to not block UI
        Future {
          try {
            // Read file content
            val source = scala.io.Source.fromFile(file, "UTF-8")
            val content = source.mkString
            source.close()
            
            // Parse games
            val games = PgnSplitter.parseGames(content)
            
            if (games.isEmpty) {
              Platform.runLater {
                notificationCenter.showWarning("pgn-split", "PGN Split", "Ei löytynyt pelejä tiedostosta.")
              }
            } else {
              // Determine output directory (Peliarkisto in project root)
              val projectRoot = new File(".").getCanonicalPath
              val outputDir = new File(projectRoot, "Peliarkisto")
              
              if (!outputDir.exists()) {
                outputDir.mkdirs()
              }
              
              // Save each game
              var savedCount = 0
              val savedFiles = scala.collection.mutable.ListBuffer[String]()
              
              for (game <- games) {
                val filename = PgnSplitter.generateFilename(game, outputDir)
                val outputFile = new File(outputDir, filename)
                
                scala.util.Try {
                  val writer = new java.io.PrintWriter(outputFile, "UTF-8")
                  writer.write(game.content)
                  writer.close()
                }.fold(
                  _ => (), // Ignore errors
                  _ => {
                    savedCount += 1
                    savedFiles += s"${game.white} vs ${game.black}"
                  }
                )
              }
              
              Platform.runLater {
                if (savedCount > 0) {
                  val gameList = savedFiles.take(5).mkString("\n")
                  val extra = if (savedCount > 5) s"\n... ja ${savedCount - 5} muuta" else ""
                  notificationCenter.showInfo("pgn-split", "PGN Split", s"Tallennettu $savedCount peliä Peliarkisto-kansioon:\n$gameList$extra")
                } else {
                  notificationCenter.showWarning("pgn-split", "PGN Split", "Pelien tallentaminen epäonnistui.")
                }
              }
            }
          } catch {
            case e: Exception =>
              Platform.runLater {
                notificationCenter.showError("pgn-split", "Virhe", s"PGN Split epäonnistui: ${e.getMessage}")
              }
          }
        }
      case None =>
        // User cancelled
    }
  }

  /** Start Error Training mode */
  private def startErrorTraining(): Unit = {
    // Hide other panels and show training panel
    isInTrainingMode = true
    
    // Setup callbacks
    errorTrainingPanel.setOnSetupPosition { fen =>
      // Set board position from FEN
      setupBoardFromFEN(fen)
    }
    
    errorTrainingPanel.setOnMakeMove { (from, to, promotion) =>
      // Make a move on the board (for Stockfish responses)
      makeTrainingMove(from, to, promotion)
    }
    
    errorTrainingPanel.setOnGetCurrentFEN { () =>
      // Return current board FEN
      FENGenerator.generateFEN(boardModel, controller)
    }
    
    errorTrainingPanel.setOnExitTraining { () =>
      exitErrorTrainingMode()
    }
    
    errorTrainingPanel.setOnFlipBoard { shouldFlip =>
      // Flip board so player is at the bottom
      if (shouldFlip != isBoardFlipped) {
        flipBoard()
      }
    }
    
    errorTrainingPanel.setOnMakeMoveAnimated { (from, to, promotion, onComplete) =>
      // Make a move with animation
      makeTrainingMoveAnimated(from, to, promotion, onComplete)
    }
    
    errorTrainingPanel.setOnShowArrow { (from, to, color) =>
      // Show arrow on the board
      showTrainingArrow(from, to, color)
    }
    
    errorTrainingPanel.setOnClearArrows { () =>
      // Clear all arrows
      boardView.setMoveHints(Seq.empty)
    }
    
    errorTrainingPanel.setOnUpdateAnalysisPanel { () =>
      // Update engine analysis panel
      analysisManager.triggerAnalysisIfActive()
    }
    
    errorTrainingPanel.setOnPlayBestLine { (moves, isWhiteToMove, onComplete) =>
      // Play the best line to show why the move was bad
      playTrainingBestLine(moves, isWhiteToMove, onComplete)
    }
    
    // Show training panel
    errorTrainingPanel.show()
  }
  
  /** Make a move during training (for Stockfish responses) */
  private def makeTrainingMove(from: String, to: String, promotion: Option[String]): Unit = {
    val fromSquare = boardModel.getSquareByName(from.toLowerCase)
    val toSquare = boardModel.getSquareByName(to.toLowerCase)
    
    (fromSquare, toSquare) match {
      case (Some(fromSq), Some(toSq)) =>
        fromSq.occupiedBy match {
          case Some(piece) =>
            val validMoves = controller.filterMovesToPreventCheck(piece, piece.possibleMoves())
            if (validMoves.contains(toSq)) {
              // Check if this is pawn promotion
              val (_, toRow) = toSq.getCoordinates
              val isPromotion = piece.pieceType == PieceType.Pawn && (toRow == 0 || toRow == 7)
              
              if (isPromotion && promotion.isDefined) {
                controller.handleAction(piece, toSq)
              } else {
                controller.handleAction(piece, toSq)
              }
              boardView.refreshFromModel()
              updateStatusLabel()
            }
          case None =>
        }
      case _ =>
    }
  }
  
  /** Make a move with animation during training */
  private def makeTrainingMoveAnimated(from: String, to: String, @annotation.unused promotion: Option[String], onComplete: () => Unit): Unit = {
    val fromSquare = boardModel.getSquareByName(from.toLowerCase)
    val toSquare = boardModel.getSquareByName(to.toLowerCase)
    
    (fromSquare, toSquare) match {
      case (Some(fromSq), Some(toSq)) =>
        fromSq.occupiedBy match {
          case Some(piece) =>
            val validMoves = controller.filterMovesToPreventCheck(piece, piece.possibleMoves())
            if (validMoves.contains(toSq)) {
              // Get coordinates for animation
              val fromCoords = toIndices(fromSq)
              val toCoords = toIndices(toSq)
              
              (fromCoords, toCoords) match {
                case (Some((fromRow, fromCol)), Some((toRow, toCol))) =>
                  // Animate piece move, then execute move
                  boardView.animatePieceMove(fromRow, fromCol, toRow, toCol,
                    piece.color, piece.pieceType.toString.toLowerCase, () => {
                      // Execute the actual move (promotion handled by controller)
                      controller.handleAction(piece, toSq)
                      boardView.refreshFromModel()
                      updateStatusLabel()
                      onComplete()
                    })
                case _ =>
                  // Fallback - no animation
                  controller.handleAction(piece, toSq)
                  boardView.refreshFromModel()
                  updateStatusLabel()
                  onComplete()
              }
            } else {
              onComplete()
            }
          case None =>
            onComplete()
        }
      case _ =>
        onComplete()
    }
  }
  
  /** Show an arrow on the board during training */
  private def showTrainingArrow(from: String, to: String, colorHex: String): Unit = {
    import ScalaFXBoardView.MoveArrow
    import scalafx.scene.paint.Color
    
    // Convert square names to indices
    val fromCoords = squareNameToCoords(from)
    val toCoords = squareNameToCoords(to)
    
    (fromCoords, toCoords) match {
      case (Some((fromRow, fromCol)), Some((toRow, toCol))) =>
        val color = Color.web(colorHex)
        val arrow = MoveArrow(fromRow, fromCol, toRow, toCol, color)
        boardView.setMoveHints(Seq(arrow))
      case _ =>
        // Invalid coordinates
    }
  }
  
  /** Convert square name (e.g., "e4") to board coordinates */
  private def squareNameToCoords(name: String): Option[(Int, Int)] = {
    if (name.length >= 2) {
      val file = name.charAt(0).toLower - 'a'
      val rank = name.charAt(1).asDigit
      if (file >= 0 && file < 8 && rank >= 1 && rank <= 8) {
        val row = 8 - rank
        Some((row, file))
      } else None
    } else None
  }
  
  /** Exit Error Training mode */
  private def exitErrorTrainingMode(): Unit = {
    isInTrainingMode = false
    errorTrainingPanel.hide()
    
    // Reset board to starting position
    startNewGame()
  }
  
  /**
   * Play a sequence of moves (PV line) on the board with animation.
   * Used to show why a bad move was bad by playing opponent's best continuation.
   */
  private def playTrainingBestLine(moves: List[String], isWhiteToMove: Boolean, onComplete: () => Unit): Unit = {
    if (moves.isEmpty) {
      onComplete()
      return
    }
    
    val boardInstance = controller.board.get
    
    // Show notification
    notificationCenter.showInfo(
      key = "training-best-line",
      title = "Vastustajan paras jatko",
      message = s"Näytetään ${moves.length} siirtoa..."
    )
    
    import java.util.{Timer, TimerTask}
    val timer = new Timer()
    var moveIndex = 0
    var currentIsWhiteToMove = isWhiteToMove
    
    def playNextMove(): Unit = {
      if (moveIndex < moves.length) {
        val uciMove = moves(moveIndex)
        
        Platform.runLater {
          if (uciMove.length >= 4) {
            val fromName = uciMove.substring(0, 2)
            val toName = uciMove.substring(2, 4)
            val promotionPiece = if (uciMove.length > 4) Some(uciMove.charAt(4)) else None
            
            boardInstance.getSquareByName(fromName).foreach { fromSquare =>
              boardInstance.getSquareByName(toName).foreach { toSquare =>
                fromSquare.occupiedBy.foreach { piece =>
                  // Move piece visually
                  val captured = toSquare.occupiedBy
                  captured.foreach(_.capture())
                  piece.moveTo(toSquare)
                  piece.hasMoved = true
                  
                  // Handle pawn promotion
                  promotionPiece.foreach { promChar =>
                    val newType = promChar.toLower match {
                      case 'q' => "Queen"
                      case 'r' => "Rook"
                      case 'b' => "Bishop"
                      case 'n' => "Knight"
                      case _ => "Queen"
                    }
                    piece.promoteTo(newType)
                  }
                  
                  // Handle castling
                  if (piece.pieceType == chess.types.PieceType.King) {
                    val colDiff = toSquare.getCoordinates._1 - fromSquare.getCoordinates._1
                    if (Math.abs(colDiff) == 2) {
                      val (rookFromCol, rookToCol) = if (colDiff > 0) (7, 5) else (0, 3)
                      val row = fromSquare.getCoordinates._2
                      val rookFromSquare = boardInstance.squares(row)(rookFromCol)
                      val rookToSquare = boardInstance.squares(row)(rookToCol)
                      rookFromSquare.occupiedBy.foreach { rook =>
                        rook.moveTo(rookToSquare)
                        rook.hasMoved = true
                      }
                    }
                  }
                  
                  // Handle en passant
                  if (piece.pieceType == chess.types.PieceType.Pawn) {
                    val fromCol = fromSquare.getCoordinates._1
                    val toCol = toSquare.getCoordinates._1
                    val toRow = toSquare.getCoordinates._2
                    if (fromCol != toCol && captured.isEmpty) {
                      val capturedPawnRow = if (currentIsWhiteToMove) toRow + 1 else toRow - 1
                      val capturedPawnSquare = boardInstance.squares(capturedPawnRow)(toCol)
                      capturedPawnSquare.occupiedBy.foreach(_.capture())
                    }
                  }
                }
              }
            }
            
            // Refresh board and highlight move
            boardView.refreshFromModel()
            indicesFromSquareName(fromName).foreach { case (fromRow, fromCol) =>
              indicesFromSquareName(toName).foreach { case (toRow, toCol) =>
                boardView.setLastMove(fromRow, fromCol, toRow, toCol)
              }
            }
          }
          
          currentIsWhiteToMove = !currentIsWhiteToMove
          moveIndex += 1
          
          notificationCenter.showInfo(
            key = "training-best-line",
            title = "Vastustajan paras jatko",
            message = s"Siirto ${moveIndex}/${moves.length}"
          )
        }
        
        // Schedule next move
        if (moveIndex < moves.length) {
          timer.schedule(new TimerTask {
            override def run(): Unit = playNextMove()
          }, 750)
        } else {
          // All moves played, wait a moment then call onComplete
          timer.schedule(new TimerTask {
            override def run(): Unit = {
              timer.cancel()
              Platform.runLater {
                notificationCenter.clear("training-best-line")
                onComplete()
              }
            }
          }, 1000)
        }
      } else {
        timer.cancel()
        Platform.runLater {
          notificationCenter.clear("training-best-line")
          onComplete()
        }
      }
    }
    
    // Start playing moves after a short delay
    timer.schedule(new TimerTask {
      override def run(): Unit = playNextMove()
    }, 300)
  }

  /** Setup board from FEN string */
  private def setupBoardFromFEN(fen: String): Unit = {
    import chess.analysis.FENGenerator
    
    // Parse and apply FEN
    FENGenerator.setupBoardFromFEN(boardModel, fen) match {
      case Right(_) =>
        // Update current player based on FEN
        val isWhiteToMove = fen.split(" ").lift(1).contains("w")
        controller.currentPlayer = if (isWhiteToMove) "white" else "black"
        
        boardView.refreshFromModel()
        updateStatusLabel()
        
      case Left(errMsg) =>
        notificationCenter.showError(
          key = "fen-error",
          title = "FEN Error",
          message = s"Could not set position: $errMsg"
        )
    }
  }

  /** Analyze the current game with Stockfish */
  private def analyzeCurrentGame(): Unit = {
    // Show file chooser to load PGN
    val fileChooser = new FileChooser {
      title = "Select PGN File to Analyze"
      extensionFilters.add(
        new FileChooser.ExtensionFilter("PGN Files", "*.pgn")
      )
      // Set initial directory to Peliarkisto
      val peliarkistoDir = new File("Peliarkisto")
      initialDirectory = if (peliarkistoDir.exists() && peliarkistoDir.isDirectory) {
        peliarkistoDir
      } else {
        new File(System.getProperty("user.home"))
      }
    }
    
    Option(fileChooser.showOpenDialog(stage)) match {
      case Some(file) =>
        println(s"DEBUG [1/10]: File selected: ${file.getName}")
        // Show progress overlay immediately
        analysisProgressOverlay.setTitle("Analyzing Game")
        analysisProgressOverlay.setStatus("Loading PGN...")
        analysisProgressOverlay.show()
        println("DEBUG [2/10]: Progress overlay shown")
        
        Future {
          try {
            println("DEBUG [3/10]: Future started - loading PGN in background thread")
            // Load and parse PGN in background
            val pgnSource = scala.io.Source.fromFile(file)
            val pgnContent = pgnSource.mkString
            pgnSource.close()
            
            println(s"DEBUG [3.1/10]: PGN file read, ${pgnContent.length} characters")
            val parseResult = chess.pgn.PGNParser.parsePGN(pgnContent)
            println(s"DEBUG [3.2/10]: PGN parsing complete, game=${parseResult.game.isDefined}, errors=${parseResult.errors.size}")
            parseResult.game match {
              case None =>
                println(s"DEBUG [ERROR]: PGN parsing failed: ${parseResult.errors}")
                Platform.runLater {
                  analysisProgressOverlay.hide()
                  notificationCenter.showError(
                    key = "parse-error",
                    title = "Parse Error",
                    message = s"Failed to parse PGN: ${parseResult.errors.headOption.getOrElse("Unknown error")}"
                  )
                }
                
              case Some(pgnGame) =>
                println(s"DEBUG [4/10]: PGN parsed successfully with ${pgnGame.moves.size} moves")
                
                Platform.runLater {
                  analysisProgressOverlay.setStatus("Preparing positions...")
                }
                
                // Generate FENs and moves in background thread
                println("DEBUG [5/10]: Starting FEN generation...")
                val startTime = System.currentTimeMillis()
                val (fens, moves) = generateFENsAndMoves(pgnGame)
                val totalMoves = moves.length
                val fenTime = System.currentTimeMillis() - startTime
                println(s"DEBUG [5/10]: Generated $totalMoves FENs/moves in ${fenTime}ms")
                println(s"DEBUG [5.1/10]: First FEN: ${fens.headOption.getOrElse("N/A")}")
                println(s"DEBUG [5.2/10]: Last FEN: ${fens.lastOption.getOrElse("N/A")}")
                
                // Optimized for Apple Silicon
                println("DEBUG [6/10]: Creating GameAnalyzer...")
                val analyzerOpt = GameAnalyzer.create(EngineConfig(
                  depth = 14,
                  threads = 6,
                  hashSizeMB = 1024
                ))
                println(s"DEBUG [7.1/10]: GameAnalyzer created: ${analyzerOpt.isDefined}")
                
                analyzerOpt match {
                  case Some(analyzer) =>
                    println("DEBUG [8/10]: Starting Stockfish analysis...")
                    Platform.runLater {
                      analysisProgressOverlay.setStatus(s"Analyzing $totalMoves moves...")
                    }
                    
                    // Create progress callback for move-by-move analysis
                    var movesAnalyzed = 0
                    val progressCallback: Int => Unit = { currentMove =>
                      movesAnalyzed = currentMove
                      val progress = if (totalMoves > 0) currentMove.toDouble / totalMoves else 0.0
                      Platform.runLater {
                        analysisProgressOverlay.setProgress(progress)
                        analysisProgressOverlay.setStatus(s"Move $currentMove / $totalMoves")
                      }
                    }
                    
                    // Use depth 16 with progress callback
                    println("DEBUG [8.1/10]: Calling analyzeGameWithProgress...")
                    val analysisStartTime = System.currentTimeMillis()
                    analyzer.analyzeGameWithProgress(moves, fens, depth = 16, progressCallback).onComplete {
                      case scala.util.Success(analysisResults) =>
                        val analysisTime = System.currentTimeMillis() - analysisStartTime
                        println(s"DEBUG [9/10]: Analysis completed with ${analysisResults.length} results in ${analysisTime}ms")
                        analyzer.stop()  // Stop engine after analysis completes
                        println("DEBUG [9.1/10]: Stockfish stopped")
                        
                        // Store moves and FENs for review mode navigation
                        // We DON'T use pgnManager.loadPGN because it has bugs with certain moves
                        // Instead, we use the already-generated moves and fens directly
                        println("DEBUG [9.2/10]: Storing moves and FENs for review mode...")
                        
                        println("DEBUG [9.4/10]: Scheduling UI update")
                        Platform.runLater {
                          try {
                            println("DEBUG [10/10]: Platform.runLater started - final UI update")
                            analysisProgressOverlay.hide()
                            println("DEBUG [10.1/10]: Overlay hidden")
                            
                            // Reset controller and populate with generated moves directly
                            println("DEBUG [10.1.1/10]: Resetting controller for fresh game state...")
                            controller.resetGame()
                            controller.fullmoveNumber = 1
                            
                            // Rebuild the board and replay moves
                            println(s"DEBUG [10.1.2/10]: Adding ${moves.length} moves to controller...")
                            val boardInstance = controller.board.get
                            boardInstance.resetBoard()  // Reset to starting position
                            
                            // Replay each move from the generated moves list AND add to move history
                            var moveIndex = 0
                            var isWhiteToMove = true
                            var moveNumber = 1  // Full move number for notation
                            var parentTob = -1L  // Track parent for proper tree structure
                            val tempArrowManager = new MoveArrowManager(controller, boardInstance, null)
                            val tempReplayer = new MoveReplayer(controller, boardInstance, tempArrowManager)
                            
                            moves.foreach { move =>
                              move.notation match {
                                case Some(san) =>
                                  // 1. Replay the move on the actual board
                                  tempReplayer.replayMoveFromSan(san, isWhiteToMove)
                                  
                                  // 2. Create proper Move object with correct parentTob and add to history
                                  val newMove = move.copy(
                                    tob = System.nanoTime(),
                                    parentTob = parentTob,
                                    halfmoveDistanceFromStart = moveIndex + 1,  // 1-based for move history
                                    isVariation = false,
                                    variationRootTob = None
                                  )
                                  
                                  // Add to move history manager (includes notation manager)
                                  controller.moveHistoryManager.addMoveToNotation(
                                    newMove, 
                                    san, 
                                    moveNumber, 
                                    isWhiteToMove
                                  )
                                  
                                  // Update parent for next move
                                  parentTob = newMove.tob
                                  
                                  // Update move number after black's move
                                  if (!isWhiteToMove) moveNumber += 1
                                  
                                case None =>
                                  println(s"DEBUG [WARNING]: Move at index $moveIndex has no notation!")
                              }
                              isWhiteToMove = !isWhiteToMove
                              moveIndex += 1
                            }
                            
                            println(s"DEBUG [10.1.3/10]: Replayed $moveIndex moves successfully")
                            
                            // Store analysis data for review mode
                            gameAnalysisFens = fens
                            gameAnalysisMoves = moves
                            
                            println("DEBUG [10.3/10]: Refreshing UI components")
                            boardView.refreshFromModel()
                            refreshMoveHistory()
                            updateStatusLabel()
                            println("DEBUG [10.4/10]: UI components refreshed")
                            
                            // Navigate to end of game
                            val mainlineMoves = controller.moveHistoryManager.getMainlineMoves
                            println(s"DEBUG [10.5/10]: Controller has ${mainlineMoves.length} mainline moves")
                            if (mainlineMoves.nonEmpty) {
                              println(s"DEBUG [10.6/10]: Navigating to move index ${mainlineMoves.length - 1}")
                              navigationHandler.navigateToHalfMoveIndex(mainlineMoves.length - 1).foreach(applyNavigationResult)
                            } else {
                              println("DEBUG [WARNING]: No mainline moves found in controller!")
                            }
                            println("DEBUG [10.7/10]: Navigation complete")
                            
                            println("DEBUG [10.8/10]: Setting analysis results to GameReviewPanel")
                            // Update the embedded game review panel
                            gameReviewPanel.setAnalysisResults(analysisResults)
                            println("DEBUG [10.9/10]: Analysis results set")
                            
                            // Store analysis results for engine arrows in review mode
                            gameAnalysisResults = analysisResults
                          
                            // Build classifications map for later use
                            val classifications = analysisResults.zipWithIndex.map { case (analysis, idx) =>
                              val (symbol, color) = gameReviewPanel.getSymbolAndColor(analysis.classification)
                              idx -> MoveClassificationInfo(symbol, color)
                            }.toMap
                            moveClassifications = classifications
                            
                            // Set up Start Review callback - this switches to review mode
                            gameReviewPanel.setOnStartReview { () =>
                              isInReviewMode = true
                              // Refresh move history with color-coded classifications
                              refreshMoveHistoryWithClassifications()
                              
                              // Navigate to first move (index 0)
                              navigationHandler.navigateToHalfMoveIndex(0).foreach { result =>
                                applyNavigationResult(result)
                              }
                            }
                            
                            // Set up Navigate to Move callback - for classification browsing
                            gameReviewPanel.setOnNavigateToMove { moveIndex =>
                              isInReviewMode = true
                              refreshMoveHistoryWithClassifications()
                              navigationHandler.navigateToHalfMoveIndex(moveIndex).foreach { result =>
                                applyNavigationResult(result)
                              }
                            }
                            
                            // Set up Best Response callback
                            gameReviewPanel.setOnBestResponse { () =>
                              playBestResponse()
                            }
                            
                            // Show success notification
                            notificationCenter.showInfo(
                              key = "analysis-complete",
                              title = "Analysis Complete",
                              message = s"Analyzed ${analysisResults.length} moves"
                            )
                            println("DEBUG [SUCCESS]: Game Review process completed successfully!")
                            println(s"DEBUG [SUMMARY]: Analyzed ${analysisResults.length} moves, controller has ${mainlineMoves.length} moves")
                          } catch {
                            case e: Exception =>
                              println(s"DEBUG: Error in Platform.runLater: ${e.getMessage}")
                              e.printStackTrace()
                              analysisProgressOverlay.hide()
                              notificationCenter.showError(
                                key = "ui-error",
                                title = "UI Error",
                                message = s"Error displaying results: ${e.getMessage}"
                              )
                          }
                        }
                      
                      case scala.util.Failure(exception) =>
                        println(s"DEBUG: Analysis failed with exception: ${exception.getMessage}")
                        analyzer.stop()  // Stop engine even on failure
                        Platform.runLater {
                          analysisProgressOverlay.hide()
                          notificationCenter.showError(
                            key = "analysis-error",
                            title = "Analysis Failed",
                            message = s"Error: ${exception.getMessage}"
                          )
                        }
                    }
                  case None =>
                    Platform.runLater {
                      analysisProgressOverlay.hide()
                      notificationCenter.showError(
                        key = "stockfish-error",
                        title = "Stockfish Not Found",
                        message = "Please install Stockfish: brew install stockfish"
                      )
                    }
                }
            }
          } catch {
            case e: Exception =>
              Platform.runLater {
                analysisProgressOverlay.hide()
                notificationCenter.showError(
                  key = "analysis-error",
                  title = "Analysis Error",
                  message = s"Error: ${e.getMessage}"
                )
              }
          }
        }
      case None =>
        // User cancelled
        ()
    }
  }

  private def showWinnerIfDecided(): Unit = {
    controller.winner.foreach {
      case "draw" =>
        notificationCenter.showInfo(
          key = "game-result",
          title = "Draw",
          message = "The game has ended in a draw."
        )
      case color =>
        notificationCenter.showInfo(
          key = "game-result",
          title = "Checkmate",
          message = s"$color wins the game."
        )
    }
  }

  private def clearSelection(): Unit = {
    selectedSquareCoord = None
    controller.deselectPiece()
    boardView.clearSelectedSquare()
    boardView.clearLegalMoveTargets()
  }

  private def toIndices(square: Square): Option[(Int, Int)] = {
    val squares = boardModel.squares
    var row = 0
    while row < squares.length do
      var col = 0
      while col < squares(row).length do
        if squares(row)(col) eq square then return Some(row -> col)
        col += 1
      row += 1
    None
  }

  // -------------------------------------------------------------------------
  // Commands & utilities
  // -------------------------------------------------------------------------

  private def startNewGame(): Unit = {
    analysisManager.setAnalysisActive(enabled = false)
    notificationCenter.clearAll()
    controller.resetGame()
    boardModel.resetBoard()
    controller.updateAllPossibleMoves()
    clearVariationSelectionState()
    viewingVariationRootTob = None
    currentVariationMove = None
    controller.variationMode = false
    controller.variationRootTob = None
    currentMoveIndex = -1
    isViewingHistory = false
    // Clear game review state
    moveClassifications = Map.empty
    gameAnalysisResults = List.empty
    isInReviewMode = false
    gameReviewPanel.clear()
    refreshEntireUI()
    pgnManager.clearPGN()
  }

  /** Called after successful PGN load to reset UI state */
  private def handlePGNLoadSuccess(): Unit = {
    clearVariationSelectionState()
    viewingVariationRootTob = None
    currentVariationMove = None
    controller.variationMode = false
    controller.variationRootTob = None
    currentMoveIndex = -1
    val mainlineSize = controller.moveHistoryManager.getMainlineMoves.size
    isViewingHistory = mainlineSize > 0
    
    refreshEntireUI()
    analysisManager.triggerAnalysisIfActive()
  }
}
