package chess.ui

import chess.board.{Board, Square}
import chess.controllers.GameController
import chess.pgn.{PGNManager, PGNGame, LiveVariationRepository}
import ScalaFXBoardView.MoveArrow
import chess.pieces.{Pawn => PawnPiece, Rook => RookPiece, Bishop => BishopPiece, Knight => KnightPiece, Queen => QueenPiece, Piece}
import chess.types.PieceType
import chess.analysis.StockfishEngine
import chess.utils.DebugOnce

import scalafx.application.JFXApp3
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.application.Platform
import scalafx.scene.Scene
import scalafx.scene.layout.{VBox, HBox, BorderPane, StackPane}
import scalafx.scene.control.{Button, Label, MenuBar, Menu, MenuItem, CheckMenuItem, SeparatorMenuItem, ChoiceDialog}
import javafx.scene.control.{Label => JfxLabel}
import javafx.scene.layout.Region
import scalafx.geometry.{Insets, Pos}
import scalafx.Includes._
import scalafx.scene.input.{KeyCode, KeyEvent}
import scalafx.stage.FileChooser
import scalafx.scene.paint.Color
import java.io.File
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.collection.mutable.{LinkedHashMap, ListBuffer, Map as MutableMap}
import scala.annotation.{unused, tailrec}
import scala.compiletime.uninitialized

/**
 * Modern ScalaFX-based chess application
 * Replaces the Swing-based UI with a much more modern and visually appealing interface
 */





object ScalaFXChessApp extends JFXApp3 {

  private object UniqueLogger {
  private val seenNumbers: scala.collection.mutable.Set[Int] = scala.collection.mutable.Set.empty[Int]

  def logOnce(number: Int): Unit = {
    if (!seenNumbers.contains(number)) {
      println(number)
      seenNumbers += number
    }
  }
}

  // --- Core game model reused from existing Swing app ---
  private val controller = new GameController()
  private val boardModel = new Board(controller)
  // In the Swing app, controller.board is set to Some(board); mirror that here
  controller.board = Some(boardModel)

  // ScalaFX move history panel encapsulating list + status
  private lazy val moveHistoryPanel = new ScalaFXMoveHistoryPanel()

  private val liveVariationRepository = new LiveVariationRepository()
  private type LiveVariation = liveVariationRepository.LiveVariation
  private type VariationId = liveVariationRepository.VariationId

  private final class ActiveVariationSession(
    val anchorIndex: Int,
    val baseNotationSize: Int,
    val startingMoveNumber: Int,
    val startingColor: String,
    val moves: ListBuffer[String],
    val originalFutureNotations: List[String],
    initialParentContext: Option[(VariationId, Int)] = None
  ) {
    var resumedFrom: Option[LiveVariation] = None
    var parentContextForDraft: Option[(VariationId, Int)] = initialParentContext
    var pendingReplay: List[String] = Nil
    var pendingReplayColor: String = startingColor

    def setResumedVariation(variation: LiveVariation): Unit =
      resumedFrom = Some(variation)
      parentContextForDraft = variationParentContext(variation)

    def branchParentContextForAnchor(anchorIndex: Int): Option[(VariationId, Int)] =
      sessionBranchParentContext(this, Some(anchorIndex))
  }

  private var activeVariationSession: Option[ActiveVariationSession] = None

  private case class VariationSelectionState(
    anchorIndex: Int,
    variations: Vector[LiveVariation],
    selectedIndex: Int
  )

  private var variationSelectionState: Option[VariationSelectionState] = None
  private var showMoveArrows: Boolean = true
  private var pendingBranchCallback: Option[BranchDecision => Unit] = None
  private var showMoveArrowsMenuItem: CheckMenuItem = uninitialized

  // Simple click-to-move state (mirrors Swing Game logic at a high level)
  private var selectedSquare: Option[chess.board.Square] = None

  private var finishVariationMenuItem: MenuItem = uninitialized
  private var cancelVariationMenuItem: MenuItem = uninitialized

  // ScalaFX board view promoted to a field so we can use it in stage-level
  // key handlers for navigation.
  private lazy val boardView = new ScalaFXBoardView()

  private val pgnManager = new PGNManager(controller, boardModel, Some(liveVariationRepository))

  private lazy val stockfishEngine = new StockfishEngine()
  private lazy val notificationCenter = new NotificationCenter()
  private lazy val analysisManager = new AnalysisManager(stockfishEngine, boardModel, controller, notificationCenter)
  private var centerContent: HBox = uninitialized

  private case class ParsedSanMove(from: Square, to: Square, promotion: Option[PieceType])
  private var analysisPanelContainer: VBox = uninitialized

  private def triggerAnalysisIfActive(): Unit =
    analysisManager.triggerAnalysisIfActive()

  // Live move navigation state for ScalaFX (no PGN/variations yet)
  // -1 means current position, 0..N-1 means viewing after that move index
  private var currentMoveIndex: Int = -1
  private var isViewingHistory: Boolean = false

  private def traceVariation(action: String)(details: => String): Unit = ()

  private def formatSanList(notations: Iterable[String]): String =
    if notations.isEmpty then "<empty>" else notations.mkString(" ")

  private def describeSession(session: ActiveVariationSession): String =
    val resumedDesc = session.resumedFrom.map(v => s"resumed=${v.id}").getOrElse("resumed=<none>")
    val parentCtx = session.parentContextForDraft.map { case (pid, ply) => s"parent=$pid ply=$ply" }.getOrElse("parent=<none>")
    val originalFuture = formatSanList(session.originalFutureNotations)
    val movesPart = formatSanList(session.moves)
    s"anchor=${session.anchorIndex} base=${session.baseNotationSize} start=${session.startingMoveNumber}.${session.startingColor} moves=$movesPart future=$originalFuture $resumedDesc $parentCtx"

  private sealed trait BranchDecision
  private case object BranchNormal extends BranchDecision
  private case class BranchOverwrite(anchorIndex: Int) extends BranchDecision
  private case class BranchVariation(anchorIndex: Int, parentContext: Option[(VariationId, Int)]) extends BranchDecision
  private case object BranchCanceled extends BranchDecision

  private def startingMoveNumberFromAnchor(anchorIndex: Int): Int =
    ((anchorIndex + 1) / 2) + 1

  private def startingColorFromAnchor(anchorIndex: Int): String =
    if ((anchorIndex + 1) % 2 == 0) "white" else "black"

  private def moveNumberAndColorAfterPly(
    startingMoveNumber: Int,
    startingColor: String,
    plyOffset: Int
  ): (Int, String) = {
    val moveNumber = startingMoveNumber + (plyOffset / 2)
    val sameColor = plyOffset % 2 == 0
    val color = startingColor.toLowerCase match
      case "white" => if sameColor then "white" else "black"
      case "black" => if sameColor then "black" else "white"
      case other    => other
    (moveNumber, color)
  }

  private def updateVariationMenuState(): Unit = {
    val active = activeVariationSession.isDefined
    if finishVariationMenuItem != null then
      finishVariationMenuItem.disable = !active || activeVariationSession.forall(_.moves.isEmpty)
    if cancelVariationMenuItem != null then
      cancelVariationMenuItem.disable = !active
  }

  private def variationIndicatorsForDisplay(): Map[Int, List[String]] = {
    val combined = MutableMap.empty[Int, List[String]]

    liveVariationRepository.variationIndicators().foreach { case (key, previews) =>
      val normalizedKey = if key >= 0 then key else 0
      combined.update(normalizedKey, previews)
    }

    activeVariationSession.foreach { session =>
      val anchorKey = if session.anchorIndex >= 0 then session.anchorIndex else 0
      val prefix =
        if session.startingColor == "white" then s"${session.startingMoveNumber}."
        else s"${session.startingMoveNumber}..."
      val head = session.moves.take(3).mkString(" ")
      val ellipsis = if session.moves.length > 3 then " ..." else ""
      val draftLabel =
        if session.moves.nonEmpty then s"$prefix $head$ellipsis (draft)".trim
        else s"$prefix (draft variation)"

      val existing = combined.getOrElse(anchorKey, Nil)
      combined.update(anchorKey, (existing :+ draftLabel).distinct)
    }

    combined.toMap
  }

  private def variationParentContext(variation: LiveVariation): Option[(VariationId, Int)] =
    for
      parentId <- variation.parentId
      parentPly <- variation.parentAnchorPly
    yield parentId -> parentPly

  private def parentContextForAnchor(anchorIndex: Int): Option[(VariationId, Int)] =
    activeVariationSession.flatMap { session =>
      val coverageStart = math.min(session.anchorIndex, session.baseNotationSize - 1)
      val coverageEnd = session.baseNotationSize + session.moves.length - 1
      if anchorIndex >= coverageStart && anchorIndex <= coverageEnd then
        session.branchParentContextForAnchor(anchorIndex)
      else None
    }

  private def variationMatchesContext(variation: LiveVariation, parentCtx: Option[(VariationId, Int)]): Boolean =
    parentCtx match
      case Some((parentId, parentPly)) =>
        variation.parentId.contains(parentId) && variation.parentAnchorPly.contains(parentPly)
      case None                => variation.parentId.isEmpty

  private def locateSessionVariation(session: ActiveVariationSession): Option[LiveVariation] = {
    val sessionMoves = session.moves.toList
    if sessionMoves.isEmpty then None
    else
      val parentCtx = session.parentContextForDraft
      liveVariationRepository.getVariations
        .filter(v => v.anchorIndex == session.anchorIndex && variationMatchesContext(v, parentCtx))
        .find(_.moves == sessionMoves)
  }

  private def ensureSessionVariationLinked(session: ActiveVariationSession): Unit = {
    val refreshed = session.resumedFrom.flatMap(existing =>
      liveVariationRepository.getVariations.find(_.id == existing.id)
    ).orElse(locateSessionVariation(session))

    refreshed.foreach(session.setResumedVariation)
  }

  private def sessionBranchParentContext(
    session: ActiveVariationSession,
    branchAnchorOverride: Option[Int]
  ): Option[(VariationId, Int)] =
    val baseVariation = session.resumedFrom.flatMap(existing =>
      liveVariationRepository.getVariations.find(_.id == existing.id)
    ).orElse(locateSessionVariation(session))

    baseVariation.map { variation =>
      val defaultAnchor =
        if session.moves.isEmpty then session.baseNotationSize - 1
        else session.baseNotationSize + session.moves.length - 1
      val anchorForCalculation = branchAnchorOverride.getOrElse(defaultAnchor)
      val nonNegativeAnchor = math.max(anchorForCalculation, session.baseNotationSize - 1)
      val provisional =
        if nonNegativeAnchor < session.baseNotationSize then 0
        else nonNegativeAnchor - session.baseNotationSize + 1
      val clamped = provisional.max(0).min(session.moves.length)
      variation.id -> clamped
    }

  private def syncActiveVariationWithHistory(): Unit =
    activeVariationSession.foreach { session =>
      val notations = controller.moveHistoryManager.getMoveNotations
      val baseSize = math.min(session.baseNotationSize, notations.length)

      val visibleCount =
        if isViewingHistory && currentMoveIndex >= 0 then
          math.max(0, math.min(currentMoveIndex + 1, notations.length) - baseSize)
        else
          math.max(0, notations.length - baseSize)

      val visibleMoves = notations.drop(baseSize).take(visibleCount)

      val savedMovesOpt =
        session.resumedFrom
          .map(_.moves)
          .orElse(locateSessionVariation(session).map(_.moves))

      session.moves.clear()
      session.moves.appendAll(visibleMoves)

      val remaining = savedMovesOpt.map(_.drop(visibleMoves.length)).getOrElse(Nil)
      session.pendingReplay = remaining

      val nextColor =
        if visibleMoves.length % 2 == 0 then session.startingColor
        else if session.startingColor == "white" then "black" else "white"
      session.pendingReplayColor = nextColor

      liveVariationRepository.updateDraftMoves(session.moves.toList)
      liveVariationRepository.persistDraftSnapshot()
    }

  private def variationsForAnchor(
    anchorIndex: Int,
    parentCtx: Option[(VariationId, Int)] = None
  ): Vector[LiveVariation] =
    val effectiveCtx = parentCtx.orElse(parentContextForAnchor(anchorIndex))
    liveVariationRepository.getVariationsIncludingDraft
      .filter(v => v.anchorIndex == anchorIndex && variationMatchesContext(v, effectiveCtx))
      .toVector

  private def variationDepth(variation: LiveVariation): Int = {
    val byId = liveVariationRepository.getVariations.map(v => v.id -> v).toMap

    @tailrec
    def loop(current: LiveVariation, depth: Int): Int =
      current.parentId match
        case Some(pid) =>
          byId.get(pid) match
            case Some(parent) => loop(parent, depth + 1)
            case None         => depth + 1
        case None => depth

    loop(variation, 0)
  }

  private def variationDisplayPrefix(variation: LiveVariation): String =
    val depth = variationDepth(variation)
    if depth <= 0 then ""
    else "  " * (depth - 1) + "↳ "

  private val VariationPickerKey = "variation-picker"
  private val BranchSelectionKey = "branch-selection"

  private def showVariationPicker(moveIndex: Int): Unit = {
    val anchorsToCheck =
      if moveIndex == 0 then Set(moveIndex, -1)
      else Set(moveIndex)

    val draftSessionOpt = activeVariationSession.collect {
      case session if anchorsToCheck.contains(session.anchorIndex) && session.moves.nonEmpty => session
    }

    val variationsByAnchor: Seq[(Int, Vector[LiveVariation])] =
      val ordered = LinkedHashMap.empty[Int, Vector[LiveVariation]]

      anchorsToCheck.toSeq.sorted.foreach { anchor =>
        val context = if anchor == moveIndex then parentContextForAnchor(anchor) else None
        val contextVariations = variationsForAnchor(anchor, context)
        val mainlineVariations =
          if context.nonEmpty then variationsForAnchor(anchor, None)
          else Vector.empty

        val combined = (mainlineVariations ++ contextVariations)
          .filterNot { variation =>
            draftSessionOpt.exists { session =>
              variation.anchorIndex == session.anchorIndex &&
              variation.startingColor == session.startingColor &&
              variation.startingMoveNumber == session.startingMoveNumber &&
              variation.moves == session.moves.toList
            }
          }

        val anchorVariations = combined.distinctBy(_.id)
        if anchorVariations.nonEmpty then
          val existing = ordered.getOrElse(anchor, Vector.empty)
          ordered.update(anchor, existing ++ anchorVariations)
      }

      ordered.toSeq

    if variationsByAnchor.isEmpty && draftSessionOpt.isEmpty then
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

      draftSessionOpt.foreach { session =>
        val prefix =
          if session.startingColor == "white" then s"${session.startingMoveNumber}."
          else s"${session.startingMoveNumber}..."
        val line = session.moves.mkString(" ")

        val draftBox = new VBox {
          spacing = 6
          style = "-fx-background-color: rgba(255,255,255,0.08); -fx-padding: 10; -fx-background-radius: 6;"
          children = Seq(
            new Label(s"Draft variation: $prefix $line") {
              style = "-fx-text-fill: #ffe082; -fx-font-weight: bold;"
              wrapText = true
            },
            new HBox {
              spacing = 8
              children = Seq(
                new Button("Save draft") {
                  styleClass += "notification-inline-action"
                  onAction = _ =>
                    finishActiveVariation(save = true)
                    notificationCenter.clear(VariationPickerKey)
                },
                new Button("Discard") {
                  styleClass += "notification-inline-action"
                  onAction = _ =>
                    finishActiveVariation(save = false)
                    notificationCenter.clear(VariationPickerKey)
                }
              )
            }
          )
        }

        content.children.add(draftBox)
      }

      if variationsByAnchor.nonEmpty then
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

          anchorVariations.zipWithIndex.foreach { (variation, idx) =>
          val preview = variation.preview(8)
          val prefix = variationDisplayPrefix(variation)
          val isSelected = variationSelectionState.exists(state =>
            state.anchorIndex == anchor && state.selectedIndex == idx
          )

          val headerLabel = new Label(s"$prefix$preview") {
            wrapText = true
            style =
              if isSelected then "-fx-text-fill: #ffffff; -fx-font-weight: bold;"
              else "-fx-text-fill: #e0e0e0;"
          }

          val commentLabelOpt = variation.comment.filter(_.nonEmpty).map { comment =>
            new Label(comment) {
              wrapText = true
              style = "-fx-text-fill: #b0bec5; -fx-font-style: italic;"
            }
          }

          val buttonRow = new HBox {
            spacing = 8
            children = Seq(
              new Button(if isSelected then "Highlighted" else "Highlight") {
                disable = isSelected
                styleClass += "notification-inline-action"
                onAction = _ =>
                  variationSelectionState = Some(VariationSelectionState(anchor, anchorVariations, idx))
                  updateVariationStatusLabel()
                  updateMoveArrows()
              },
              new Button("Resume editing") {
                styleClass += "notification-inline-action"
                onAction = _ =>
                  notificationCenter.clear(VariationPickerKey)
                  resumeSavedVariation(variation, autoStep = true)
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

  private def branchSelectionTitle(anchorIndex: Int): String =
    if anchorIndex < 0 then "Choose continuation from the starting position"
    else
      val moveNo = startingMoveNumberFromAnchor(anchorIndex)
      val colourText = startingColorFromAnchor(anchorIndex).capitalize
      s"$colourText to move after move $moveNo"

  private def updateBranchSelectionOverlay(anchorIndex: Int, variations: Vector[LiveVariation]): Unit = {
    val viewingAnchor = isViewingHistory && currentMoveIndex == anchorIndex
    if !viewingAnchor || variations.isEmpty then
      notificationCenter.clear(BranchSelectionKey)
      return

    val selectionIndex = variationSelectionState
      .filter(_.anchorIndex == anchorIndex)
      .map(_.selectedIndex)
      .getOrElse(-1)

    val futureNotations = controller.moveHistoryManager.getMoveNotations
    val dropCount = math.max(anchorIndex + 1, 0)
    val futureMoves =
      if dropCount < futureNotations.length then futureNotations.drop(dropCount) else Nil

    val colourToMove = controller.currentPlayer.capitalize

    val mainlinePreview = futureMoves.headOption match
      case Some(move) => s"$colourToMove plays $move"
      case None       => "No recorded mainline continuation from this position."

    def entryStyle(selected: Boolean, accent: String): String =
      if selected then s"-fx-background-color: ${accent}; -fx-padding: 10; -fx-background-radius: 6;"
      else "-fx-background-color: rgba(255,255,255,0.05); -fx-padding: 10; -fx-background-radius: 6;"

    val content = new VBox {
      spacing = 8
    }

    content.children.add(new VBox {
      spacing = 4
      style = entryStyle(selectionIndex == -1, "rgba(255,202,40,0.18)")
      children = Seq(
        new Label("Mainline continuation") {
          wrapText = true
          style =
            if selectionIndex == -1 then "-fx-text-fill: #ffca28; -fx-font-weight: bold;"
            else "-fx-text-fill: #e0e0e0; -fx-font-weight: bold;"
        },
        new Label(mainlinePreview) {
          wrapText = true
          style = "-fx-text-fill: #cfd8dc;"
        }
      )
    })

    variations.zipWithIndex.foreach { case (variation, idx) =>
      val nextMoveText = variation.moves.headOption match
        case Some(move) => s"${variation.startingColor.capitalize} plays $move"
        case None       => "Variation has no recorded moves yet."
      val commentLabelOpt = variation.comment.filter(_.nonEmpty).map { comment =>
        new Label(comment) {
          wrapText = true
          style = "-fx-text-fill: #b0bec5; -fx-font-style: italic;"
        }
      }

      val prefix = variationDisplayPrefix(variation)

      val titleLabel = new Label(s"Variation ${idx + 1}") {
        wrapText = true
        style =
          if selectionIndex == idx then "-fx-text-fill: #64b5f6; -fx-font-weight: bold;"
          else "-fx-text-fill: #e0e0e0; -fx-font-weight: bold;"
      }

      val previewLabel = new Label(s"$prefix$nextMoveText") {
        wrapText = true
        style =
          if selectionIndex == idx then "-fx-text-fill: #ffffff;"
          else "-fx-text-fill: #cfd8dc;"
      }

      val childrenNodes = commentLabelOpt match
        case Some(commentLabel) => Seq(titleLabel, previewLabel, commentLabel)
        case None => Seq(titleLabel, previewLabel)

      content.children.add(new VBox {
        spacing = 4
        style = entryStyle(selectionIndex == idx, "rgba(100,181,246,0.2)")
        children = childrenNodes
      })
    }

    val instructions =
      "Use ↑/↓ to choose a continuation and press → to follow it."

    notificationCenter.showCustom(
      key = BranchSelectionKey,
      level = notificationCenter.InfoLevel,
      title = branchSelectionTitle(anchorIndex),
      body = Some(instructions),
      content = content,
      actions = Nil,
      dismissible = true
    )
  }

  private def startVariationSession(
    anchorIndex: Int,
    resumedVariation: Option[LiveVariation] = None,
    parentContext: Option[(VariationId, Int)] = None,
    baseOverride: Option[Int] = None,
    startingContextOverride: Option[(Int, String)] = None,
    futureOverride: Option[List[String]] = None
  ): ActiveVariationSession = {
    traceVariation("startSession.request") {
      val history = controller.moveHistoryManager.getMoveNotations
      val resumedDesc = resumedVariation.map(v => s"id=${v.id}").getOrElse("<none>")
      val parentDesc = parentContext.map { case (pid, ply) => s"parent=$pid ply=$ply" }.getOrElse("<none>")
      val baseDesc = baseOverride.map(_.toString).getOrElse("<auto>")
      val startDesc = startingContextOverride.map { case (move, color) => s"override=${move}.${color}" }.getOrElse("<auto>")
      val futureDesc = futureOverride.map(formatSanList).getOrElse("<auto>")
      s"anchor=$anchorIndex resumed=$resumedDesc parent=$parentDesc baseOverride=$baseDesc startContext=$startDesc futureOverride=$futureDesc history=${formatSanList(history)}"
    }
    val baseNotationSize = baseOverride.getOrElse(math.max(anchorIndex + 1, 0))
    val historyTail = controller.moveHistoryManager.getMoveNotations.drop(baseNotationSize)
    val draftParentContext = resumedVariation.flatMap(variationParentContext).orElse(parentContext)
    val futureNotations = futureOverride.getOrElse(historyTail)

    val startingContext =
      startingContextOverride
        .orElse(resumedVariation.map(variation => variation.startingMoveNumber -> variation.startingColor))
        .orElse(draftParentContext.flatMap { case (parentId, ply) =>
          liveVariationRepository.getVariations.find(_.id == parentId).map { parentVariation =>
            moveNumberAndColorAfterPly(parentVariation.startingMoveNumber, parentVariation.startingColor, ply)
          }
        })
        .getOrElse(startingMoveNumberFromAnchor(anchorIndex) -> startingColorFromAnchor(anchorIndex))

    val (startingMoveNumber, startingColor) = startingContext

    val session = new ActiveVariationSession(
      anchorIndex = anchorIndex,
      baseNotationSize = baseNotationSize,
      startingMoveNumber = startingMoveNumber,
      startingColor = startingColor,
      moves = ListBuffer.empty[String],
      originalFutureNotations = futureNotations,
      initialParentContext = draftParentContext
    )

    resumedVariation.foreach(session.setResumedVariation)

    controller.moveHistoryManager.trimToSize(baseNotationSize)
    restoreBoardFromHistory()

    activeVariationSession = Some(session)
    liveVariationRepository.discardDraft()
    liveVariationRepository.beginDraft(
      anchorIndex = session.anchorIndex,
      baseNotationSize = session.baseNotationSize,
      startingMoveNumber = session.startingMoveNumber,
      startingColor = session.startingColor,
      originalFutureMoves = session.originalFutureNotations,
      resumedFrom = resumedVariation,
      parentContext = draftParentContext
    )
    traceVariation("startSession.state") {
      val historyAfter = controller.moveHistoryManager.getMoveNotations
      s"session=${describeSession(session)} history=${formatSanList(historyAfter)}"
    }
    currentMoveIndex = -1
    isViewingHistory = false
    session.pendingReplay = Nil
    session.pendingReplayColor = session.startingColor
    updateVariationMenuState()
    if variationSelectionState.exists(_.anchorIndex == anchorIndex) then
      clearVariationSelectionState()

    val highlight =
      if baseNotationSize > 0 then Some(baseNotationSize - 1)
      else None
    updateMoveHistoryView(highlight)
    updateMoveArrows()
    session
  }

  private def captureVariationMove(): Unit = {
    activeVariationSession.foreach { session =>
      val notations = controller.moveHistoryManager.getMoveNotations
      traceVariation("captureVariationMove.check") {
        s"session=${describeSession(session)} history=${formatSanList(notations)}"
      }
      if notations.length > session.baseNotationSize then
        val newMoves = notations.drop(session.baseNotationSize)
        session.moves.clear()
        session.moves.appendAll(newMoves)
        updateVariationMenuState()
        updateMoveArrows()
        updateMoveHistoryView(highlightIndex = None)
        liveVariationRepository.updateDraftMoves(session.moves.toList)
        liveVariationRepository.persistDraftSnapshot()
        ensureSessionVariationLinked(session)
        traceVariation("captureVariationMove.applied") {
          s"session=${describeSession(session)}"
        }
    }
  }

  private def playNextPendingVariationMove(session: ActiveVariationSession): Boolean = {
    if session.pendingReplay.isEmpty then return false

    val san = session.pendingReplay.head
    val remaining = session.pendingReplay.tail
    val isWhiteMove = session.pendingReplayColor == "white"

    val beforeCount = controller.moveHistoryManager.getMoveHistory.length
    applySanMove(san, isWhiteMove)
    val afterCount = controller.moveHistoryManager.getMoveHistory.length
    val success = afterCount > beforeCount

    if success then
      session.pendingReplay = remaining
      session.pendingReplayColor = if isWhiteMove then "black" else "white"
      captureVariationMove()
      restoreBoardFromHistory()
      currentMoveIndex = -1
      isViewingHistory = false
      true
    else
      false
  }

  private def clearMoveSelection(): Unit = {
    selectedSquare = None
    boardView.clearSelectedSquare()
    boardView.clearLegalMoveTargets()
  }

  private def autoCloseActiveVariation(saveIfMoves: Boolean = true): Boolean =
    activeVariationSession match
      case Some(session) =>
        val shouldSave = saveIfMoves && session.moves.nonEmpty
        finishActiveVariation(save = shouldSave)
        true
      case None => false

  private def finishActiveVariation(save: Boolean): Unit = {
    activeVariationSession.foreach { session =>
      traceVariation("finishActiveVariation.begin") {
        val historyBefore = controller.moveHistoryManager.getMoveNotations
        s"save=$save session=${describeSession(session)} history=${formatSanList(historyBefore)}"
      }
      restoreMainlineAfterVariation(session)
      traceVariation("finishActiveVariation.afterRestore") {
        val historyAfterRestore = controller.moveHistoryManager.getMoveNotations
        s"session=${describeSession(session)} history=${formatSanList(historyAfterRestore)}"
      }

      if save && session.moves.nonEmpty then
        traceVariation("finishActiveVariation.commit") {
          s"session=${describeSession(session)}"
        }
        liveVariationRepository.commitDraft()
      else
        traceVariation("finishActiveVariation.discard") {
          s"session=${describeSession(session)}"
        }
        liveVariationRepository.discardDraftAndRevert()

      activeVariationSession = None
      updateVariationMenuState()
      currentMoveIndex = -1
      isViewingHistory = false
      updateMoveHistoryView(highlightIndex = None)
      updateMoveArrows()
      traceVariation("finishActiveVariation.end") {
        val historyFinal = controller.moveHistoryManager.getMoveNotations
        s"history=${formatSanList(historyFinal)}"
      }
    }
  }

  private def restoreMainlineAfterVariation(session: ActiveVariationSession): Unit = {
    traceVariation("restoreMainline.start") {
      val historyBefore = controller.moveHistoryManager.getMoveNotations
      s"session=${describeSession(session)} history=${formatSanList(historyBefore)}"
    }
    val restoreBaseSize = session.baseNotationSize

    controller.moveHistoryManager.trimToSize(restoreBaseSize)
    restoreBoardFromHistory()
    traceVariation("restoreMainline.afterTrim") {
      val historyAfterTrim = controller.moveHistoryManager.getMoveNotations
      s"session=${describeSession(session)} history=${formatSanList(historyAfterTrim)}"
    }

    if session.originalFutureNotations.nonEmpty then
      traceVariation("restoreMainline.replay") {
        s"future=${formatSanList(session.originalFutureNotations)}"
      }
      var colorToMove = controller.currentPlayer
      session.originalFutureNotations.foreach { notation =>
        val isWhiteMove = colorToMove == "white"
        applySanMove(notation, isWhiteMove = isWhiteMove)
        colorToMove = if isWhiteMove then "black" else "white"
      }
      restoreBoardFromHistory()
      traceVariation("restoreMainline.afterReplay") {
        val historyAfterReplay = controller.moveHistoryManager.getMoveNotations
        s"history=${formatSanList(historyAfterReplay)}"
      }
    else
      traceVariation("restoreMainline.noReplay")("no future notations to restore")
    traceVariation("restoreMainline.end") {
      val historyFinal = controller.moveHistoryManager.getMoveNotations
      s"history=${formatSanList(historyFinal)}"
    }
  }

  private def restoreBoardFromHistory(): Unit = {
    val history = controller.moveHistoryManager.getMoveHistory

    boardModel.resetBoard()
    controller.currentPlayer = "white"
    controller.board.foreach(_.deselectAll())

    history.foreach { move =>
      controller.replayMove(move)
    }

    controller.currentPlayer = if history.length % 2 == 0 then "white" else "black"

    if history.nonEmpty then
      val last = history.last
      val fromCoords = last.from.getCoordinates
      val toCoords = last.to.getCoordinates
      boardView.setLastMove(fromCoords._2, fromCoords._1, toCoords._2, toCoords._1)
    else
      boardView.clearLastMove()

    boardView.refreshFromModel()
    boardView.clearSelectedSquare()
    boardView.clearLegalMoveTargets()
    selectedSquare = None
    syncActiveVariationWithHistory()
    triggerAnalysisIfActive()
    updateMoveArrows()
  }

  override def start(): Unit = {
    controller.onPawnPromotion = Some(handlePawnPromotion)

    stage = new PrimaryStage {
      title = "Scala Chess - Modern Edition"
      width = 1200
      height = 800
      scene = createMainScene()
    }

    stage.onShown = _ =>
      Platform.runLater { () =>
        boardView.refreshFromModel()
        updateMoveArrows()
      }

    // Attach keyboard navigation at the stage level to ensure key events are
    // received regardless of which child currently has focus.
    stage.scene().onKeyPressed = (event: KeyEvent) => {
      event.code match
        case KeyCode.Left =>
          navigateBackward()
          event.consume()
        case KeyCode.Right =>
          val handled =
            resumeSelectedVariation() ||
              autoCloseActiveVariation(saveIfMoves = true)
          if !handled then
            navigateForward()
          event.consume()
        case KeyCode.Up =>
          if cycleVariationSelection(-1) then event.consume()
        case KeyCode.Down =>
          if cycleVariationSelection(1) then event.consume()
        case _ =>
    }
  }

  private def choosePromotionPiece(): PieceType = {
    val dialog = new ChoiceDialog[String](
      "Queen",
      Seq("Queen", "Rook", "Bishop", "Knight")
    )
    Option(stage).foreach(dialog.initOwner)
    dialog.title = "Pawn Promotion"
    dialog.headerText = "Pick a piece to promote to"
    dialog.contentText = "Promote pawn to:"
    val selection = dialog.showAndWait().map(_.toLowerCase).getOrElse("queen")
    selection match
      case "rook"   => PieceType.Rook
      case "bishop" => PieceType.Bishop
      case "knight" => PieceType.Knight
      case _         => PieceType.Queen
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

  private val BranchDecisionKey = "branch-decision"

  private def requestBranchDecision(onDecision: BranchDecision => Unit): Unit =
    activeVariationSession match
      case Some(session) =>
        val attemptingBeforeAnchor =
          isViewingHistory && currentMoveIndex >= 0 && currentMoveIndex < session.anchorIndex

        if attemptingBeforeAnchor then
          notificationCenter.showWarning(
            key = "variation-branch",
            title = "Variation in progress",
            message = "Finish or cancel the active variation before starting a new branch."
          )
          onDecision(BranchCanceled)
        else
          val variationStartIndex = session.baseNotationSize
          val variationEndExclusive = session.baseNotationSize + session.moves.length
          val editingExistingVariation =
            isViewingHistory &&
              currentMoveIndex >= variationStartIndex &&
              currentMoveIndex < variationEndExclusive
          val parentCtxAtCurrent =
            if editingExistingVariation then
              sessionBranchParentContext(session, Some(currentMoveIndex))
            else sessionBranchParentContext(session, None)

          if editingExistingVariation then
            showBranchDecisionPrompt(currentMoveIndex, {
              case BranchVariation(anchor, ctxFromPrompt) =>
                val effectiveCtx = ctxFromPrompt.orElse(parentCtxAtCurrent)
                onDecision(BranchVariation(anchor, effectiveCtx))
              case decision =>
                onDecision(decision)
            })
          else
            onDecision(BranchVariation(session.anchorIndex, parentCtxAtCurrent))
      case None =>
        val historySize = controller.moveHistoryManager.getMoveHistory.size
        val atLiveEdge =
          isViewingHistory && historySize > 0 && currentMoveIndex >= historySize - 1

        if !isViewingHistory || currentMoveIndex == -1 || atLiveEdge then onDecision(BranchNormal)
        else showBranchDecisionPrompt(currentMoveIndex, onDecision)

  private def finishPendingBranch(decision: BranchDecision, beforeInvoke: () => Unit = () => ()): Unit = {
    val callbackOpt = pendingBranchCallback
    pendingBranchCallback = None
    notificationCenter.clear(BranchDecisionKey)
    beforeInvoke()
    callbackOpt.foreach(_.apply(decision))
  }

  private def showBranchDecisionPrompt(anchorIndex: Int, onDecision: BranchDecision => Unit): Unit = {
    val moveNumber = ((anchorIndex + 1) / 2) + 1
    val colourText = if ((anchorIndex + 1) % 2 == 0) "White" else "Black"

    val retainCount = math.max(anchorIndex + 1, 0)
    val futureMoves = controller.moveHistoryManager.getMoveNotations.drop(retainCount)

    val futurePreview =
      if futureMoves.isEmpty then "There are no future moves to overwrite."
      else {
        val preview = futureMoves.take(6).mkString(" ")
        val suffix = if futureMoves.length > 6 then " ..." else ""
        s"Current continuation: $preview$suffix"
      }

    val variationsAtAnchor = variationsForAnchor(anchorIndex, None)

    pendingBranchCallback.foreach(_ => notificationCenter.clear(BranchDecisionKey))
    pendingBranchCallback = Some(onDecision)

    val variationList = new VBox {
      spacing = 6
    }

    if variationsAtAnchor.nonEmpty then
      variationList.children.add(new Label("Saved variations:") {
        style = "-fx-text-fill: #f5f5f5; -fx-font-weight: bold;"
      })

      variationsAtAnchor.zipWithIndex.foreach { (variation, idx) =>
        val preview = variation.preview(6)
        val prefix = variationDisplayPrefix(variation)
        val isSelected = variationSelectionState.exists(state =>
          state.anchorIndex == anchorIndex && state.selectedIndex == idx
        )

        val previewLabel = new Label(s"$prefix$preview") {
          wrapText = true
          style = if isSelected then "-fx-text-fill: #ffffff; -fx-font-weight: bold;"
          else "-fx-text-fill: #e0e0e0;"
        }

        val resumeButton = new Button("Resume editing") {
          styleClass += "notification-inline-action"
          onAction = _ =>
            finishPendingBranch(
              BranchVariation(anchorIndex, None),
              beforeInvoke = () => {
                variationSelectionState = Some(VariationSelectionState(anchorIndex, variationsAtAnchor, idx))
                updateVariationStatusLabel()
                updateMoveArrows()
              }
            )
        }

        val highlightButton = new Button("Highlight") {
          styleClass += "notification-inline-action"
          onAction = _ =>
            variationSelectionState = Some(VariationSelectionState(anchorIndex, variationsAtAnchor, idx))
            updateVariationStatusLabel()
            updateMoveArrows()
        }

        variationList.children.add(new HBox {
          spacing = 10
          alignment = Pos.CenterLeft
          children = Seq(previewLabel, new HBox {
            spacing = 6
            children = Seq(highlightButton, resumeButton)
          })
        })
      }
    else
      variationList.children.add(new Label("No saved variations yet for this position.") {
        style = "-fx-text-fill: #cccccc;"
      })

    val actions = Seq(
      notificationCenter.NotificationAction(
        label = "Create variation",
        onTrigger = () => finishPendingBranch(BranchVariation(anchorIndex, None), () => clearVariationSelectionState()),
        styleClass = "primary"
      ),
      notificationCenter.NotificationAction(
        label = "Overwrite future moves",
        onTrigger = () => finishPendingBranch(BranchOverwrite(anchorIndex)),
        styleClass = "danger"
      ),
      notificationCenter.NotificationAction(
        label = "Cancel",
        onTrigger = () => finishPendingBranch(BranchCanceled),
        styleClass = "secondary"
      )
    )

    notificationCenter.showCustom(
      key = BranchDecisionKey,
      level = notificationCenter.WarningLevel,
      title = s"$colourText to move after move $moveNumber",
      body = Some(futurePreview),
      content = variationList,
      actions = actions,
      dismissible = false
    )
  }

  private def prepareOverwrite(anchorIndex: Int): Unit = {
    val retainCount = math.max(anchorIndex + 1, 0)
    controller.moveHistoryManager.trimToSize(retainCount)
    currentMoveIndex = anchorIndex
    isViewingHistory = anchorIndex >= 0

    val highlight =
      if retainCount > 0 then Some(retainCount - 1)
      else None
    updateMoveHistoryView(highlight)
  }

  private def prepareVariationContinuation(session: ActiveVariationSession): Unit = {
    if isViewingHistory then
      val targetSize = math.max(session.baseNotationSize, currentMoveIndex + 1)
      controller.moveHistoryManager.trimToSize(targetSize)
      restoreBoardFromHistory()
      currentMoveIndex = -1
      isViewingHistory = false
      updateMoveHistoryView(highlightIndex = None)

      val trimmedMoves = controller.moveHistoryManager.getMoveNotations.drop(session.baseNotationSize)
      session.moves.clear()
      session.moves.appendAll(trimmedMoves)
      updateMoveArrows()
    liveVariationRepository.updateDraftMoves(session.moves.toList)
    liveVariationRepository.persistDraftSnapshot()
    ensureSessionVariationLinked(session)
  }

  private def handlePawnPromotion(
    pawn: PawnPiece,
    @unused from: Square,
    to: Square,
    capturedPiece: Option[Piece]
  ): PieceType = {
    val promotedType = choosePromotionPiece()
    applyPromotion(pawn, to, capturedPiece, promotedType)
  }

  private def createMainScene(): Scene = {
    val mainLayout = new BorderPane {
      style = "-fx-background-color: #2b2b2b;"
    }

    // Top menu bar (simplified for now)
    val menuBar = createMenuBar()
    mainLayout.top = menuBar

    // Center content
    centerContent = new HBox {
      spacing = 10
      padding = Insets(10)
    }

    // Center - Chess board (ScalaFX view backed by existing Board model)
    boardView.setModel(boardModel)
    boardView.onSquareClick = Some { (row, col) =>
      val square = boardModel.squares(row)(col)

      selectedSquare match {
        case None =>
          // First click: select a piece of the current player
          square.occupiedBy match {
            case Some(piece) if piece.color == controller.currentPlayer =>
              selectedSquare = Some(square)
              boardView.setSelectedSquare(row, col)
              val legalDestinations = controller.filterMovesToPreventCheck(piece, piece.possibleMoves())
              val moveCoords = legalDestinations.map { sq =>
                val (c, r) = sq.getCoordinates
                (r, c)
              }
              boardView.setLegalMoveTargets(moveCoords)
            case _ =>
              // Clicked empty square or opponent piece without selection – ignore for now
              boardView.clearLegalMoveTargets()
          }

        case Some(fromSq) =>
          // Second click: attempt to move selected piece to target square
          fromSq.occupiedBy match {
            case Some(piece) =>
              def performMove(): Boolean = {
                val moved = controller.handleAction(piece, square)
                if moved then
                  val fromCoords = fromSq.getCoordinates
                  val toCoords = square.getCoordinates
                  boardView.setLastMove(fromCoords._2, fromCoords._1, toCoords._2, toCoords._1)
                  boardView.refreshFromModel()
                  boardView.clearLegalMoveTargets()
                  updateMoveArrows()
                else
                  // Intentionally silent during normal play; consider enabling detailed logging when debugging.
                  boardView.clearLegalMoveTargets()
                moved
              }

              requestBranchDecision {
                case BranchCanceled =>
                  clearMoveSelection()
                case BranchOverwrite(anchor) =>
                  prepareOverwrite(anchor)
                  if performMove() then
                    currentMoveIndex = -1
                    isViewingHistory = false
                    updateMoveHistoryView(highlightIndex = None)
                    triggerAnalysisIfActive()
                  clearMoveSelection()
                case BranchVariation(anchor, parentCtx) =>
                  val selectedVariation =
                    variationSelectionState
                      .filter(_.anchorIndex == anchor)
                      .flatMap(state => state.variations.lift(state.selectedIndex))
                  val resumeContext = selectedVariation.flatMap(variationParentContext).orElse(parentCtx)
                  val currentNotations = controller.moveHistoryManager.getMoveNotations
                  val proposedBaseSize = math.max(anchor + 1, 0)
                  val clampedBaseSize = math.max(0, math.min(proposedBaseSize, currentNotations.length))
                  val futureAtAnchor =
                    if clampedBaseSize <= currentNotations.length then currentNotations.drop(clampedBaseSize)
                    else Nil

                  val activeSessionOpt = activeVariationSession.filter(_.anchorIndex == anchor)

                  def activeSessionVariationId(session: ActiveVariationSession): Option[VariationId] =
                    session.resumedFrom.map(_.id).orElse(locateSessionVariation(session).map(_.id))

                  def continueExistingSession(session: ActiveVariationSession, variationOverride: Option[LiveVariation]): Unit =
                    variationOverride.foreach(session.setResumedVariation)
                    if variationOverride.isDefined then
                      liveVariationRepository.updateDraftResumedFrom(variationOverride)
                    prepareVariationContinuation(session)

                  def startingContextFor(parentId: VariationId, parentPly: Int): Option[(Int, String)] =
                    liveVariationRepository.getVariations.find(_.id == parentId).map { parentVariation =>
                      moveNumberAndColorAfterPly(parentVariation.startingMoveNumber, parentVariation.startingColor, parentPly)
                    }

                  parentCtx match
                    case Some((parentId, parentPly)) =>
                      activeSessionOpt match
                        case Some(existing) =>
                          val sessionVariationId = activeSessionVariationId(existing)
                          val selectedMatchesActive = selectedVariation.exists(_.id == parentId)
                          val continuingCurrent =
                            sessionVariationId.contains(parentId) && parentPly == existing.moves.length && (selectedVariation.isEmpty || selectedMatchesActive)

                          if continuingCurrent then
                            continueExistingSession(existing, selectedVariation.filter(_.id == parentId))
                          else if selectedVariation.isEmpty then
                            val startingOverride = startingContextFor(parentId, parentPly)

                            val newSession = startVariationSession(
                              anchorIndex = anchor,
                              resumedVariation = None,
                              parentContext = Some(parentId -> parentPly),
                              baseOverride = Some(clampedBaseSize),
                              startingContextOverride = startingOverride,
                              futureOverride = Some(existing.originalFutureNotations)
                            )
                            prepareVariationContinuation(newSession)
                          else
                            val newSession = startVariationSession(
                              anchorIndex = anchor,
                              resumedVariation = selectedVariation,
                              parentContext = resumeContext
                            )
                            liveVariationRepository.updateDraftResumedFrom(selectedVariation)
                            prepareVariationContinuation(newSession)
                        case None =>
                          val startingOverride = startingContextFor(parentId, parentPly)
                          val newSession = startVariationSession(
                            anchorIndex = anchor,
                            resumedVariation = selectedVariation,
                            parentContext = Some(parentId -> parentPly),
                            baseOverride = Some(clampedBaseSize),
                            startingContextOverride = startingOverride,
                            futureOverride = Some(futureAtAnchor)
                          )
                          if selectedVariation.isDefined then
                            liveVariationRepository.updateDraftResumedFrom(selectedVariation)
                          prepareVariationContinuation(newSession)
                    case None =>
                      activeSessionOpt match
                        case Some(existing) =>
                          continueExistingSession(existing, selectedVariation)
                        case None =>
                          val newSession = startVariationSession(
                            anchorIndex = anchor,
                            resumedVariation = selectedVariation,
                            parentContext = resumeContext
                          )
                          if selectedVariation.isDefined then
                            liveVariationRepository.updateDraftResumedFrom(selectedVariation)
                          prepareVariationContinuation(newSession)

                  if performMove() then
                    captureVariationMove()
                    triggerAnalysisIfActive()
                  clearMoveSelection()
                case BranchNormal =>
                  if performMove() then
                    currentMoveIndex = -1
                    isViewingHistory = false
                    updateMoveHistoryView(highlightIndex = None)
                    triggerAnalysisIfActive()
                  clearMoveSelection()
              }
            case None =>
              // Previously selected square no longer has a piece (shouldn't normally happen)
          }
          // Selection cleared within branch decision callbacks
      }
    }

    // Wrap board in a bordered grid with coordinate labels (A-H, 1-8)
    val labeledBoard = createLabeledBoard()

    // Right panel - Modern move history and analysis
    analysisPanelContainer = createAnalysisPanel()

    centerContent.children = Seq(analysisManager.evalBar, labeledBoard, analysisPanelContainer)
    mainLayout.center = centerContent

    val scene = new Scene(mainLayout)

    // Ensure root has focus initially (helps on some platforms)
    mainLayout.requestFocus()

    scene
  }

  /**
   * Create a BorderPane that wraps the board with simple coordinate gutters.
   *
   * Layout rules:
   *   - Borders (top/bottom/left/right) have a fixed pixel thickness so they
   *     feel consistent regardless of board size.
   *   - There is exactly one cell per file/rank, and each cell is as wide or
   *     tall as a board square. This keeps letters/numbers aligned with the
   *     square centers.
   *   - Labels are centered within their cells; small visual tweaks are
   *     controlled via borderThickness, font size and outer padding only.
   */
  private def createLabeledBoard(): BorderPane = {
    val files = Seq("A","B","C","D","E","F","G","H")
    val ranks = Seq("8","7","6","5","4","3","2","1")

    def fileLabel(text: String): Label = new Label(text) {
      DebugOnce.mark(261, "[261]")
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

    val topRow = new HBox {
      alignment = Pos.Center
      spacing = 0
      children = files.map(fileLabel)
    }

    val bottomRow = new HBox {
      alignment = Pos.Center
      spacing = 0
      children = files.map(fileLabel)
    }

    val leftCol = new VBox {
      alignment = Pos.Center
      spacing = 0
      children = ranks.map(r => rankCell(r, Pos.CenterLeft, Insets(0, 0, 0, 4)))
    }

    val rightCol = new VBox {
      alignment = Pos.Center
      spacing = 0
      children = ranks.map(r => rankCell(r, Pos.CenterRight, Insets(0, 4, 0, 0)))
    }

    // Fixed border thickness in pixels so the frame feels consistent.
    val borderThickness = 24.0

    // Keep coordinates aligned with squares by tying each cell to the
    // current board cell size.
    def syncWithCellSize(): Unit =
      val s = math.max(boardView.cellSize, 0.0)
      if s <= 0 then return

      // Top/bottom thickness is constant; each label spans one file.
      Seq(topRow, bottomRow).foreach { row =>
        row.minHeight = borderThickness
        row.prefHeight = borderThickness
        row.maxHeight = borderThickness
        row.children.foreach {
          case l: JfxLabel =>
            l.setPrefWidth(s)
            l.setPrefHeight(borderThickness)
            l.setMinHeight(borderThickness)
            l.setMaxHeight(borderThickness)
          case region: Region =>
            region.setPrefWidth(s)
            region.setMinWidth(s)
            region.setMaxWidth(s)
            region.setPrefHeight(borderThickness)
            region.setMinHeight(borderThickness)
            region.setMaxHeight(borderThickness)
          case _ =>
        }
      }

      // Left/right thickness is constant; each rank cell spans one rank.
      Seq(leftCol, rightCol).foreach { col =>
        col.minWidth = borderThickness
        col.prefWidth = borderThickness
        col.maxWidth = borderThickness
        col.children.foreach {
          case region: Region =>
            region.setPrefHeight(s)
            region.setMinHeight(s)
            region.setMaxHeight(s)
            region.setPrefWidth(borderThickness)
            region.setMinWidth(borderThickness)
            region.setMaxWidth(borderThickness)
          case _ =>
        }
      }

    // Recompute geometry when the board resizes and once initially.
    boardView.height.onChange { (_, _, _) => syncWithCellSize() }
    boardView.width.onChange  { (_, _, _) => syncWithCellSize() }
    syncWithCellSize()

    new BorderPane {
      style = "-fx-background-color: #252525; -fx-border-color: #ffffff; -fx-border-width: 1;"
      padding = Insets(6) // uniform outer margin
      center = boardView
      top = topRow
      bottom = bottomRow
      left = leftCol
      right = rightCol
    }
  }

  private def createMenuBar(): MenuBar = {
    val newGameItem = new MenuItem("New Game") {
      onAction = _ => startNewGame()
    }

    val loadPgnItem = new MenuItem("Load PGN...") {
      onAction = _ => loadPGNFromChooser()
    }

    val savePgnItem = new MenuItem("Save PGN...") {
      onAction = _ => savePGNThroughChooser()
    }

    val loadAnalysisItem = new MenuItem("Load Analysis...") {
      onAction = _ => loadAnalysisFromFile()
    }

    val saveAnalysisItem = new MenuItem("Save Analysis...") {
      onAction = _ => saveAnalysisToFile()
    }

    finishVariationMenuItem = new MenuItem("Finish Variation") {
      disable = true
      onAction = _ => finishActiveVariation(save = true)
    }

    cancelVariationMenuItem = new MenuItem("Cancel Variation") {
      disable = true
      onAction = _ => finishActiveVariation(save = false)
    }

    showMoveArrowsMenuItem = new CheckMenuItem("Show Move Arrows") {
      selected = showMoveArrows
      onAction = _ => setMoveArrowsEnabled(selected())
    }

    val menuBar = new MenuBar {
      styleClass.add("main-menu-bar")
      menus = Seq(
        new Menu("File") {
          items = Seq(newGameItem, loadPgnItem, savePgnItem)
        },
        new Menu("Variations") {
          items = Seq(finishVariationMenuItem, cancelVariationMenuItem, new SeparatorMenuItem(), showMoveArrowsMenuItem)
        },
        new Menu("Analysis") {
          items = Seq(analysisManager.toggleMenuItem, new SeparatorMenuItem(), loadAnalysisItem, saveAnalysisItem)
        }
      )
    }

    Option(getClass.getResource("/ui/menu.css"))
      .map(_.toExternalForm)
      .foreach(menuBar.stylesheets += _)

    updateVariationMenuState()

    menuBar
  }

  // NOTE: previous static createChessBoard() removed; board is now rendered
  // via ScalaFXBoardView which reads from the real Board model.

  private def createAnalysisPanel(): VBox =
    new VBox {
      spacing = 10
      prefWidth = 350
      padding = Insets(10)

      children = Seq(
        createMoveHistoryPanel(),
        notificationCenter,
        analysisManager.engineAnalysisPanel
      )
    }

  private def startNewGame(): Unit = {
    controller.resetGame()
    boardModel.resetBoard()
    controller.moveHistoryManager.clearHistory()
    controller.currentPlayer = "white"
    activeVariationSession = None
    liveVariationRepository.clear()
    updateVariationMenuState()
    selectedSquare = None
    boardView.clearSelectedSquare()
    boardView.clearLastMove()
    boardView.refreshFromModel()
    updateMoveHistoryView(None)
    currentMoveIndex = -1
    isViewingHistory = false
    triggerAnalysisIfActive()
    clearVariationSelectionState()
    updateMoveArrows()
  }

  private def loadPGNFromChooser(): Unit = {
    val chooser = new FileChooser {
      title = "Load PGN"
      extensionFilters.add(new FileChooser.ExtensionFilter("PGN Files", "*.pgn"))
    }
    defaultDirectory("Peliarkisto").foreach(dir => chooser.initialDirectory = dir)
    Option(chooser.showOpenDialog(stage)).foreach(loadPGNFile)
  }

  private def loadPGNFile(file: File): Unit = {
    pgnManager.loadPGN(file) match {
      case Right(game) =>
        applyPGNGame(game)
        notificationCenter.showInfo(
          key = "pgn-status",
          title = "PGN loaded",
          message = s"Loaded ${file.getName}"
        )
      case Left(error) =>
        notificationCenter.showError(
          key = "pgn-status",
          title = "PGN load failed",
          message = error
        )
    }
  }

  private def savePGNThroughChooser(): Unit = {
    val chooser = new FileChooser {
      title = "Save PGN"
      extensionFilters.add(new FileChooser.ExtensionFilter("PGN Files", "*.pgn"))
      initialFileName = s"game_${LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss"))}.pgn"
    }
    defaultDirectory("Peliarkisto").foreach(dir => chooser.initialDirectory = dir)

    Option(chooser.showSaveDialog(stage)).foreach { targetFile =>
      val file = ensureExtension(targetFile, ".pgn")
      pgnManager.savePGN(file) match {
        case Right(_) =>
          notificationCenter.showInfo(
            key = "pgn-status",
            title = "PGN saved",
            message = s"Game saved to ${file.getName}"
          )
        case Left(error) =>
          notificationCenter.showError(
            key = "pgn-status",
            title = "PGN save failed",
            message = error
          )
      }
    }
  }

  private def loadAnalysisFromFile(): Unit = {
    val chooser = new FileChooser {
      title = "Load Analysis"
      extensionFilters.add(new FileChooser.ExtensionFilter("Analysis Files", Seq("*.analysis", "*.txt")))
    }
    defaultDirectory("Peliarkisto").foreach(dir => chooser.initialDirectory = dir)

    Option(chooser.showOpenDialog(stage)).foreach { file =>
      analysisManager.loadSnapshotFromFile(file)
    }
  }

  private def saveAnalysisToFile(): Unit = {
    val chooser = new FileChooser {
      title = "Save Analysis"
      extensionFilters.add(new FileChooser.ExtensionFilter("Analysis Files", Seq("*.analysis", "*.txt")))
      initialFileName = s"analysis_${LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss"))}.analysis"
    }
    defaultDirectory("Peliarkisto").foreach(dir => chooser.initialDirectory = dir)

    Option(chooser.showSaveDialog(stage)).foreach { targetFile =>
      val file = ensureExtension(targetFile, ".analysis")
      analysisManager.saveSnapshotToFile(file)
    }
  }

  private def applyPGNGame(game: PGNGame): Unit = {
    controller.resetGame()
    boardModel.resetBoard()
    controller.moveHistoryManager.clearHistory()
    controller.moveHistoryManager.notationManager.clear()
    controller.currentPlayer = "white"
    activeVariationSession = None
    liveVariationRepository.clear()
    updateVariationMenuState()
    controller.updateAllPossibleMoves()

    game.moves.foreach { move =>
      move.whiteMove.map(_.trim).filter(_.nonEmpty).foreach { notation =>
        applySanMove(notation, isWhiteMove = true)
      }
      move.blackMove.map(_.trim).filter(_.nonEmpty).foreach { notation =>
        applySanMove(notation, isWhiteMove = false)
      }
    }

    liveVariationRepository.replaceWithPGNVariations(game)
    updateVariationMenuState()

    controller.currentPlayer = if (controller.moveHistoryManager.getMoveHistory.size % 2 == 0) "white" else "black"
    controller.updateAllPossibleMoves()

    selectedSquare = None
    boardView.clearSelectedSquare()
    boardView.clearLastMove()
    boardView.refreshFromModel()
    updateMoveHistoryView(None)
    currentMoveIndex = -1
    isViewingHistory = false
    triggerAnalysisIfActive()
    clearVariationSelectionState()
    updateMoveArrows()
  }

  private def applySanMove(notation: String, isWhiteMove: Boolean): Unit = {
    parseSanMove(notation, isWhiteMove) match
      case Some(parsed) =>
        val previousPromotionHandler = controller.onPawnPromotion
        var pendingPromotion = parsed.promotion
        controller.onPawnPromotion = Some((_, _, _, _) => pendingPromotion.getOrElse(PieceType.Queen))

        controller.currentPlayer = if isWhiteMove then "white" else "black"

        parsed.from.occupiedBy match
          case Some(piece) =>
            val moved = controller.handleAction(piece, parsed.to)
            if !moved then
              println(s"[PGN] Failed to apply SAN move '$notation' (${if isWhiteMove then "white" else "black"})")
          case None =>
            println(s"[PGN] No piece found on ${parsed.from.name} for SAN move '$notation'")

        pendingPromotion = None
        controller.onPawnPromotion = previousPromotionHandler
      case None =>
        println(s"[PGN] Unable to parse SAN move '$notation'")
  }

  private def parseSanMove(notation: String, isWhiteMove: Boolean): Option[ParsedSanMove] = {
    if notation == null || notation.trim.isEmpty then return None

    val normalized = notation.replace('0', 'O').trim
    val clean = normalized.replaceAll("[+#?!]", "")
    if clean.isEmpty then return None

    if clean == "O-O" || clean == "O-O-O" then
      val fromName = if isWhiteMove then "e1" else "e8"
      val toName =
        if clean == "O-O" then
          if isWhiteMove then "g1" else "g8"
        else
          if isWhiteMove then "c1" else "c8"
      for
        fromSq <- boardModel.getSquareByName(fromName)
        toSq   <- boardModel.getSquareByName(toName)
      yield ParsedSanMove(fromSq, toSq, None)
    else
      val promotionIndex = clean.indexOf('=')
      val (body, promotionType) =
        if promotionIndex >= 0 && promotionIndex < clean.length - 1 then
          val promoChar = clean.charAt(promotionIndex + 1).toUpper
          val promoType = promoChar match
            case 'N' => Some(PieceType.Knight)
            case 'B' => Some(PieceType.Bishop)
            case 'R' => Some(PieceType.Rook)
            case 'Q' => Some(PieceType.Queen)
            case _   => Some(PieceType.Queen)
          (clean.substring(0, promotionIndex), promoType)
        else
          (clean, None)

      if body.length < 2 then return None

      val firstChar = body.head
      val (pieceType, remainder) =
        if "NBRQK".contains(firstChar) then
          val pType = firstChar match
            case 'N' => PieceType.Knight
            case 'B' => PieceType.Bishop
            case 'R' => PieceType.Rook
            case 'Q' => PieceType.Queen
            case 'K' => PieceType.King
          (pType, body.substring(1))
        else
          (PieceType.Pawn, body)

      if remainder.length < 2 then return None

      val destinationStr = remainder.takeRight(2).toLowerCase
      val disambiguation = remainder.dropRight(2).replace("x", "")
      val destSquareOpt = boardModel.getSquareByName(destinationStr)
      if destSquareOpt.isEmpty then return None
      val destSquare = destSquareOpt.get

      val fileHint = disambiguation.find(_.isLetter).map(_.toLower - 'a')
      val rankHint = disambiguation.find(_.isDigit).map(d => 8 - (d.asDigit))
      val color = if isWhiteMove then "white" else "black"

      val candidates = boardModel.squares.iterator.flatMap(_.iterator).flatMap { square =>
        square.occupiedBy match
          case Some(piece) if piece.color == color && piece.pieceType == pieceType && !piece.isCaptured =>
            val (col, row) = square.getCoordinates
            val fileMatches = fileHint.forall(_ == col)
            val rankMatches = rankHint.forall(_ == row)
            if fileMatches && rankMatches then
              val legalMoves = controller.filterMovesToPreventCheck(piece, piece.possibleMoves())
              if legalMoves.contains(destSquare) then Some(square) else None
            else None
          case _ => None
      }.toList

      candidates match
        case head :: Nil => Some(ParsedSanMove(head, destSquare, promotionType))
        case head :: _ =>
          println(s"[PGN] Ambiguous SAN move '$notation', choosing ${head.name}")
          Some(ParsedSanMove(head, destSquare, promotionType))
        case Nil =>
          println(s"[PGN] No legal candidates found for SAN move '$notation'")
          None
  }


  private def defaultDirectory(name: String): Option[File] = {
    val directPath = new File(name)
    if (directPath.exists() && directPath.isDirectory) then Some(directPath)
    else {
      val projectPath = new File(System.getProperty("user.dir"), name)
      if (projectPath.exists() && projectPath.isDirectory) then Some(projectPath) else None
    }
  }

  private def ensureExtension(file: File, extension: String): File = {
    if (file.getName.toLowerCase.endsWith(extension.toLowerCase)) then file
    else
      Option(file.getParentFile)
        .map(dir => new File(dir, file.getName + extension))
        .getOrElse(new File(file.getAbsolutePath + extension))
  }

  private def createMoveHistoryPanel(): VBox = {
    moveHistoryPanel.setOnMoveSelected(handleMoveHistoryClick)
    moveHistoryPanel.setOnVariationSelected(showVariationPicker)
    moveHistoryPanel
  }

  /**
   * Pull notations from MoveHistoryManager and push them to the ScalaFX view.
   * highlightIndex is the zero-based move index to highlight (optional).
   */
  private def updateMoveHistoryView(highlightIndex: Option[Int]): Unit = {
    val notations = controller.moveHistoryManager.getMoveNotations
    val indicators = variationIndicatorsForDisplay()
    val displayState = MoveHistoryDisplayState(notations, highlightIndex, indicators)
    moveHistoryPanel.updateDisplay(displayState)
    refreshBranchSelectionForCurrentAnchor()
  }

  private def currentAnchorIndex: Int = {
    val historySize = controller.moveHistoryManager.getMoveHistory.size
    if isViewingHistory then currentMoveIndex
    else historySize - 1
  }

  private def updateVariationStatusLabel(): Unit = {
    val anchor = currentAnchorIndex
    val variationsAtAnchor =
      if isViewingHistory then
        variationsForAnchor(anchor, parentContextForAnchor(anchor))
      else Vector.empty

    val selectionOpt = variationSelectionState.filter(_.anchorIndex == anchor)

    val message =
      if !isViewingHistory || variationsAtAnchor.isEmpty then ""
      else selectionOpt match
        case Some(state) if state.selectedIndex >= 0 && state.selectedIndex < state.variations.size =>
          val selection = state.variations(state.selectedIndex)
          val preview = s"${variationDisplayPrefix(selection)}${selection.preview(5)}"
          val indexText = s"${state.selectedIndex + 1}/${state.variations.size}"
          s"Variation $indexText selected: $preview\n(↑/↓ to browse, → to resume editing)"
        case _ =>
          val count = variationsAtAnchor.size
          val plural = if count == 1 then "" else "s"
          s"Mainline selected – $count saved variation$plural available (↑/↓ to browse, → to continue)."

    moveHistoryPanel.updateStatus(message)
  }

  private def clearVariationSelectionState(): Unit = {
    variationSelectionState = None
    notificationCenter.clear(BranchSelectionKey)
    updateVariationStatusLabel()
  }

  private def refreshVariationSelectionState(anchorIndex: Int, variations: Vector[LiveVariation]): Unit = {
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
            if state.selectedIndex < 0 then -1
            else math.min(state.selectedIndex, variations.size - 1)
          state.copy(variations = variations, selectedIndex = clamped)
        case _ =>
          VariationSelectionState(anchorIndex, variations, -1)
      variationSelectionState = Some(updatedState)
      updateVariationStatusLabel()
  }

  private def refreshBranchSelectionForAnchor(anchorIndex: Int): Vector[LiveVariation] = {
    val variations = variationsForAnchor(anchorIndex, parentContextForAnchor(anchorIndex))
    refreshVariationSelectionState(anchorIndex, variations)
    updateBranchSelectionOverlay(anchorIndex, variations)
    variations
  }

  private def refreshBranchSelectionForCurrentAnchor(): Vector[LiveVariation] =
    refreshBranchSelectionForAnchor(currentAnchorIndex)

  private def cycleVariationSelection(delta: Int): Boolean = {
    if activeVariationSession.nonEmpty then return false

    val anchor = currentAnchorIndex
    val variations = variationsForAnchor(anchor, parentContextForAnchor(anchor))
    if variations.isEmpty then
      clearVariationSelectionState()
      false
    else
      val options = -1 +: (0 until variations.size)
      val currentSelection = variationSelectionState
        .filter(_.anchorIndex == anchor)
        .map(_.selectedIndex)
        .filter(options.contains)
        .getOrElse(-1)

      val direction = if delta >= 0 then 1 else -1
      val currentPos = options.indexOf(currentSelection)
      val nextPos = (currentPos + direction + options.length) % options.length
      val nextSelection = options(nextPos)

      variationSelectionState = Some(VariationSelectionState(anchor, variations, nextSelection))
      updateVariationStatusLabel()
      updateMoveArrows()
      true
  }

  private def resumeSelectedVariation(): Boolean = {
    activeVariationSession match
      case Some(session) if session.pendingReplay.nonEmpty =>
        return playNextPendingVariationMove(session)
      case Some(session) if session.pendingReplay.isEmpty && session.moves.nonEmpty =>
        // Already at the end of the resumed variation; keep the session active
        updateMoveArrows()
        return false
      case _ =>

    variationSelectionState match
      case Some(state) if state.anchorIndex == currentAnchorIndex && state.selectedIndex >= 0 && state.selectedIndex < state.variations.size =>
        val variation = state.variations(state.selectedIndex)
        val progressed = resumeSavedVariation(variation, autoStep = true)
        if progressed then clearVariationSelectionState()
        progressed
      case Some(state) if state.anchorIndex == currentAnchorIndex && state.selectedIndex == -1 =>
        false
      case _ => false
  }

  private def resumeSavedVariation(variation: LiveVariation, autoStep: Boolean): Boolean = {
    val parentCtx = variationParentContext(variation)
    val session =
      activeVariationSession match
        case Some(existing) if existing.resumedFrom.exists(_.id == variation.id) =>
          existing.setResumedVariation(variation)
          liveVariationRepository.updateDraftResumedFrom(Some(variation))
          existing
        case Some(_) =>
          finishActiveVariation(save = false)
          val next = startVariationSession(
            anchorIndex = variation.anchorIndex,
            resumedVariation = Some(variation),
            parentContext = parentCtx
          )
          next.pendingReplay = variation.moves.toList
          next.pendingReplayColor = variation.startingColor
          liveVariationRepository.updateDraftResumedFrom(Some(variation))
          next
        case None =>
          val next = startVariationSession(
            anchorIndex = variation.anchorIndex,
            resumedVariation = Some(variation),
            parentContext = parentCtx
          )
          next.pendingReplay = variation.moves.toList
          next.pendingReplayColor = variation.startingColor
          liveVariationRepository.updateDraftResumedFrom(Some(variation))
          next

    val shouldStep = autoStep && session.pendingReplay.nonEmpty
    val stepped = if shouldStep then playNextPendingVariationMove(session) else true
    if !shouldStep then
      updateVariationMenuState()
      updateMoveArrows()
      triggerAnalysisIfActive()
    stepped
  }

  private def setMoveArrowsEnabled(enabled: Boolean): Unit = {
    showMoveArrows = enabled
    if showMoveArrowsMenuItem != null then showMoveArrowsMenuItem.selected = enabled
    updateMoveArrows()
  }

  private def updateMoveArrows(): Unit = {
    val history = controller.moveHistoryManager.getMoveHistory
    val anchorIndex = currentAnchorIndex

    variationSelectionState match
      case Some(state) if state.anchorIndex != anchorIndex =>
        clearVariationSelectionState()
      case _ =>

    val variations = refreshBranchSelectionForAnchor(anchorIndex)

    val selectedIndexOpt = variationSelectionState.collect {
      case state if state.anchorIndex == anchorIndex => state.selectedIndex
    }
    val mainlineSelected = selectedIndexOpt.contains(-1)

    if showMoveArrows then
      val hints = ListBuffer.empty[MoveArrow]
      val effectiveCell = if boardView.cellSize > 0 then boardView.cellSize else 64.0

      val nextIndex = anchorIndex + 1
      if nextIndex >= 0 && nextIndex < history.size then
        val move = history(nextIndex)
        val (fromCol, fromRow) = move.from.getCoordinates
        val (toCol, toRow) = move.to.getCoordinates
        val color = if mainlineSelected then Color.web("#ffca28") else Color.web("#4CAF50")
        val opacity = if mainlineSelected then 0.95 else 0.9
        val width =
          if mainlineSelected then math.max(effectiveCell * 0.16, 6.5)
          else math.max(effectiveCell * 0.12, 4.5)
        hints += MoveArrow(fromRow = fromRow, fromCol = fromCol, toRow = toRow, toCol = toCol, color = color, opacity = opacity, width = width)

      variations.zipWithIndex.foreach { case (variation, idx) =>
        variation.moves.headOption.foreach { san =>
          val isWhiteMove = variation.startingColor == "white"
          parseSanMove(san, isWhiteMove).foreach { parsed =>
            val (fromCol, fromRow) = parsed.from.getCoordinates
            val (toCol, toRow) = parsed.to.getCoordinates
            val isSelected = selectedIndexOpt.contains(idx)
            val color = if isSelected then Color.web("#ffca28") else Color.web("#64b5f6")
            val opacity = if isSelected then 0.95 else 0.75
            val width =
              if isSelected then math.max(effectiveCell * 0.14, 6.0)
              else math.max(effectiveCell * 0.1, 4.0)
            hints += MoveArrow(fromRow = fromRow, fromCol = fromCol, toRow = toRow, toCol = toCol, color = color, opacity = opacity, width = width)
          }
        }
      }

      boardView.setMoveHints(hints.toList)
    else
      boardView.setMoveHints(Nil)

    updateVariationStatusLabel()
  }

  private def handleActiveVariationDuringNavigation(targetIndex: Int): Unit = {
    activeVariationSession.foreach { session =>
      if targetIndex < session.anchorIndex then
        finishActiveVariation(save = true)
    }
  }

  private def gotoMoveIndex(targetIndex: Int): Unit = {
    val historyBefore = controller.moveHistoryManager.getMoveHistory
    if historyBefore.isEmpty then
      gotoStartPosition()
      return

    val maxIndexBefore = historyBefore.size - 1
    val boundedTarget = math.max(0, math.min(targetIndex, maxIndexBefore))

    handleActiveVariationDuringNavigation(boundedTarget)

    val historyAfter = controller.moveHistoryManager.getMoveHistory
    if historyAfter.isEmpty then
      gotoStartPosition()
      return

    val maxIndexAfter = historyAfter.size - 1
    val finalTarget = math.max(0, math.min(boundedTarget, maxIndexAfter))

    currentMoveIndex = finalTarget
    isViewingHistory = true
    applyLivePosition(boardView, finalTarget)
  }

  private def gotoStartPosition(): Unit = {
    handleActiveVariationDuringNavigation(-1)
    currentMoveIndex = -1
    isViewingHistory = true
    applyLivePosition(boardView, -1)
  }

  private def gotoLivePosition(): Unit = {
  if !autoCloseActiveVariation(saveIfMoves = true) then
      currentMoveIndex = -1
      isViewingHistory = false
      restoreBoardFromHistory()
  }

  private def handleMoveHistoryClick(moveIndex: Int): Unit = {
    if moveIndex < 0 then
      gotoStartPosition()
    else
      gotoMoveIndex(moveIndex)

    Platform.runLater { () =>
      boardView.requestFocus()
    }
  }

  // --- Simple live-game navigation helpers (no PGN/variations) ---

  /** Navigate one move forward in live move history. */
  private def navigateForward(): Unit = {
    val history = controller.moveHistoryManager.getMoveHistory
    if history.isEmpty then return

    if !isViewingHistory then return

    val atEndOfHistory = currentMoveIndex >= history.size - 1
    if atEndOfHistory then
      gotoLivePosition()
      return

    val nextIndex = currentMoveIndex + 1
    if nextIndex < 0 then
      gotoStartPosition()
    else
    gotoMoveIndex(nextIndex)
  }

  /** Navigate one move backward in live move history. */
  private def navigateBackward(): Unit = {
    val history = controller.moveHistoryManager.getMoveHistory
    if history.isEmpty then return

    val desiredIndex =
      if !isViewingHistory then history.size - 2
      else currentMoveIndex - 1

    if desiredIndex < 0 then
      gotoStartPosition()
    else
      gotoMoveIndex(desiredIndex)
  }

  /**
   * Apply board state corresponding to the given move index.
   * -1 => starting position, >=0 => position after that move.
   */
  private def applyLivePosition(boardView: ScalaFXBoardView, moveIndex: Int): Unit = {
    val history = controller.moveHistoryManager.getMoveHistory
    boardModel.resetBoard()
    controller.currentPlayer = "white"
    controller.board.foreach(_.deselectAll())
    boardView.clearSelectedSquare()
    boardView.clearLegalMoveTargets()

    if (moveIndex >= 0) then
      history.take(moveIndex + 1).zipWithIndex.foreach { case (m, _) =>
        controller.replayMove(m)
      }

    val nextPlayer =
      if (moveIndex < 0) then "white"
      else if (((moveIndex + 1) % 2) == 0) "white" else "black"
    controller.currentPlayer = nextPlayer

    boardView.refreshFromModel()

    // Update highlights & move-history highlight index
    if (moveIndex >= 0 && moveIndex < history.size) then
      val move = history(moveIndex)
      val fromCoords = move.from.getCoordinates
      val toCoords = move.to.getCoordinates
      boardView.setLastMove(fromCoords._2, fromCoords._1, toCoords._2, toCoords._1)
      updateMoveHistoryView(highlightIndex = Some(moveIndex))
    else
      boardView.clearLastMove()
      updateMoveHistoryView(highlightIndex = None)

    syncActiveVariationWithHistory()
    triggerAnalysisIfActive()
    updateMoveArrows()
  }

  override def stopApp(): Unit =
    analysisManager.stop()
}