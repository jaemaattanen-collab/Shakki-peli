package chess.ui

import chess.board.{Board, Square}
import chess.controllers.{GameController, Move}
import chess.ui.ScalaFXBoardView.MoveArrow
import scalafx.scene.paint.Color

import scala.collection.mutable.ListBuffer

/**
 * Manages move arrow visualization for the chess board.
 * Calculates and renders arrows showing available moves, variations, and selected moves.
 */
class MoveArrowManager(
  controller: GameController,
  boardModel: Board,
  boardView: ScalaFXBoardView
) {

  private var showMoveArrows: Boolean = true

  def isEnabled: Boolean = showMoveArrows

  def setEnabled(enabled: Boolean): Unit = {
    showMoveArrows = enabled
  }

  /**
   * Update move arrows based on current position and variation state.
   * 
   * @param viewingVariationRootTob Currently viewed variation root (None if mainline)
   * @param currentVariationMove Current move within variation
   * @param anchorIndex Current anchor index in mainline
   * @param variations Available variations at current position
   * @param selectedIndex Currently selected variation index (-1 for mainline/continue)
   * @param engineArrows Engine suggestion arrows to display (from live analysis)
   */
  def updateArrows(
    viewingVariationRootTob: Option[Long],
    currentVariationMove: Option[Move],
    anchorIndex: Int,
    variations: Vector[Move],
    selectedIndex: Option[Int],
    engineArrows: List[MoveArrow] = List.empty
  ): Unit = {
    if !showMoveArrows then
      // Still show engine arrows even if variation arrows are disabled
      boardView.setMoveHints(engineArrows)
      return

    val history = controller.moveHistoryManager.getMoveHistory
    val hints = ListBuffer.empty[MoveArrow]
    val effectiveCell = if boardView.cellSize > 0 then boardView.cellSize else 64.0

    val mainlineSelected = selectedIndex.contains(-1)

    viewingVariationRootTob match
      case Some(rootTob) =>
        // Inside a variation - show arrow for next variation move AND nested variations
        renderVariationArrows(rootTob, currentVariationMove, selectedIndex, hints, effectiveCell)

      case None =>
        // Normal mainline view - show mainline continuation and variations
        renderMainlineArrows(history, anchorIndex, mainlineSelected, variations, selectedIndex, hints, effectiveCell)

    // Combine variation arrows with engine arrows (engine arrows go first so they appear behind)
    boardView.setMoveHints(engineArrows ++ hints.toList)
  }

  private def renderVariationArrows(
    rootTob: Long,
    currentVariationMove: Option[Move],
    selectedIndex: Option[Int],
    hints: ListBuffer[MoveArrow],
    effectiveCell: Double
  ): Unit = {
    val variationLine = controller.moveHistoryManager.collectVariationLine(rootTob)
    val currentIdx = currentVariationMove.map(m => variationLine.indexWhere(_.tob == m.tob)).getOrElse(-1)

    // Get nested variations at current position
    val nestedVariations = if currentIdx >= 0 then
      controller.moveHistoryManager.getNestedVariationRootsAt(rootTob, currentIdx).toVector
    else
      Vector.empty

    // Check if "continue in variation" is selected (-1) or a nested variation
    val continueSelected = selectedIndex.contains(-1)

    // Find next move in variation line (by index, not ply)
    val nextMoveOpt = if currentIdx >= 0 && currentIdx + 1 < variationLine.size then
      Some(variationLine(currentIdx + 1))
    else if currentIdx == -1 && variationLine.nonEmpty then
      Some(variationLine.head)
    else
      None

    nextMoveOpt.foreach { nextMove =>
      val (fromCol, fromRow) = nextMove.from.getCoordinates
      val (toCol, toRow) = nextMove.to.getCoordinates
      // Highlight if "continue" is selected, otherwise normal variation color
      val color = if continueSelected && nestedVariations.nonEmpty then Color.web("#ffca28") else Color.web("#64b5f6")
      val opacity = if continueSelected && nestedVariations.nonEmpty then 0.95 else 0.9
      val width = if continueSelected && nestedVariations.nonEmpty then
        math.max(effectiveCell * 0.16, 6.5)
      else
        math.max(effectiveCell * 0.14, 5.5)
      hints += MoveArrow(
        fromRow = fromRow, fromCol = fromCol,
        toRow = toRow, toCol = toCol,
        color = color,
        opacity = opacity,
        width = width
      )
    }

    // Show arrows for nested variations
    nestedVariations.zipWithIndex.foreach { case (nestedRoot, idx) =>
      val firstNotation = controller.moveHistoryManager.getVariationMoveNotations(nestedRoot.tob).headOption
      val startingColor = controller.moveHistoryManager.getVariationStartingColor(nestedRoot.tob)
      firstNotation.foreach { san =>
        val isWhiteMove = startingColor == "white"
        parseSanMoveForArrow(san, isWhiteMove).foreach { case (fromSquare, toSquare) =>
          val (fromCol, fromRow) = fromSquare.getCoordinates
          val (toCol, toRow) = toSquare.getCoordinates
          val isSelected = selectedIndex.contains(idx)
          val color = if isSelected then Color.web("#ffca28") else Color.web("#64b5f6")
          val opacity = if isSelected then 0.95 else 0.75
          val width = if isSelected then math.max(effectiveCell * 0.14, 6.0)
          else math.max(effectiveCell * 0.1, 4.0)
          hints += MoveArrow(fromRow = fromRow, fromCol = fromCol, toRow = toRow, toCol = toCol, color = color, opacity = opacity, width = width)
        }
      }
    }
  }

  private def renderMainlineArrows(
    history: List[Move],
    anchorIndex: Int,
    mainlineSelected: Boolean,
    variations: Vector[Move],
    selectedIndex: Option[Int],
    hints: ListBuffer[MoveArrow],
    effectiveCell: Double
  ): Unit = {
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

    variations.zipWithIndex.foreach { case (variationRoot, idx) =>
      val firstNotation = controller.moveHistoryManager.getVariationMoveNotations(variationRoot.tob).headOption
      val startingColor = controller.moveHistoryManager.getVariationStartingColor(variationRoot.tob)
      firstNotation.foreach { san =>
        val isWhiteMove = startingColor == "white"
        parseSanMoveForArrow(san, isWhiteMove).foreach { case (fromSquare, toSquare) =>
          val (fromCol, fromRow) = fromSquare.getCoordinates
          val (toCol, toRow) = toSquare.getCoordinates
          val isSelected = selectedIndex.contains(idx)
          val color = if isSelected then Color.web("#ffca28") else Color.web("#64b5f6")
          val opacity = if isSelected then 0.95 else 0.75
          val width =
            if isSelected then math.max(effectiveCell * 0.14, 6.0)
            else math.max(effectiveCell * 0.1, 4.0)
          hints += MoveArrow(fromRow = fromRow, fromCol = fromCol, toRow = toRow, toCol = toCol, color = color, opacity = opacity, width = width)
        }
      }
    }
  }

  /**
   * Parse a SAN move notation and return the from/to squares for arrow rendering.
   */
  def parseSanMoveForArrow(notation: String, isWhiteMove: Boolean): Option[(Square, Square)] = {
    if notation == null || notation.trim.isEmpty then
      return None

    val normalized = notation.replace('0', 'O').trim
    val clean = normalized.replaceAll("[+#?!]", "")
    if clean.isEmpty then
      return None

    // Handle castling
    if clean == "O-O" || clean == "O-O-O" then
      val fromName = if isWhiteMove then "e1" else "e8"
      val toName =
        if clean == "O-O" then
          if isWhiteMove then "g1" else "g8"
        else
          if isWhiteMove then "c1" else "c8"
      for
        fromSq <- boardModel.getSquareByName(fromName)
        toSq <- boardModel.getSquareByName(toName)
      yield (fromSq, toSq)
    else
      parsePieceMoveForArrow(clean, isWhiteMove)
  }

  private def parsePieceMoveForArrow(clean: String, isWhiteMove: Boolean): Option[(Square, Square)] = {
    val promotionIndex = clean.indexOf('=')
    val body = if promotionIndex >= 0 then clean.substring(0, promotionIndex) else clean

    if body.length < 2 then
      return None

    val firstChar = body.head
    val (pieceType, remainder) =
      if "NBRQK".contains(firstChar) then (firstChar.toString, body.substring(1))
      else ("P", body)

    if remainder.length < 2 then
      return None

    val destinationStr = remainder.takeRight(2).toLowerCase
    val disambiguation = remainder.dropRight(2).replace("x", "")
    val destSquareOpt = boardModel.getSquareByName(destinationStr)
    if destSquareOpt.isEmpty then
      return None
    val destSquare = destSquareOpt.get

    val fileHint = disambiguation.find(_.isLetter).map(_.toLower - 'a')
    val rankHint = disambiguation.find(_.isDigit).map(d => 8 - (d.asDigit))
    val color = if isWhiteMove then "white" else "black"

    val expectedPieceTypeName = sanToPieceTypeName(pieceType)

    val candidates = boardModel.squares.iterator.flatMap(_.iterator).flatMap { square =>
      square.occupiedBy match
        case Some(piece) if piece.color == color && piece.pieceType.toString == expectedPieceTypeName && !piece.isCaptured =>
          val (col, row) = square.getCoordinates
          val fileMatches = fileHint.forall(_ == col)
          val rankMatches = rankHint.forall(_ == row)
          if fileMatches && rankMatches then
            val legalMoves = controller.filterMovesToPreventCheck(piece, piece.possibleMoves())
            val containsDest = legalMoves.contains(destSquare)
            if containsDest then Some(square) else None
          else None
        case _ => None
    }.toList

    candidates.headOption.map(fromSquare => (fromSquare, destSquare))
  }

  private def sanToPieceTypeName(san: String): String = san match
    case "N" => "Knight"
    case "B" => "Bishop"
    case "R" => "Rook"
    case "Q" => "Queen"
    case "K" => "King"
    case "P" => "Pawn"
    case _ => san

  def clearArrows(): Unit = {
    boardView.setMoveHints(Nil)
  }
}
