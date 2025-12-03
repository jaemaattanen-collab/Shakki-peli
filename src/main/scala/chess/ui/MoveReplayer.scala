package chess.ui

import chess.board.{Board, Square}
import chess.controllers.{GameController, Move}
import chess.types.PieceType

/**
 * Result from restoreVariationPosition containing both the move and animation info
 */
case class VariationPositionResult(
  move: Option[Move],
  lastMoveCoords: Option[(Int, Int, Int, Int, String, String)] // fromRow, fromCol, toRow, toCol, color, pieceType (as string)
)

/**
 * Handles replaying moves visually on the board without modifying move history.
 * Used when navigating variations - the moves already exist in history,
 * we just need to show them on the board.
 */
class MoveReplayer(
  controller: GameController,
  boardModel: Board,
  moveArrowManager: MoveArrowManager
) {

  /**
   * Replay a move from SAN notation visually (without logging to history).
   * Updates the board state and current player.
   * Returns the move coordinates for animation purposes.
   * 
   * @param notation The SAN notation of the move (e.g., "e4", "Nxf3", "O-O")
   * @param isWhiteMove true if this is white's move
   * @return Option containing (fromRow, fromCol, toRow, toCol, color, pieceTypeString) for animation
   */
  def replayMoveFromSan(notation: String, isWhiteMove: Boolean): Option[(Int, Int, Int, Int, String, String)] = {
    moveArrowManager.parseSanMoveForArrow(notation, isWhiteMove) match
      case Some((fromSquare, toSquare)) =>
        controller.currentPlayer = if isWhiteMove then "white" else "black"
        fromSquare.occupiedBy match
          case Some(piece) =>
            val pieceColor = piece.color
            val pieceTypeStr = piece.pieceType.toString.toLowerCase
            val (fromCol, fromRow) = fromSquare.getCoordinates
            val (toCol, toRow) = toSquare.getCoordinates
            
            // Directly move the piece without going through handleAction/logMove
            val captured = toSquare.occupiedBy
            captured.foreach(_.capture())

            // Handle castling
            handleCastling(piece, fromSquare, toSquare)

            // Handle promotion
            val isPromotion = piece.pieceType == PieceType.Pawn &&
              ((isWhiteMove && toSquare.getCoordinates._2 == 0) ||
                (!isWhiteMove && toSquare.getCoordinates._2 == 7))

            if isPromotion then
              handlePromotion(piece, fromSquare, toSquare, notation)
            else
              // Normal move
              piece.moveTo(toSquare)
              piece.hasMoved = true

            // Alternate player
            controller.currentPlayer = if isWhiteMove then "black" else "white"
            
            // Return animation coordinates
            Some((fromRow, fromCol, toRow, toCol, pieceColor, pieceTypeStr))
          case None =>
            None
      case None =>
        None
  }

  /**
   * Restore board to a specific position within a variation.
   * Replays mainline up to branch point, then ancestor variations, then current variation.
   * 
   * @param variationRootTob The root TOB of the variation
   * @param moveIdx The index within the variation to restore to
   * @return VariationPositionResult containing the Move and animation coordinates
   */
  def restoreVariationPosition(variationRootTob: Long, moveIdx: Int): VariationPositionResult = {
    // Reset board to starting position
    boardModel.resetBoard()
    controller.currentPlayer = "white"
    controller.board.foreach(_.deselectAll())

    val mainlineMoves = controller.moveHistoryManager.getMoveHistory.filter(!_.isVariation)

    // Replay mainline up to the variation's anchor point
    val anchorIndex = controller.moveHistoryManager.getMainlineAnchorIndexForVariation(variationRootTob)
    val movesToReplay = mainlineMoves.take(anchorIndex + 1)
    movesToReplay.foreach { move =>
      controller.replayMove(move)
    }
    var colorToMove = if (anchorIndex + 1) % 2 == 0 then "white" else "black"

    // Helper to collect ancestor chain from root-level to current variation
    def collectAncestorChain(varRootTob: Long): List[(Long, Int)] =
      controller.moveHistoryManager.getParentVariationRootTob(varRootTob) match
        case Some(parentRootTob) =>
          val branchIdx = controller.moveHistoryManager.getParentVariationBranchIndex(varRootTob)
          collectAncestorChain(parentRootTob) :+ (varRootTob, branchIdx)
        case None =>
          List((varRootTob, -1)) // Root-level variation

    val ancestorChain = collectAncestorChain(variationRootTob)

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

    controller.currentPlayer = colorToMove
    controller.updateAllPossibleMoves()

    // Get the variation line
    val variationLine = controller.moveHistoryManager.collectVariationLine(variationRootTob)

    // Replay variation moves up to moveIdx, capturing animation info for the LAST move
    var resultMove: Option[Move] = None
    var lastMoveCoords: Option[(Int, Int, Int, Int, String, String)] = None
    if moveIdx >= 0 && variationLine.nonEmpty then
      val startingColor = controller.moveHistoryManager.getVariationStartingColor(variationRootTob)
      colorToMove = startingColor
      variationLine.take(moveIdx + 1).zipWithIndex.foreach { case (move, idx) =>
        move.notation.foreach { san =>
          val isWhite = colorToMove == "white"
          val coords = replayMoveFromSan(san, isWhite)
          // Only keep coords for the LAST move (for animation)
          if idx == moveIdx then
            lastMoveCoords = coords
          colorToMove = if isWhite then "black" else "white"
        }
      }

      // Return the move at moveIdx
      if moveIdx < variationLine.size then
        resultMove = Some(variationLine(moveIdx))

    // Update possible moves after restoring position
    controller.updateAllPossibleMoves()
    VariationPositionResult(resultMove, lastMoveCoords)
  }

  private def handleCastling(piece: chess.pieces.Piece, fromSquare: Square, toSquare: Square): Unit = {
    val isCastling = piece.pieceType == PieceType.King &&
      math.abs(fromSquare.getCoordinates._1 - toSquare.getCoordinates._1) == 2

    if isCastling then
      val (fromCol, fromRow) = fromSquare.getCoordinates
      val (toCol, _) = toSquare.getCoordinates
      if toCol - fromCol == 2 then // Kingside
        val rookFrom = boardModel.squares(fromRow)(7)
        val rookTo = boardModel.squares(fromRow)(5)
        rookFrom.occupiedBy.foreach { rook =>
          rook.moveTo(rookTo)
          rook.hasMoved = true
        }
      else if toCol - fromCol == -2 then // Queenside
        val rookFrom = boardModel.squares(fromRow)(0)
        val rookTo = boardModel.squares(fromRow)(3)
        rookFrom.occupiedBy.foreach { rook =>
          rook.moveTo(rookTo)
          rook.hasMoved = true
        }
  }

  private def handlePromotion(
    piece: chess.pieces.Piece,
    fromSquare: Square,
    toSquare: Square,
    notation: String
  ): Unit = {
    // Check notation for promotion piece
    val promotedType = notation.dropWhile(_ != '=').drop(1).headOption match
      case Some('Q') => PieceType.Queen
      case Some('R') => PieceType.Rook
      case Some('B') => PieceType.Bishop
      case Some('N') => PieceType.Knight
      case _ => PieceType.Queen // Default to queen

    // Replace pawn with promoted piece
    val promotedPiece = promotedType match
      case PieceType.Rook => new chess.pieces.Rook(piece.color, toSquare, boardModel)
      case PieceType.Bishop => new chess.pieces.Bishop(piece.color, toSquare, boardModel)
      case PieceType.Knight => new chess.pieces.Knight(piece.color, toSquare, boardModel)
      case _ => new chess.pieces.Queen(piece.color, toSquare, boardModel)

    fromSquare.occupiedBy = None
    toSquare.occupiedBy = Some(promotedPiece)
  }
}
