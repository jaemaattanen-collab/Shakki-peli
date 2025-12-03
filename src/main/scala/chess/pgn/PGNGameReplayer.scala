package chess.pgn

import chess.board.Board
import chess.controllers.GameController
import chess.state.CastlingRights
import chess.pieces.{Pawn as PawnPiece, Piece as ChessPiece}
import chess.board.Square
import chess.types.PieceType

/**
 * Rebuilds a live game state from parsed PGN mainlines by replaying moves
 * through the {@link GameController}. This ensures move history, notation,
 * and TOB/parent relationships mirror live play.
 */
object PGNGameReplayer:

  /**
   * Apply the mainline moves of the PGN game to the provided controller/board.
   * Resets the game state before replaying moves so the resulting history
   * matches the parsed PGN.
   */
  def replay(game: PGNGame, controller: GameController, board: Board): Either[String, Unit] =
    resetState(controller, board)

    val sanEntries = buildSanEntries(game.moves)
    controller.updateAllPossibleMoves()

    var plyIndex = 0
    var error: Option[String] = None

    while error.isEmpty && plyIndex < sanEntries.length do
      val entry = sanEntries(plyIndex)
      val (san, isWhiteMove, moveNumber, colourLabel) = entry

      SANParser.parseMove(san, isWhiteMove, controller, board) match
        case Left(message) =>
          error = Some(s"$message (move $moveNumber $colourLabel)")
        case Right(parsed) =>
          controller.currentPlayer = colourLabel
          val originPiece = parsed.from.occupiedBy
          originPiece match
            case None =>
              error = Some(s"No piece present on ${parsed.from.name} for SAN move '$san' (move $moveNumber $colourLabel)")
            case Some(piece) =>
              val previousPromotionHandler = controller.onPawnPromotion
              controller.onPawnPromotion = promotionCallback(parsed.promotion)
              val moved = controller.handleAction(piece, parsed.to)
              controller.onPawnPromotion = previousPromotionHandler

              if !moved then
                error = Some(s"Failed to apply SAN move '$san' (move $moveNumber $colourLabel)")
              else
                controller.updateAllPossibleMoves()
      plyIndex += 1

    error match
      case Some(message) => Left(message)
      case None =>
        controller.winner = winnerFromResult(game.result)
        Right(())

  private def promotionCallback(promotion: Option[PieceType]): Option[(PawnPiece, Square, Square, Option[ChessPiece]) => PieceType] =
    promotion.map { promotionType =>
      (_: PawnPiece, _: Square, _: Square, _: Option[ChessPiece]) => promotionType
    }

  private def buildSanEntries(moves: List[PGNMove]): Vector[(String, Boolean, Int, String)] =
    val buffer = scala.collection.mutable.ArrayBuffer[(String, Boolean, Int, String)]()
    moves.foreach { move =>
      move.whiteMove.map(_.trim).filter(_.nonEmpty).foreach { san =>
        buffer += ((san, true, move.moveNumber, "white"))
      }
      move.blackMove.map(_.trim).filter(_.nonEmpty).foreach { san =>
        buffer += ((san, false, move.moveNumber, "black"))
      }
    }
    buffer.toVector

  private def resetState(controller: GameController, board: Board): Unit =
    controller.resetGame()
    controller.currentPlayer = "white"
    controller.winner = None
    controller.halfmoveClock = 0
    controller.fullmoveNumber = 0
    controller.totalMoves = 0
    controller.enPassantTarget = None
    controller.castlingRights = CastlingRights(whiteKingSide = true, whiteQueenSide = true, blackKingSide = true, blackQueenSide = true)
    controller.board = Some(board)
    board.resetBoard()
    controller.deselectPiece()
    controller.onPawnPromotion = None

  private def winnerFromResult(result: Option[String]): Option[String] =
    result.map(_.trim).flatMap {
      case "1-0"       => Some("white")
      case "0-1"       => Some("black")
      case "1/2-1/2"   => Some("draw")
      case "*"         => None
      case other if other.equalsIgnoreCase("1-0")     => Some("white")
      case other if other.equalsIgnoreCase("0-1")     => Some("black")
      case other if other.equalsIgnoreCase("1/2-1/2") => Some("draw")
      case _ => None
    }

end PGNGameReplayer
