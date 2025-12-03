package chess.pgn

import chess.board.{Board, Square}
import chess.controllers.GameController
import chess.types.PieceType

/**
 * Utility for resolving SAN (Standard Algebraic Notation) strings into
 * concrete move metadata using the current board/controller state.
 */
object SANParser:

  final case class ParsedMove(from: Square, to: Square, promotion: Option[PieceType])

  /**
   * Parse a SAN string into a concrete move description.
   *
   * @param notation SAN move text (e.g. "Nf3", "exd5", "O-O", "c8=Q")
   * @param isWhiteMove whether the move to parse belongs to white
   * @param controller active game controller (used for legality checks)
   * @param board active board instance containing current piece placement
   */
  def parseMove(notation: String, isWhiteMove: Boolean, controller: GameController, board: Board): Either[String, ParsedMove] =
    if notation == null || notation.trim.isEmpty then
      return Left("SAN notation is empty")

    val normalized = notation.replace('0', 'O').trim
    val clean = normalized.replaceAll("[+#?!]", "")
    if clean.isEmpty then
      return Left(s"SAN notation '$notation' has no actionable content")

    if clean == "O-O" || clean == "O-O-O" then
      val fromName = if isWhiteMove then "e1" else "e8"
      val toName =
        if clean == "O-O" then (if isWhiteMove then "g1" else "g8")
        else (if isWhiteMove then "c1" else "c8")

      for
        fromSq <- board.getSquareByName(fromName).toRight(s"Square '$fromName' not found for castling move '$notation'")
        toSq <- board.getSquareByName(toName).toRight(s"Square '$toName' not found for castling move '$notation'")
      yield ParsedMove(fromSq, toSq, None)
    else
      parseNonCastlingMove(clean, notation, isWhiteMove, controller, board)

  private def parseNonCastlingMove(clean: String, original: String, isWhiteMove: Boolean, controller: GameController, board: Board): Either[String, ParsedMove] =
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

    if body.length < 2 then return Left(s"Unable to parse SAN move '$original'")

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

    if remainder.length < 2 then
      return Left(s"Missing destination square in SAN move '$original'")

    val destinationStr = remainder.takeRight(2).toLowerCase
    val disambiguationRaw = remainder.dropRight(2)
    val disambiguation = disambiguationRaw.replace("x", "")

    board.getSquareByName(destinationStr).toRight(s"Destination square '$destinationStr' is invalid in SAN move '$original'").flatMap { destSquare =>
      val fileHints = disambiguation.filter(_.isLetter).map(_.toLower)
      val rankHints = disambiguation.filter(_.isDigit)
      val fileHint = fileHints.lastOption.map(_ - 'a')
      val rankHint = rankHints.lastOption.map(d => 8 - d.asDigit)
      val colour = if isWhiteMove then "white" else "black"

      val candidates = board.squares.iterator.flatMap(_.iterator).flatMap { square =>
        square.occupiedBy match
          case Some(piece) if piece.color == colour && piece.pieceType == pieceType && !piece.isCaptured =>
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
        case Nil => Left(s"No legal origin square resolved for SAN move '$original'")
        case origin :: Nil => Right(ParsedMove(origin, destSquare, promotionType))
        case origin :: _ =>
          // Disambiguation failed to reduce to a single candidate; prefer the first legal origin
          Right(ParsedMove(origin, destSquare, promotionType))
    }

end SANParser
