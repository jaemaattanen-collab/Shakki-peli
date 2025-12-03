package chess.controllers
import chess.board.{Board, Square}
import chess.pieces.Piece
import chess.pieces.{King as KingPiece, Pawn as PawnPiece}
import chess.pieces.{Rook as RookPiece, Bishop as BishopPiece, Knight as KnightPiece, Queen as QueenPiece}
import chess.state.CastlingRights
import chess.types.PieceType
import chess.intelligent._
import java.time.{Duration, ZoneId, ZonedDateTime}

class GameController(var board: Option[Board] = None) {  // Use `Option` to handle unset state
  val moveHistoryManager: MoveHistoryManager = new MoveHistoryManager()
  private val systemZoneId: ZoneId = ZoneId.systemDefault()
  var currentPlayer: String = "white"
  private var selectedSquare: Option[Square] = None
  var winner: Option[String] = None
  var halfmoveClock: Int = 0
  var fullmoveNumber: Int = 0
  var totalMoves: Int = 0  // Track total moves made for consistent numbering
  var enPassantTarget: Option[Piece] = None
  var autoflip: Boolean = false
  
  /** When true, moves are logged as variations (isVariation=true) instead of mainline */
  var variationMode: Boolean = false
  /** The tob of the first move in the current variation (set when entering variation mode) */
  var variationRootTob: Option[Long] = None
  /** Override parent TOB for variation creation - set this before creating a variation move */
  var variationParentTob: Option[Long] = None

  var castlingRights: CastlingRights = CastlingRights(whiteKingSide = true, whiteQueenSide = true, blackKingSide = true, blackQueenSide = true)

  private def computeTimeOfBirthMillis(): Long = {
    val now = ZonedDateTime.now(systemZoneId)
    val midnight = now.toLocalDate.atStartOfDay(systemZoneId)
    Duration.between(midnight, now).toMillis
  }

  def isStalemate(piece:Piece) = board match
      case Some(b) => !b.hasValidMoves(if piece.color == "white" then "black" else "white") && !b.isKingInCheck(if piece.color == "white" then "black" else "white")._1
      case None => sys.exit()
  def isCheckmate(piece:Piece) = board match
      case Some(b) =>
        b.isKingInCheck(if piece.color == "white" then "black" else "white")._1 && !b.hasValidMoves(if piece.color == "white" then "black" else "white")
      case None => sys.exit()

  def selectPiece(square: Square): Unit =
    square.occupiedBy match
      case Some(piece) if piece.color == currentPlayer && !piece.isCaptured =>
        selectedSquare = Some(square)
        board.get.selectSquare(square)
        square.occupiedBy.foreach { piece =>
        if (piece.isInstanceOf[KingPiece]) {
          val attackSquares = board.get.generateAttackSquaresBuffer(if (piece.color == "white") "black" else "white")
          val kingPossibleMoves = piece.possibleMoves().filterNot(attackSquares.contains)
          piece.updatePossibleMoves(kingPossibleMoves)}}
      case _ =>
  end selectPiece

  def handlePawnPromotion(pawn: PawnPiece, from: Square, to: Square, capturedPiece: Option[Piece]): PieceType =
    onPawnPromotion match
      case Some(callback) => callback(pawn, from, to, capturedPiece)
      case None => PieceType.Queen
  end handlePawnPromotion

  def deselectPiece(): Unit =
    selectedSquare = None
    board.get.deselectAll()
  end deselectPiece

  def handleAction(piece: Piece, targetSquare: Square): Boolean =
    // Check if trying to capture the king (illegal move - game should already be over)
    if targetSquare.occupiedBy.exists(p => p.pieceType == PieceType.King) then
      // This should never happen in a valid game - it means checkmate was missed
      println(s"ERROR: Attempted to capture king at ${targetSquare.name}. Game should have ended in checkmate!")
      deselectPiece()
      return false
    
    val validMoves = filterMovesToPreventCheck(piece, piece.possibleMoves())
    if validMoves.contains(targetSquare) then
      val isCastlingAction = piece.isInstanceOf[KingPiece] && isCastlingMove(piece, piece.square, targetSquare)
      movePiece(piece, piece.square, targetSquare, isCastlingAction)
      enPassantTarget = Some(piece)
      deselectPiece()
      switchPlayer()
      squareIndexes(board.get)
      true
    else
      deselectPiece()
      piece.square.occupiedBy = Some(piece)
      false
  end handleAction

  def checkInsufficientMaterial(): Unit =
    val pieces = board.get.squares.flatten.flatMap(_.occupiedBy)
    val nonKingPieces = pieces.filterNot(p => p.pieceType == PieceType.King)
    val finalCheck: Boolean = nonKingPieces.length match
      case 0 => true
      case 1 => nonKingPieces.exists(p => p.pieceType == PieceType.Knight || p.pieceType == PieceType.Bishop)
      case 2 => if (nonKingPieces.forall( p => p.pieceType == PieceType.Knight)) then true
                else nonKingPieces.forall(p => p.pieceType == PieceType.Knight || p.pieceType == PieceType.Bishop) && (!nonKingPieces.forall(_.color == "white") || !nonKingPieces.forall(_.color == "black"))
      case (_) => false
    if finalCheck then winner = Some("draw")
  end checkInsufficientMaterial

  var onPawnPromotion: Option[(PawnPiece, Square, Square, Option[Piece]) => PieceType] = None

  private def isCastlingMove(piece: Piece, from: Square, to: Square): Boolean =
    piece match
      case _: KingPiece =>
        val colDiff = to.getCoordinates._1 - from.getCoordinates._1
        (colDiff == 2 || colDiff == -2)
      case _ => false
  end isCastlingMove

  private def executeCastling(king: KingPiece, from: Square, to: Square): Unit =
    val (fromCol, fromRow) = from.getCoordinates
    val (toCol, _) = to.getCoordinates
    if (toCol - fromCol == 2)
      val rookFrom = board.get.squares(fromRow)(7)
      val rookTo = board.get.squares(fromRow)(5)
      rookFrom.occupiedBy.foreach(rook =>
        rook.moveTo(rookTo)
        rookFrom.occupiedBy = None
        rookTo.occupiedBy = Some(rook))
    else if (toCol - fromCol == -2)
      val rookFrom = board.get.squares(fromRow)(0)
      val rookTo = board.get.squares(fromRow)(3)
      rookFrom.occupiedBy.foreach(rook =>
        rook.moveTo(rookTo)
        rookFrom.occupiedBy = None
        rookTo.occupiedBy = Some(rook))
    king.moveTo(to)
    from.occupiedBy = None
    to.occupiedBy = Some(king)
  end executeCastling
  // Siirtää nappulan kohde ruutuun, on olemassa, en passant, castling, perussiirto ja capture
  // Half Clock Move nollaantuu, jos capture tapahtuu tai sotilasta siirretään
  //finalizeMove hoitaa full clock moven
  private def movePiece(piece: Piece, from: Square, to: Square, isCastlingAction: Boolean = false): Unit =
    // Increment total moves counter for consistent numbering
    totalMoves += 1
    
    var enPassant = false
    var promoted: Option[PieceType] = None
    var captured: Option[Piece] = to.occupiedBy
    
    // Calculate disambiguation BEFORE moving the piece
    val disambiguation = if !isCastlingAction then calculateDisambiguation(piece, from, to) else ""
    
    if piece.pieceType == PieceType.Pawn then
      val pawnPiece = piece.asInstanceOf[PawnPiece]
      enPassant = pawnPiece.isEnPassantCapture(to)
      halfmoveClock = 0
    if isCastlingAction then
      executeCastling(piece.asInstanceOf[KingPiece], from, to)
      logMove(piece, from, to, captured, isCastlingAction, None, "")
      endOfTurnActions(piece)
      halfmoveClock += 1
      return
    if captured.isDefined then
      captured match
        case Some(target) =>
          target.capture()
          to.occupiedBy = Some(piece)
          halfmoveClock = 0
        case None =>
    if (enPassant) then
      enPassantTarget match
        case Some(target) =>
          captured = Some(target)
        case None =>
    if (piece.pieceType == PieceType.Pawn) && (to.getCoordinates._2 == 0||to.getCoordinates._2 == 7) then
      promoted = Some(handlePawnPromotion(piece.asInstanceOf[PawnPiece], from, to, captured))
      logMove(piece, from, to, captured, false, promoted, disambiguation)
      endOfTurnActions(piece)
      halfmoveClock = 0
      return
    if captured.isEmpty then halfmoveClock += 1
    piece.moveTo(to)
    logMove(piece, from, to, captured, false, promoted, disambiguation)
    endOfTurnActions(piece)
  end movePiece

  def finalizeMove(): Unit =
      updateAllPossibleMoves()
      if (currentPlayer == "black") then
        fullmoveNumber += 1
      val newPosition = chess.state.Position.fromGameState(board.get, this)
      moveHistoryManager.addPosition(newPosition)
    end finalizeMove

  def endOfTurnActions(piece:Piece) =
    board.get.squares.foreach { row =>
      row.foreach { square =>
        square.seenByBlack = 0
        square.seenByWhite = 0
      }}
    finalizeMove()
    deselectPiece()
    checkInsufficientMaterial()
    if autoflip then
      board.get.flipped = !(board.get.flipped)
    if isCheckmate(piece) then
      winner = Some(piece.color)
    else if isStalemate(piece) then
      winner = Some("draw")
    else if (moveHistoryManager.isThreefoldRepetition) then
        winner = Some("draw")
  end endOfTurnActions


  def resetGame(): Unit =
    moveHistoryManager.clearHistory()
    currentPlayer = "white"
    winner = None
    totalMoves = 0  // Reset total moves counter
  end resetGame

  def updateAllPossibleMoves(): Unit =
    for {
      row <- board.get.squares
      square <- row
      piece <- square.occupiedBy if !piece.isCaptured
    } {
      val filteredMoves = filterMovesToPreventCheck(piece, piece.possibleMoves())
      piece.updatePossibleMoves(filteredMoves)
    }
  end updateAllPossibleMoves

  def filterMovesToPreventCheck(piece: Piece, possibleMoves: List[Square]): List[Square] =
    possibleMoves.filter { targetSquare =>
    val originalSquare = piece.square
    // CRITICAL: Save the captured piece BEFORE moving
    val capturedPiece = targetSquare.occupiedBy
    val capturedPieceOriginalSquare = capturedPiece.map(_.square)
    
    // Simulate the move
    piece.moveTo(targetSquare, isSimulation = true)
    
    // Check if king would be in check after this move
    val kingInCheck = board.exists(_.isKingInCheck(piece.color)._1)
    
    // Undo the move
    piece.moveTo(originalSquare, isSimulation = true)
    targetSquare.occupiedBy = capturedPiece
    capturedPiece.foreach { cp =>
      cp.square = capturedPieceOriginalSquare.getOrElse(targetSquare)
    }
    
    // Move is valid if king is NOT in check
    !kingInCheck
  }
  end filterMovesToPreventCheck

  def logMove(piece: Piece, from: Square, to: Square, captured: Option[Piece], isCastlingMove: Boolean, promotedPieceType: Option[PieceType], disambiguation: String): Unit =
    val tob = computeTimeOfBirthMillis()
    // For mainline moves, use last mainline move as parent
    // For variation moves, use variationParentTob if set (first move in variation),
    // otherwise use last move IN THE CURRENT VARIATION (for continuation moves)
    val lastMove = if variationMode then
      // If we have a variationRootTob, get the last move in that variation
      // Otherwise fall back to getLastMove
      variationRootTob match
        case Some(rootTob) =>
          val variationLine = moveHistoryManager.collectVariationLine(rootTob)
          variationLine.lastOption
        case None =>
          // First move in variation - use the move at branch point
          None
    else 
      moveHistoryManager.getLastMainlineMove
    // Use variationParentTob if set (for variation creation), otherwise use last move
    val parentTob = variationParentTob.getOrElse(lastMove.map(_.tob).getOrElse(-1L))
    val parentDepth = variationParentTob match
      case Some(-1L) => 0  // Starting position has depth 0
      case Some(ptob) => moveHistoryManager.getMoveByTob(ptob).map(_.halfmoveDistanceFromStart).getOrElse(0)
      case None => lastMove.map(_.halfmoveDistanceFromStart).getOrElse(0)
    
    // Track variationRootTob for the first move in a variation - MUST be set BEFORE creating Move
    if variationMode && variationRootTob.isEmpty then
      variationRootTob = Some(tob)
    
    // Get defender/attacker counts for target square BEFORE making the move
    // This is used for sacrifice detection
    val (defenders, attackers) = board match {
      case Some(b) =>
        val targetSquare = to
        val isWhiteMove = piece.color == "white"
        // Defenders = own pieces seeing the square (excluding the moving piece itself)
        // Attackers = opponent pieces seeing the square
        val ownSeen = if (isWhiteMove) targetSquare.seenByWhite else targetSquare.seenByBlack
        val oppSeen = if (isWhiteMove) targetSquare.seenByBlack else targetSquare.seenByWhite
        // Subtract 1 from defenders if the moving piece was seeing the target (it won't defend after moving there)
        // Actually, seenBy counts don't include the moving piece's new position, so we use them directly
        (ownSeen, oppSeen)
      case None => (0, 0)
    }
    
    val baseMove = Move(
      piece,
      from,
      to,
      captured,
      promotedPieceType,
      tob = tob,
      parentTob = parentTob,
      halfmoveDistanceFromStart = parentDepth + 1,
      isVariation = variationMode,
      variationRootTob = if variationMode then variationRootTob else None,
      targetSquareDefenders = defenders,
      targetSquareAttackers = attackers
    )
    val notation =
      if (isCastlingMove)
        if (to.getCoordinates._1 > from.getCoordinates._1) "O-O" else "O-O-O"
      else
        moveHistoryManager.generateAlgebraicNotationWithDisambiguation(
          baseMove, 
          board.getOrElse(new Board(this)),
          disambiguation
        )
    val move = baseMove.copy(notation = Some(notation))
    
    // Calculate move number and determine if it's white's move
    // Use the current move count to determine move number and color
    val moveCount = moveHistoryManager.notationManager.getMoveCount()
    val actualMoveNumber = (moveCount / 2) + 1
    val actualIsWhite = (moveCount % 2) == 0
    
    // Use appropriate storage method based on variation mode
    if variationMode then
      moveHistoryManager.addVariationMove(move)
    else
      moveHistoryManager.addMoveToNotation(move, notation, actualMoveNumber, actualIsWhite)
  end logMove

  /**
   * Replay a previously recorded move on the current board state WITHOUT
   * mutating move history or clocks.
   *
   * This is specifically intended for navigation / visualisation scenarios
   * (Swing NavigationManager, PGNNavigationManager live navigation, ScalaFX
   * navigation helpers) where we want to reconstruct a position from the
   * existing linear history, not create new moves.
   *
   * Behavioural contract:
   *   - Uses the stored from/to square names to locate the piece on the
   *     *current* board instance.
   *   - Updates the board and basic controller turn state, but DOES NOT:
   *       * append anything to MoveHistoryManager (notation or move list)
   *       * touch clocks / halfmoveClock / fullmoveNumber / totalMoves
   *       * set winner or other end-of-game flags
   */
  def replayMove(move: Move): Boolean =
    board match
      case Some(b) =>
        val fromName = move.from.name
        val toName   = move.to.name
        val fromSqOpt = b.getSquareByName(fromName)
        val toSqOpt   = b.getSquareByName(toName)

        (fromSqOpt, toSqOpt) match
          case (Some(fromSq), Some(toSq)) =>
            fromSq.occupiedBy match
              case Some(p) =>
                // Apply a *minimal* move: directly manipulate board squares
                // rather than going through handleAction/movePiece/logMove.
                // This mirrors NavigationManager.replayMoveFromHistory and
                // PGNNavigationManager.replayMoveFromHistory behaviour.

                val isCastlingReplay =
                  p.pieceType == PieceType.King && math.abs(p.square.getCoordinates._1 - toSq.getCoordinates._1) == 2

                fromSq.occupiedBy = None

                if isCastlingReplay then
                  val (fromCol, fromRow) = move.from.getCoordinates
                  val (toCol, _) = move.to.getCoordinates
                  if toCol - fromCol == 2 then
                    val rookFrom = b.squares(fromRow)(7)
                    val rookTo = b.squares(fromRow)(5)
                    rookFrom.occupiedBy.foreach { rook =>
                      rook.moveTo(rookTo)
                      rook.hasMoved = true
                    }
                  else if toCol - fromCol == -2 then
                    val rookFrom = b.squares(fromRow)(0)
                    val rookTo = b.squares(fromRow)(3)
                    rookFrom.occupiedBy.foreach { rook =>
                      rook.moveTo(rookTo)
                      rook.hasMoved = true
                    }

                // Remove any captured piece occupying the destination square
                toSq.occupiedBy.foreach(_.capture())
                move.capturedPiece.foreach(_.capture())

                move.promotedPieceType match
                  case Some(promotedType) =>
                    // Replace the pawn with the promoted piece when replaying history
                    val promotedPiece = promotedType match
                      case PieceType.Rook   => new RookPiece(p.color, toSq, b)
                      case PieceType.Bishop => new BishopPiece(p.color, toSq, b)
                      case PieceType.Knight => new KnightPiece(p.color, toSq, b)
                      case _                => new QueenPiece(p.color, toSq, b)

                    promotedPiece.hasMoved = true
                    toSq.occupiedBy = Some(promotedPiece)
                  case None =>
                    toSq.occupiedBy = Some(p)
                    p.square = toSq
                    p.hasMoved = true

                // Update current player turn: alternate by move color
                currentPlayer = if currentPlayer == "white" then "black" else "white"

                true
              case None => false
          case _ => false
      case None => false

  private def calculateDisambiguation(piece: Piece, from: Square, to: Square): String =
    if (piece.pieceType == PieceType.Pawn || piece.pieceType == PieceType.King) then
      return ""
    
    // Etsi kaikki samanväriset ja samantyyppiset nappulat (paitsi siirrettävä itse)
    val allSameTypePieces = board.get.squares.flatten
      .flatMap(_.occupiedBy)
      .filter(p => p.color == piece.color && 
                  p.pieceType == piece.pieceType && 
                  p != piece)
    
    val ambiguousPieces = allSameTypePieces.filter { p =>
      // Tarkista voiko tämä nappula LAILLISESTI siirtyä samaan kohteeseen
      val validMoves = filterMovesToPreventCheck(p, p.possibleMoves())
      validMoves.exists(_.name == to.name)
    }
    
    if (ambiguousPieces.isEmpty) then
      ""
    else
      val fromFile = from.name(0).toLower
      val fromRank = from.name(1)
      
      // Tarkista onko sama file riittävä
      val sameFile = ambiguousPieces.filter { p => 
        board.get.squares.flatten.find(sq => sq.occupiedBy.contains(p))
          .exists(_.name(0).toLower == fromFile)
      }
      
      // Tarkista onko sama rank riittävä
      val sameRank = ambiguousPieces.filter { p => 
        board.get.squares.flatten.find(sq => sq.occupiedBy.contains(p))
          .exists(_.name(1) == fromRank)
      }
      
      if (sameFile.isEmpty) then fromFile.toString
      else if (sameRank.isEmpty) then fromRank.toString
      else s"$fromFile$fromRank" // Molemmat tarvitaan
  end calculateDisambiguation

  private def switchPlayer(): Unit =
    currentPlayer = if (currentPlayer == "white") "black" else "white"
    val pawnsToReset = board.get.squares.flatten.flatMap(_.occupiedBy).collect {case pawn: PawnPiece if pawn.color == currentPlayer => pawn}
    pawnsToReset.foreach(pawn => pawn.justMovedTwoSquares = false)
  end switchPlayer
  
}