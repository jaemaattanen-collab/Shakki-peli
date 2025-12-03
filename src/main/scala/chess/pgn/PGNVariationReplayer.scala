package chess.pgn

import chess.board.Board
import chess.controllers.{GameController, Move, MoveHistoryManager}
import chess.pieces.{Pawn as PawnPiece, Piece as ChessPiece}
import chess.board.Square

/**
 * Replays PGN variations into the Move tree stored in MoveHistoryManager.
 * This creates proper Move objects with TOB/parentTob relationships for each variation.
 */
object PGNVariationReplayer:

  /**
   * Replay all variations from a PGN game into the MoveHistoryManager.
   * This should be called AFTER the mainline has been replayed.
   * 
   * @param game The parsed PGN game containing variations
   * @param controller The game controller (needed for move validation)
   * @param board The board instance
   * @param moveHistoryManager The move history manager to add variations to
   */
  def replayVariations(
    game: PGNGame,
    controller: GameController,
    board: Board,
    moveHistoryManager: MoveHistoryManager
  ): List[String] = {
    val errors = scala.collection.mutable.ListBuffer[String]()
    
    // Collect all variations: root-level + those attached to moves
    val rootVariations = game.variations
    val moveVariations = game.moves.flatMap(_.variations)
    val allVariations = rootVariations ++ moveVariations
    
    // Process each variation
    allVariations.foreach { variation =>
      replayVariation(variation, None, controller, board, moveHistoryManager) match
        case Left(error) => errors += error
        case Right(_) => ()
    }
    
    errors.toList
  }

  /**
   * Replay a single variation (and its nested sub-variations) into the Move tree.
   * 
   * @param variation The PGN variation to replay
   * @param parentVariationRootTob If this is a nested variation, the root TOB of the parent variation
   * @param controller Game controller for move validation
   * @param board Board instance
   * @param moveHistoryManager Move history manager
   * @return Either an error message or Unit on success
   */
  private def replayVariation(
    variation: PGNVariation,
    parentVariationRootTob: Option[Long],
    controller: GameController,
    board: Board,
    moveHistoryManager: MoveHistoryManager
  ): Either[String, Unit] = {
    
    // Calculate anchor index (mainline half-move index where this branches)
    val anchorIndex = anchorIndexFromPGN(variation.originMoveNumber, variation.originColor)
    
    // Find the parent Move (where this variation branches from)
    val parentTob: Long = parentVariationRootTob match
      case Some(parentRootTob) =>
        // Nested variation - find the parent move within the parent variation
        val parentLine = moveHistoryManager.collectVariationLine(parentRootTob)
        val plyWithinParent = calculatePlyOffset(variation, parentLine, moveHistoryManager, parentRootTob)
        // Parent is the move BEFORE the move this variation is alternative to
        // If plyWithinParent is 0, parent is the mainline move before this variation
        if plyWithinParent > 0 then
          parentLine.lift(plyWithinParent - 1).map(_.tob).getOrElse(-1L)
        else
          // Branch from start of parent variation - parent is parent variation's parent
          moveHistoryManager.getMoveByTob(parentRootTob)
            .flatMap(m => if m.parentTob == -1L then None else Some(m.parentTob))
            .getOrElse(-1L)
      case None =>
        // Root-level variation - branches from mainline
        if anchorIndex < 0 then -1L  // Branches from starting position
        else
          moveHistoryManager.getMainlineMoveAt(anchorIndex).map(_.tob).getOrElse(-1L)
    
    // Get the board state at the branch point
    val branchBoardState = saveBoardState(board)
    restoreBoardToBranchPoint(anchorIndex, parentVariationRootTob, controller, board, moveHistoryManager, Some(variation))
    
    // Extract SAN moves from the variation
    val sanMoves = flattenPGNMoves(variation.moves)
    if sanMoves.isEmpty then
      restoreBoardState(board, branchBoardState)
      return Right(())
    
    // Build a map of which PGN move indices have attached variations
    val variationsByMoveIndex = buildVariationMap(
      variation.moves, 
      variation.startingMoveNumber, 
      variation.startingColor
    )
    
    // Collect nested variations with their branch points for later processing
    val nestedVariationsToProcess = scala.collection.mutable.ListBuffer[(PGNVariation, Int)]()
    
    // Replay each move in the variation
    var currentParentTob = parentTob
    var variationRootTob: Option[Long] = None
    var isWhiteMove = variation.startingColor.toLowerCase == "white"
    var plyIndex = 0
    var error: Option[String] = None
    
    controller.updateAllPossibleMoves()
    
    while error.isEmpty && plyIndex < sanMoves.length do
      val san = sanMoves(plyIndex)
      val colorLabel = if isWhiteMove then "white" else "black"
      
      SANParser.parseMove(san, isWhiteMove, controller, board) match
        case Left(message) =>
          error = Some(s"Variation replay error: $message (move ${variation.startingMoveNumber + plyIndex / 2} $colorLabel)")
        case Right(parsed) =>
          parsed.from.occupiedBy match
            case None =>
              error = Some(s"No piece on ${parsed.from.name} for variation move '$san'")
            case Some(piece) =>
              // Create the Move object
              val depth = calculateDepth(anchorIndex, plyIndex, parentVariationRootTob.isDefined)
              val tob = System.nanoTime()
              
              val move = Move(
                piece = piece,
                from = parsed.from,
                to = parsed.to,
                capturedPiece = parsed.to.occupiedBy.filter(_.color != piece.color),
                promotedPieceType = parsed.promotion,
                tob = tob,
                parentTob = currentParentTob,
                halfmoveDistanceFromStart = depth,
                notation = Some(san),
                isVariation = true,
                variationRootTob = variationRootTob.orElse(Some(tob))  // First move is the root
              )
              
              // Set variationRootTob for subsequent moves
              if variationRootTob.isEmpty then
                variationRootTob = Some(tob)
              
              // Add to move history
              moveHistoryManager.addVariationMove(move)
              
              // Actually execute the move on the board (for subsequent move validation)
              executeMove(parsed, piece, board)
              controller.updateAllPossibleMoves()
              
              // Collect any nested variations that branch from THIS move (process later)
              variationsByMoveIndex.get(plyIndex).foreach { nestedVariations =>
                nestedVariations.foreach { nestedVar =>
                  nestedVariationsToProcess += ((nestedVar, plyIndex))
                }
              }
              
              currentParentTob = tob
              isWhiteMove = !isWhiteMove
              plyIndex += 1
    
    // Now process nested variations - board is at end of parent variation
    // We need to restore board to each branch point and replay
    if error.isEmpty && variationRootTob.isDefined then
      nestedVariationsToProcess.foreach { case (nestedVar, branchPlyIndex) =>
        // Restore board to branch point within this variation
        // We need the position BEFORE the move at branchPlyIndex, so we take moves up to (not including) branchPlyIndex
        restoreBoardToBranchPointWithinVariation(
          anchorIndex, 
          parentVariationRootTob, 
          sanMoves.take(branchPlyIndex),  // Moves up to (but not including) the branch point
          variation.startingColor,
          controller, 
          board, 
          moveHistoryManager
        )
        controller.updateAllPossibleMoves()
        
        // Replay the nested variation
        replayVariation(nestedVar, variationRootTob, controller, board, moveHistoryManager)
      }
      
      // Process leading variations (those that branch from the START of this variation)
      variation.leadingVariations.foreach { nestedVar =>
        // Restore board to branch point (start of this variation)
        restoreBoardToBranchPoint(anchorIndex, parentVariationRootTob, controller, board, moveHistoryManager, None)
        controller.updateAllPossibleMoves()
        replayVariation(nestedVar, variationRootTob, controller, board, moveHistoryManager)
      }
    
    // Restore board to state before this variation started
    restoreBoardToBranchPoint(anchorIndex, parentVariationRootTob, controller, board, moveHistoryManager, None)
    controller.updateAllPossibleMoves()
    
    error match
      case Some(msg) => Left(msg)
      case None => Right(())
  }

  /**
   * Build a map from ply index to variations that branch from that move.
   * Ply index is the index into the flattened SAN list.
   * 
   * @param moves The PGNMove list from the variation
   * @param startingMoveNumber The move number where this variation starts
   * @param startingColor The color whose turn starts this variation
   */
  private def buildVariationMap(
    moves: List[PGNMove], 
    startingMoveNumber: Int, 
    startingColor: String
  ): Map[Int, List[PGNVariation]] = {
    val result = scala.collection.mutable.Map[Int, List[PGNVariation]]()
    
    moves.foreach { pgnMove =>
      pgnMove.variations.foreach { variation =>
        // Calculate the ply index where this variation branches from
        (variation.originMoveNumber, variation.originColor) match {
          case (Some(originMoveNo), Some(originColor)) =>
            // Calculate ply index from origin move number and color
            val plyIndex = calculatePlyIndexFromOrigin(
              startingMoveNumber, startingColor, originMoveNo, originColor
            )
            if plyIndex >= 0 then
              result(plyIndex) = result.getOrElse(plyIndex, Nil) :+ variation
          case _ =>
            // If no origin info, skip (shouldn't happen for properly parsed PGN)
            ()
        }
      }
    }
    
    result.toMap
  }
  
  /**
   * Calculate the ply index within a flattened move list given origin move number and color.
   * The ply index is 0-based within the parent variation.
   * 
   * For white-starting variation at move N:
   *   - Move N white = plyIndex 0
   *   - Move N black = plyIndex 1
   *   - Move N+1 white = plyIndex 2
   *   - Move N+1 black = plyIndex 3
   * 
   * For black-starting variation at move N:
   *   - Move N black = plyIndex 0
   *   - Move N+1 white = plyIndex 1
   *   - Move N+1 black = plyIndex 2
   *   - Move N+2 white = plyIndex 3
   */
  private def calculatePlyIndexFromOrigin(
    startingMoveNumber: Int,
    startingColor: String,
    originMoveNumber: Int,
    originColor: String
  ): Int = {
    val moveDiff = originMoveNumber - startingMoveNumber
    
    startingColor.toLowerCase match {
      case "white" =>
        // White start: N white=0, N black=1, N+1 white=2, N+1 black=3
        val base = moveDiff * 2
        if originColor.toLowerCase == "black" then base + 1 else base
        
      case "black" =>
        // Black start: N black=0, N+1 white=1, N+1 black=2, N+2 white=3
        if moveDiff == 0 then
          0  // Same move, must be black
        else
          // For move M (M > N), M white = (M-N)*2 - 1, M black = (M-N)*2
          val base = moveDiff * 2
          if originColor.toLowerCase == "white" then base - 1 else base
          
      case _ => 0
    }
  }

  private def calculateDepth(anchorIndex: Int, plyIndex: Int, @annotation.unused isNested: Boolean): Int =
    // Depth = anchor position + 1 + ply within variation
    val base = if anchorIndex < 0 then 0 else anchorIndex + 1
    base + plyIndex + 1

  private def calculatePlyOffset(
    variation: PGNVariation,
    @annotation.unused parentLine: List[Move],
    moveHistoryManager: MoveHistoryManager,
    parentRootTob: Long
  ): Int = {
    // Calculate where within the parent variation this nested variation branches
    (variation.originMoveNumber, variation.originColor) match
      case (Some(moveNo), Some(color)) =>
        val parentStartMoveNo = moveHistoryManager.getVariationStartingMoveNumber(parentRootTob)
        val parentStartColor = moveHistoryManager.getVariationStartingColor(parentRootTob)
        plyOffset(parentStartMoveNo, parentStartColor, moveNo, color)
      case _ => 0
  }

  private def plyOffset(
    startingMoveNumber: Int,
    startingColor: String,
    targetMoveNumber: Int,
    targetColor: String
  ): Int = {
    val moveDiff = targetMoveNumber - startingMoveNumber
    val base = moveDiff * 2
    (startingColor.toLowerCase, targetColor.toLowerCase) match
      case ("white", "white") => base
      case ("white", "black") => base + 1
      case ("black", "black") => base
      case ("black", "white") => base + 1
      case _                  => base
  }

  private def anchorIndexFromPGN(originMoveNumber: Option[Int], originColor: Option[String]): Int =
    (originMoveNumber, originColor) match
      case (Some(moveNumber), Some(color)) =>
        color.toLowerCase match
          case "white" => (moveNumber - 1) * 2 - 1
          case "black" => (moveNumber - 1) * 2
          case _       => (moveNumber - 1) * 2
      case _ => -1

  private def flattenPGNMoves(moves: List[PGNMove]): List[String] =
    moves.flatMap { move =>
      move.whiteMove.toList ++ move.blackMove.toList
    }.map(_.trim).filter(_.nonEmpty)

  private def restoreBoardToBranchPoint(
    anchorIndex: Int,
    parentVariationRootTob: Option[Long],
    controller: GameController,
    board: Board,
    moveHistoryManager: MoveHistoryManager,
    variation: Option[PGNVariation] = None  // If provided, used to find exact branch point in parent
  ): Unit = {
    board.resetBoard()
    controller.currentPlayer = "white"
    
    parentVariationRootTob match
      case Some(parentRootTob) =>
        // Find the full chain of ancestor variations
        val ancestorChain = collectAncestorVariationChain(parentRootTob, moveHistoryManager)
        
        // Get mainline anchor from the outermost variation
        val outermostRoot = ancestorChain.lastOption.getOrElse(parentRootTob)
        val mainlineAnchor = moveHistoryManager.getMainlineAnchorIndexForVariation(outermostRoot)
        
        // Replay mainline
        val mainlineMoves = moveHistoryManager.getMainlineMoves.take(mainlineAnchor + 1)
        mainlineMoves.foreach(controller.replayMove)
        
        // Replay each ancestor variation in order (oldest to newest)
        // For each ancestor, we need to know where the NEXT ancestor (its child) branches from it
        val reversedChain = ancestorChain.reverse  // oldest to newest
        reversedChain.zipWithIndex.foreach { case (ancestorRootTob, idx) =>
          val ancestorLine = moveHistoryManager.collectVariationLine(ancestorRootTob)
          
          val stopIndex = if ancestorRootTob == parentRootTob then
            // This is the immediate parent - use the current variation to find stop point
            variation match
              case Some(v) =>
                val plyOffset = calculatePlyOffset(v, ancestorLine, moveHistoryManager, ancestorRootTob)
                plyOffset
              case None =>
                ancestorLine.size
          else
            // This is an intermediate ancestor - find where the next (child) variation branches from it
            val childRootTob = reversedChain(idx + 1)  // safe because if we're not at parentRootTob, there's a next one
            val childStopIndex = calculateStopIndexForChildVariation(ancestorRootTob, childRootTob, ancestorLine, moveHistoryManager)
            childStopIndex
          
          ancestorLine.take(stopIndex).foreach(controller.replayMove)
        }
        
        val totalPlies = mainlineAnchor + 1 + {
          val reversedChainCalc = ancestorChain.reverse
          reversedChainCalc.zipWithIndex.foldLeft(0) { case (acc, (rootTob, idx)) =>
            val line = moveHistoryManager.collectVariationLine(rootTob)
            val count = if rootTob == parentRootTob then
              variation.map(v => calculatePlyOffset(v, line, moveHistoryManager, rootTob)).getOrElse(line.size)
            else
              val childRootTob = reversedChainCalc(idx + 1)
              calculateStopIndexForChildVariation(rootTob, childRootTob, line, moveHistoryManager)
            acc + count
          }
        }
        controller.currentPlayer = if totalPlies % 2 == 0 then "white" else "black"
        
      case None =>
        // Just replay mainline to anchor point
        if anchorIndex >= 0 then
          val mainlineMoves = moveHistoryManager.getMainlineMoves.take(anchorIndex + 1)
          mainlineMoves.foreach(controller.replayMove)
          controller.currentPlayer = if (anchorIndex + 1) % 2 == 0 then "white" else "black"
  }
  
  /**
   * Calculate where a child variation branches from its parent variation.
   * Returns the ply index (0-based) within the parent where we should stop replay.
   * This is the number of moves to replay (we need to replay UP TO AND INCLUDING the parent of the branch point).
   */
  private def calculateStopIndexForChildVariation(
    @annotation.unused parentRootTob: Long,
    childRootTob: Long,
    parentLine: List[Move],
    moveHistoryManager: MoveHistoryManager
  ): Int = {
    // getParentVariationBranchIndex returns the index of the move that is the PARENT of the child variation's root
    // (i.e., the last move that was played before the branch)
    // We want to replay all moves UP TO AND INCLUDING that parent, so we add 1
    val branchIdx = moveHistoryManager.getParentVariationBranchIndex(childRootTob)
    if branchIdx >= 0 then branchIdx + 1 else parentLine.size
  }
  
  /**
   * Collect the chain of ancestor variation root TOBs from innermost to outermost.
   * Returns List(parentRoot, grandparentRoot, ...) until we reach a root-level variation.
   */
  private def collectAncestorVariationChain(
    variationRootTob: Long,
    moveHistoryManager: MoveHistoryManager
  ): List[Long] = {
    val chain = scala.collection.mutable.ListBuffer[Long]()
    var current = variationRootTob
    var continue = true
    
    while continue do
      chain += current
      moveHistoryManager.getParentVariationRootTob(current) match
        case Some(parentRoot) =>
          current = parentRoot
        case None =>
          continue = false
    
    chain.toList
  }

  /**
   * Restore board to a specific point within a variation by:
   * 1. First restoring to the variation's branch point
   * 2. Then replaying the given SAN moves within the variation
   */
  private def restoreBoardToBranchPointWithinVariation(
    anchorIndex: Int,
    parentVariationRootTob: Option[Long],
    sanMovesToReplay: List[String],
    startingColor: String,
    controller: GameController,
    board: Board,
    moveHistoryManager: MoveHistoryManager
  ): Unit = {
    // First restore to the variation's branch point (replay entire parent variation for this case)
    restoreBoardToBranchPoint(anchorIndex, parentVariationRootTob, controller, board, moveHistoryManager, None)
    
    // Then replay the moves within this variation up to the branch point
    var isWhiteMove = startingColor.toLowerCase == "white"
    sanMovesToReplay.foreach { san =>
      SANParser.parseMove(san, isWhiteMove, controller, board) match
        case Right(parsed) =>
          parsed.from.occupiedBy.foreach { piece =>
            executeMove(parsed, piece, board)
            controller.updateAllPossibleMoves()
          }
        case Left(_) => ()
      isWhiteMove = !isWhiteMove
    }
  }

  /**
   * Execute a move on the board WITHOUT recording it to move history.
   * This just updates the board state for subsequent move validation.
   */
  private def executeMove(
    parsed: SANParser.ParsedMove,
    piece: ChessPiece,
    board: Board
  ): Unit = {
    val from = parsed.from
    val to = parsed.to
    
    // Check for castling
    val isCastling = piece.pieceType == chess.types.PieceType.King && 
      math.abs(from.getCoordinates._1 - to.getCoordinates._1) == 2
    
    if isCastling then
      executeCastlingMove(piece, from, to, board)
    else
      // Check for en passant capture (pawn moving diagonally to empty square)
      val isEnPassant = piece.pieceType == chess.types.PieceType.Pawn &&
        from.getCoordinates._1 != to.getCoordinates._1 &&
        to.occupiedBy.isEmpty
      
      if isEnPassant then
        // Remove the captured pawn
        val (_, fromRow) = from.getCoordinates
        val (toCol, _) = to.getCoordinates
        board.squares(fromRow)(toCol).occupiedBy.foreach(_.capture())
        board.squares(fromRow)(toCol).occupiedBy = None
      
      // Handle capture
      to.occupiedBy.foreach(_.capture())
      
      // Move the piece
      from.occupiedBy = None
      piece.moveTo(to)
      to.occupiedBy = Some(piece)
      piece.hasMoved = true
      
      // Handle promotion
      parsed.promotion.foreach { promotionType =>
        piece match
          case pawn: PawnPiece =>
            val promotedPiece = createPromotedPiece(promotionType, piece.color, to, board)
            to.occupiedBy = Some(promotedPiece)
          case _ => ()
      }
  }
  
  private def executeCastlingMove(
    king: ChessPiece,
    from: Square,
    to: Square,
    board: Board
  ): Unit = {
    val (fromCol, fromRow) = from.getCoordinates
    val (toCol, _) = to.getCoordinates
    
    // Move king
    from.occupiedBy = None
    king.moveTo(to)
    to.occupiedBy = Some(king)
    king.hasMoved = true
    
    // Move rook
    if toCol - fromCol == 2 then
      // King-side castling
      val rookFrom = board.squares(fromRow)(7)
      val rookTo = board.squares(fromRow)(5)
      rookFrom.occupiedBy.foreach { rook =>
        rookFrom.occupiedBy = None
        rook.moveTo(rookTo)
        rookTo.occupiedBy = Some(rook)
        rook.hasMoved = true
      }
    else if toCol - fromCol == -2 then
      // Queen-side castling
      val rookFrom = board.squares(fromRow)(0)
      val rookTo = board.squares(fromRow)(3)
      rookFrom.occupiedBy.foreach { rook =>
        rookFrom.occupiedBy = None
        rook.moveTo(rookTo)
        rookTo.occupiedBy = Some(rook)
        rook.hasMoved = true
      }
  }
  
  private def createPromotedPiece(
    pieceType: chess.types.PieceType,
    color: String,
    square: Square,
    board: Board
  ): ChessPiece = {
    import chess.pieces.*
    pieceType match
      case chess.types.PieceType.Queen => Queen(color, square, board)
      case chess.types.PieceType.Rook => Rook(color, square, board)
      case chess.types.PieceType.Bishop => Bishop(color, square, board)
      case chess.types.PieceType.Knight => Knight(color, square, board)
      case _ => Queen(color, square, board) // Default to queen
  }

  // Board state saving/restoration for variation replay
  private case class BoardState(
    squares: Array[Array[Option[ChessPiece]]],
    currentPlayer: String
  )

  private def saveBoardState(board: Board): BoardState = {
    val squares = board.squares.map(_.map(_.occupiedBy))
    BoardState(squares, "white") // Will be set properly during restore
  }

  private def restoreBoardState(board: Board, @annotation.unused state: BoardState): Unit = {
    board.resetBoard()
    // Note: Full state restoration would require copying pieces back
    // For now, we rely on the fact that we restore from scratch after each variation
  }

end PGNVariationReplayer
