package chess.analysis

import chess.controllers.Move
import chess.types.PieceType
import scala.concurrent.{Future, ExecutionContext}

/**
 * Service for analyzing chess games and classifying moves
 */
class GameAnalyzer(private val stockfishEngine: StockfishEngine)(implicit ec: ExecutionContext) {
  
  /**
   * Get access to the underlying engine for direct analysis
   */
  def engine: StockfishEngine = stockfishEngine
  
  /**
   * Stop the engine and clean up resources
   */
  def stop(): Unit = {
    stockfishEngine.stop()
  }
  
  /** Format evaluation for console output */
  private def formatEval(eval: PositionEvaluation): String = eval match {
    case PositionEvaluation.Centipawns(cp) =>
      val value = cp / 100.0
      if (value >= 0) f"+$value%.2f" else f"$value%.2f"
    case PositionEvaluation.MateIn(moves) =>
      if (moves > 0) s"M$moves" else s"M${-moves}"
  }
  
  /**
   * Analyze a single move and classify it
   * 
   * @param fenBefore FEN position before the move
   * @param fenAfter FEN position after the move (not used for analysis, only for context)
   * @param move The move that was played
   * @param isWhiteMove Whether white made this move
   * @param depth Analysis depth for Stockfish
   * @return Future containing move classification and evaluations
   */
  def analyzeMove(
    fenBefore: String,
    fenAfter: String,
    move: Move,
    isWhiteMove: Boolean,
    depth: Int = 22
  ): Future[MoveAnalysis] = {
    
    // Only analyze position BEFORE the move to get:
    // 1. Best move available
    // 2. Evaluation before the move
    // We'll get evalAfter from the NEXT move's evalBefore
    analyzePositionWithFuture(fenBefore, depth, multiPV = 1).map { beforeResult =>
      val evalBefore = beforeResult.evaluation
      
      // For now, we'll use evalBefore as evalAfter too
      // The sequential analysis in analyzeGame will provide the real evalAfter
      val evalAfter = evalBefore
      
      // Detect material sacrifice
      val materialSacrificed = detectMaterialSacrifice(move)
      
      // Get the best move from analysis
      val bestMove = beforeResult.bestMove.getOrElse("")
      val actualMove = move.notation.getOrElse("")
      
      // Check if the played move matches the best move
      val isBestMove = bestMove.nonEmpty && actualMove.nonEmpty && 
                       (bestMove == actualMove || bestMove.replace("+", "").replace("#", "") == actualMove.replace("+", "").replace("#", ""))
      
      // Simple classification based on whether it's the best move
      val classification = if (isBestMove) {
        MoveClassification.Best
      } else if (materialSacrificed) {
        // Check if evaluation is winning despite sacrifice
        val isWinning = beforeResult.evaluation match {
          case PositionEvaluation.Centipawns(cp) => if (isWhiteMove) cp > 200 else cp < -200
          case PositionEvaluation.MateIn(_) => true
        }
        if (isWinning) MoveClassification.Brilliant else MoveClassification.Good
      } else {
        // For now, classify as Good if not best but close
        MoveClassification.Good
      }
      
      MoveAnalysis(
        move = move,
        classification = classification,
        evalBefore = evalBefore,
        evalAfter = evalAfter,
        bestMove = Some(bestMove),
        bestMoveEval = beforeResult.evaluation,
        isWhiteMove = isWhiteMove
      )
    }
  }
  
  /**
   * Wrapper to convert callback-based engine to Future
   */
  private def analyzePositionWithFuture(
    fen: String,
    depth: Int,
    multiPV: Int
  ): Future[AnalysisResult] = {
    Future {
      var result: Option[AnalysisResult] = None
      var partialResult: Option[AnalysisResult] = None  // Store partial results
      val lock = new Object()
      var completed = false
      
      // Set up callback
      val callback: StockfishEngine#EngineAnalysis => Unit = analysis => {
        lock.synchronized {
          // Convert engine analysis to our format
          val evaluation = if (analysis.mate.isDefined) {
            PositionEvaluation.MateIn(analysis.mate.get)
          } else {
            PositionEvaluation.Centipawns(analysis.score.getOrElse(0).toDouble)
          }
          
          // Extract top moves and their evaluations from all variations
          val sortedVariations = analysis.variations.sortBy(_.multiPvIndex)
          val topMoves = sortedVariations.flatMap(_.pv.headOption).distinct
          
          // Extract evaluations for each variation
          val topMoveEvals: List[PositionEvaluation] = sortedVariations.map { v =>
            if (v.mate.isDefined) {
              PositionEvaluation.MateIn(v.mate.get)
            } else {
              PositionEvaluation.Centipawns(v.score.getOrElse(0).toDouble)
            }
          }
          
          // Use PV first move as fallback bestMove if not provided yet
          val effectiveBestMove = analysis.bestMove.orElse(analysis.pv.headOption)
          
          val currentResult = AnalysisResult(
            evaluation = evaluation,
            bestMove = effectiveBestMove,
            principalVariation = analysis.pv,
            topMoves = topMoves,
            topMoveEvals = topMoveEvals
          )
          
          // Always save partial result (in case of timeout)
          partialResult = Some(currentResult)
          
          // Mark as complete only when we have official bestMove from "bestmove" line
          if (analysis.bestMove.isDefined) {
            result = Some(currentResult)
            completed = true
            lock.notify()
          }
        }
      }
      
      stockfishEngine.addAnalysisCallback(callback)
      
      try {
        // Wait a bit to ensure engine is ready from previous analysis
        Thread.sleep(50)
        stockfishEngine.analyzePosition(fen, depth, multiPV)
        
        // Wait for analysis to complete
        // Use shorter timeout (30s) but if we have partial results, use them
        lock.synchronized {
          if (!completed) {
            lock.wait(30000) // 30 second timeout per position
          }
        }
        
        // Priority: complete result > partial result > neutral fallback
        result.orElse {
          partialResult match {
            case Some(partial) =>
              println(s"  INFO: Using partial analysis result (no bestmove received)")
              Some(partial)
            case None =>
              println(s"  WARNING: Analysis timeout for position, using neutral evaluation")
              None
          }
        }.getOrElse {
          AnalysisResult(
            PositionEvaluation.Centipawns(0),
            None,
            List.empty,
            List.empty
          )
        }
      } finally {
        stockfishEngine.removeAnalysisCallback(callback)
      }
    }
  }
  
  /** Standard piece values in centipawns */
  private val pieceValues: Map[PieceType, Int] = Map(
    PieceType.Pawn -> 100,
    PieceType.Knight -> 300,
    PieceType.Bishop -> 320,
    PieceType.Rook -> 500,
    PieceType.Queen -> 900,
    PieceType.King -> 0  // King can't be captured
  )
  
  // Piece representation for FEN analysis
  private case class FenPiece(pieceType: Char, isWhite: Boolean, row: Int, col: Int) {
    def value: Int = pieceType.toLower match {
      case 'p' => 100
      case 'n' => 300
      case 'b' => 320
      case 'r' => 500
      case 'q' => 900
      case 'k' => 10000  // King - very high value to detect checks
      case _ => 0
    }
  }
  
  /**
   * Parse FEN board to get all pieces with their positions.
   */
  private def parseFenBoard(fen: String): List[FenPiece] = {
    val boardPart = fen.split(" ").headOption.getOrElse("")
    val rows = boardPart.split("/")
    
    rows.zipWithIndex.flatMap { case (rowStr, rowIdx) =>
      var col = 0
      rowStr.flatMap { c =>
        if (c.isDigit) {
          col += c.asDigit
          None
        } else {
          val piece = FenPiece(c, c.isUpper, rowIdx, col)
          col += 1
          Some(piece)
        }
      }
    }.toList
  }
  
  /**
   * Check if a piece at (fromRow, fromCol) can attack square (toRow, toCol).
   * This is a simplified attack check (doesn't consider blocking pieces for sliding pieces).
   */
  private def canAttack(piece: FenPiece, toRow: Int, toCol: Int, allPieces: List[FenPiece]): Boolean = {
    val dRow = toRow - piece.row
    val dCol = toCol - piece.col
    
    piece.pieceType.toLower match {
      case 'p' =>
        // Pawns attack diagonally
        val direction = if (piece.isWhite) -1 else 1  // White moves up (decreasing row), black down
        dRow == direction && math.abs(dCol) == 1
        
      case 'n' =>
        // Knight moves in L-shape
        (math.abs(dRow) == 2 && math.abs(dCol) == 1) || (math.abs(dRow) == 1 && math.abs(dCol) == 2)
        
      case 'b' =>
        // Bishop moves diagonally
        math.abs(dRow) == math.abs(dCol) && dRow != 0 && !isBlocked(piece.row, piece.col, toRow, toCol, allPieces)
        
      case 'r' =>
        // Rook moves horizontally or vertically
        (dRow == 0 || dCol == 0) && (dRow != 0 || dCol != 0) && !isBlocked(piece.row, piece.col, toRow, toCol, allPieces)
        
      case 'q' =>
        // Queen moves like bishop or rook
        val isDiagonal = math.abs(dRow) == math.abs(dCol) && dRow != 0
        val isStraight = (dRow == 0 || dCol == 0) && (dRow != 0 || dCol != 0)
        (isDiagonal || isStraight) && !isBlocked(piece.row, piece.col, toRow, toCol, allPieces)
        
      case 'k' =>
        // King moves one square in any direction
        math.abs(dRow) <= 1 && math.abs(dCol) <= 1 && (dRow != 0 || dCol != 0)
        
      case _ => false
    }
  }
  
  /**
   * Check if path between two squares is blocked by any piece.
   */
  private def isBlocked(fromRow: Int, fromCol: Int, toRow: Int, toCol: Int, allPieces: List[FenPiece]): Boolean = {
    val dRow = Integer.signum(toRow - fromRow)
    val dCol = Integer.signum(toCol - fromCol)
    
    var row = fromRow + dRow
    var col = fromCol + dCol
    
    while (row != toRow || col != toCol) {
      if (allPieces.exists(p => p.row == row && p.col == col)) {
        return true
      }
      row += dRow
      col += dCol
    }
    false
  }
  
  /**
   * Detect objective sacrifice from FEN position AFTER the move.
   * 
   * A sacrifice exists when:
   * 1. The MOVED piece is attacked by a lower-value enemy piece (direct sacrifice)
   * 2. OR one of our OTHER pieces is attacked by a lower-value enemy piece (indirect sacrifice)
   * 3. AND the counter-threat (what our moved piece attacks) doesn't compensate
   * 
   * @param fenAfter FEN position after the move
   * @param isWhiteMove Whether white just moved
   * @param movedPieceSquare The square where the moved piece landed (for counter-threat check)
   * @return Material at risk (potential sacrifice value), 0 if no sacrifice
   */
  /**
   * Detect objective sacrifice from FEN position AFTER the move.
   * 
   * A sacrifice exists when:
   * 1. The MOVED piece is attacked by a lower-value enemy piece (direct sacrifice)
   * 2. OR one of our OTHER pieces is attacked by a lower-value enemy piece (indirect sacrifice)
   * 3. AND the counter-threat (what our moved piece attacks) doesn't compensate
   * 
   * Returns: (sacrificeValue, captureIsLegal)
   * - sacrificeValue: centipawns of material at risk
   * - captureIsLegal: true if opponent can legally capture the sacrificed material
   *                   (false if check prevents capture of a hanging piece)
   * 
   * @param fenAfter FEN position after the move
   * @param isWhiteMove Whether white just moved
   * @param movedPieceSquare The square where the moved piece landed (for counter-threat check)
   * @return Tuple of (sacrificeValue, captureIsLegal, isIndirectSacrifice)
   */
  private def detectObjectiveSacrifice(
    fenAfter: String,
    isWhiteMove: Boolean,
    movedPieceSquare: Option[(Int, Int)] = None
  ): (Int, Boolean, Boolean) = {  // Returns (sacrificeValue, captureIsLegal, isIndirectSacrifice)
    val pieces = parseFenBoard(fenAfter)
    val ourPieces = pieces.filter(_.isWhite == isWhiteMove)
    val enemyPieces = pieces.filter(_.isWhite != isWhiteMove)
    
    // Find the moved piece (if square provided)
    val movedPiece = movedPieceSquare.flatMap { case (row, col) =>
      ourPieces.find(p => p.row == row && p.col == col)
    }
    
    // Find if we're giving check and if check can be blocked by capturing our piece
    val (givingCheck, checkCanBeCaptured) = movedPiece.map { mp =>
      val threatenedPieces = enemyPieces.filter(ep => canAttack(mp, ep.row, ep.col, pieces))
      val isCheck = threatenedPieces.exists(_.pieceType.toLower == 'k')
      
      if (isCheck) {
        // Check if any enemy piece can capture the checking piece (our moved piece)
        val canCapture = enemyPieces.exists(ep => canAttack(ep, mp.row, mp.col, pieces))
        (true, canCapture)
      } else {
        (false, false)
      }
    }.getOrElse((false, false))
    
    // Calculate counter-threat: what is the moved piece attacking?
    // IMPORTANT: If check can be blocked by capturing our piece, it's NOT a real counter-threat
    // because our piece will be taken!
    val counterThreatValue = movedPiece.map { mp =>
      val threatenedPieces = enemyPieces.filter(ep => canAttack(mp, ep.row, ep.col, pieces))
      
      if (givingCheck && !checkCanBeCaptured) {
        // Check that can't be blocked by capture - huge advantage!
        10000
      } else if (givingCheck && checkCanBeCaptured) {
        // Check but our piece can be captured - this is a SACRIFICE, not compensation
        // The check is part of the sacrifice (forcing move)
        0
      } else {
        // No check - just count material threats
        threatenedPieces.map(_.value).maxOption.getOrElse(0)
      }
    }.getOrElse(0)
    
    // FIRST: Check if the MOVED piece itself is sacrificed (most common sacrifice type)
    val movedPieceSacrifice = movedPiece.map { mp =>
      if (mp.pieceType.toLower == 'k') {
        0  // King can't be sacrificed
      } else {
        val attackers = enemyPieces.filter(ep => canAttack(ep, mp.row, mp.col, pieces))
        
        if (attackers.nonEmpty) {
          val lowestAttackerValue = attackers.map(_.value).min
          val defenders = ourPieces.filter(op => op != mp && canAttack(op, mp.row, mp.col, pieces))
          
          val potentialLoss = if (defenders.isEmpty) {
            // Moved piece is undefended and attacked - full value at risk
            mp.value
          } else if (lowestAttackerValue < mp.value) {
            // Moved piece is defended but attacker is cheaper
            mp.value - lowestAttackerValue
          } else {
            0  // Defended by piece of equal or lower value
          }
          
          // Subtract counter-threat value (but if check can be captured, counter = 0)
          math.max(0, potentialLoss - counterThreatValue)
        } else {
          0  // Not attacked
        }
      }
    }.getOrElse(0)
    
    // SECOND: Check if any OTHER piece is now hanging (indirect sacrifice)
    var maxOtherPieceSacrifice = 0
    
    for (ourPiece <- ourPieces if ourPiece.value > 0 && ourPiece.pieceType.toLower != 'k' && !movedPiece.contains(ourPiece)) {
      val attackers = enemyPieces.filter(ep => canAttack(ep, ourPiece.row, ourPiece.col, pieces))
      
      if (attackers.nonEmpty) {
        val lowestAttackerValue = attackers.map(_.value).min
        val defenders = ourPieces.filter(op => op != ourPiece && canAttack(op, ourPiece.row, ourPiece.col, pieces))
        
        val potentialLoss = if (defenders.isEmpty) {
          ourPiece.value
        } else if (lowestAttackerValue < ourPiece.value) {
          ourPiece.value - lowestAttackerValue
        } else {
          0
        }
        
        // For other pieces, counter-threat doesn't directly apply
        if (potentialLoss > maxOtherPieceSacrifice) {
          maxOtherPieceSacrifice = potentialLoss
        }
      }
    }
    
    // Return the larger sacrifice value and whether capture is legal
    val totalSacrifice = math.max(movedPieceSacrifice, maxOtherPieceSacrifice)
    
    // Determine if sacrifice is from moved piece or indirect (other piece exposed)
    val sacrificeIsFromMovedPiece = movedPieceSacrifice >= maxOtherPieceSacrifice && movedPieceSacrifice > 0
    val isIndirectSacrifice = !sacrificeIsFromMovedPiece && maxOtherPieceSacrifice > 0
    
    // Determine if the sacrifice can be legally captured:
    // - If we're giving check, only the MOVED piece can be captured (if it's the one giving check)
    // - Other hanging pieces CANNOT be captured during check - not a real sacrifice!
    val captureIsLegal = if (givingCheck) {
      // During check, only the checking piece can be captured
      // If sacrifice comes from OTHER piece (not the one giving check), opponent can't take it!
      sacrificeIsFromMovedPiece && checkCanBeCaptured
    } else {
      // No check - any attacked piece can be captured
      true
    }
    
    // Only return if significant (>= 200 centipawns = 2 pawns)
    if (totalSacrifice >= 200) (totalSacrifice, captureIsLegal, isIndirectSacrifice) else (0, true, false)
  }
  
  /**
   * Check if a capture is a trade (equal value exchange).
   * 
   * Trade occurs when:
   * - Knight takes Knight or Bishop
   * - Bishop takes Knight or Bishop  
   * - Queen takes Queen
   * - Pawn takes Pawn
   * 
   * Note: We assume the captured piece was protected (most pieces are protected in real games).
   * If Stockfish evaluation stays stable, the piece was likely protected.
   */
  private def isEqualTrade(movingPieceType: PieceType, capturedPieceType: Option[PieceType]): Boolean = {
    capturedPieceType match {
      case Some(captured) =>
        (movingPieceType, captured) match {
          // Knight takes Knight or Bishop = trade
          case (PieceType.Knight, PieceType.Knight) => true
          case (PieceType.Knight, PieceType.Bishop) => true
          // Bishop takes Knight or Bishop = trade
          case (PieceType.Bishop, PieceType.Knight) => true
          case (PieceType.Bishop, PieceType.Bishop) => true
          // Queen takes Queen = trade
          case (PieceType.Queen, PieceType.Queen) => true
          // Pawn takes Pawn = trade
          case (PieceType.Pawn, PieceType.Pawn) => true
          // Everything else is not a trade
          case _ => false
        }
      case None => false  // No capture = not a trade
    }
  }
  
  /**
   * Detect if a move is a material sacrifice.
   * 
   * A sacrifice occurs when:
   * 1. You capture with a MORE VALUABLE piece than what you captured
   *    (e.g., Queen takes Pawn, Rook takes Pawn, Knight takes Pawn)
   * 2. The target square is PROTECTED (defenders >= attackers means opponent can recapture)
   * 3. It's NOT an equal trade (Knight×Knight, Bishop×Bishop, etc.)
   * 4. The evaluation AFTER the move is still good for the player
   *    (Stockfish confirms it's a good sacrifice, not a blunder)
   * 
   * Trade logic:
   * - Knight/Bishop takes protected Knight/Bishop = trade (equal value exchange)
   * - Queen takes protected Queen = trade
   * - Pawn takes protected Pawn = trade
   * - Square is "protected" if defenders >= attackers
   * 
   * @return SacrificeInfo with material sacrificed and whether it's just a trade
   */
  private def detectSacrificeInfo(move: Move, evalBefore: PositionEvaluation, evalAfter: PositionEvaluation, isWhiteMove: Boolean): SacrificeInfo = {
    // Note: evalBefore is kept for API compatibility but not used in current logic
    val _ = evalBefore  // Suppress unused warning
    
    // CRITICAL: Sacrifice can only happen on a CAPTURE
    // If no piece was captured, this is not a sacrifice
    if (move.capturedPiece.isEmpty) {
      return SacrificeInfo(materialSacrificed = 0, isTrade = false)
    }
    
    val movingPieceType = move.piece.pieceType
    val capturedPieceType = move.capturedPiece.map(_.pieceType)
    val movingPieceValue = pieceValues.getOrElse(movingPieceType, 0)
    val capturedPieceValue = capturedPieceType.flatMap(pieceValues.get).getOrElse(0)
    
    // Get defender/attacker info from the Move object
    val defenders = move.targetSquareDefenders  // Own pieces defending
    val attackers = move.targetSquareAttackers  // Opponent pieces attacking (can recapture)
    
    // Square is protected if opponent has attackers that can recapture
    // They need at least one attacker to be able to recapture
    val squareIsProtected = attackers > 0
    
    // Check if this is an equal trade on a protected square
    // Trade = equal value pieces exchanged AND square is protected (opponent will recapture)
    if (isEqualTrade(movingPieceType, capturedPieceType) && squareIsProtected) {
      return SacrificeInfo(materialSacrificed = 0, isTrade = true)
    }
    
    // Material difference: positive = we used a more valuable piece to capture
    val materialDiff = movingPieceValue - capturedPieceValue
    
    // DEBUG: Print square protection info for potential sacrifices
    if (materialDiff >= 200) {
      println(s"    SACRIFICE CHECK: ${movingPieceType}(${movingPieceValue}) x ${capturedPieceType.getOrElse("None")}(${capturedPieceValue}), diff=${materialDiff}, defenders=${defenders}, attackers=${attackers}, protected=${squareIsProtected}")
    }
    
    // Only consider it a potential sacrifice if:
    // 1. We captured with a more valuable piece (materialDiff >= 200)
    // 2. The square is protected (opponent can recapture our more valuable piece)
    if (materialDiff >= 200 && squareIsProtected) {
      // Calculate eval from player's perspective (in pawns)
      val evalAfterPlayer = if (isWhiteMove) evalAfter.toNumeric else -evalAfter.toNumeric
      
      // Check if position is still good after the "sacrifice"
      // If Stockfish says position is still >= -0.5 from player's view, it's a real sacrifice
      val positionStillGood = evalAfterPlayer >= -0.5
      
      if (positionStillGood) {
        // Real sacrifice - position stays good despite apparent material loss on protected square
        SacrificeInfo(materialSacrificed = materialDiff, isTrade = false)
      } else {
        // Bad sacrifice (blunder) - treat as failed sacrifice attempt
        SacrificeInfo(materialSacrificed = materialDiff, isTrade = true)
      }
    } else if (materialDiff >= 200 && !squareIsProtected) {
      // Capturing with more valuable piece BUT square is not protected
      // This is just winning material, not a sacrifice
      SacrificeInfo(materialSacrificed = 0, isTrade = false)
    } else {
      // No significant material imbalance - not a sacrifice
      SacrificeInfo(materialSacrificed = 0, isTrade = false)
    }
  }
  
  /**
   * Detect if a move sacrifices significant material (legacy method for compatibility)
   */
  private def detectMaterialSacrifice(move: Move): Boolean = {
    val movingPieceValue = pieceValues.getOrElse(move.piece.pieceType, 0)
    val capturedPieceValue = move.capturedPiece.flatMap(p => pieceValues.get(p.pieceType)).getOrElse(0)
    movingPieceValue - capturedPieceValue >= 200
  }
  
  /**
   * Analyze an entire game (mainline only, no variations)
   * Optimized: Analyzes each position once and uses results for both evalAfter and next evalBefore
   * 
   * @param moves List of moves in the game
   * @param fens List of FEN positions (one for each move, plus starting position)
   * @param depth Analysis depth
   * @return Future with list of move analyses
   */
  def analyzeGame(
    moves: List[Move],
    fens: List[String],
    depth: Int = 15
  ): Future[List[MoveAnalysis]] = {
    
    if (fens.length != moves.length + 1) {
      return Future.failed(new IllegalArgumentException(
        s"FEN count (${fens.length}) must be moves count + 1 (${moves.length + 1})"
      ))
    }
    
    println(s"\n=== Analysoidaan ${moves.length} siirtoa (syvyys: $depth) ===\n")
    
    // First, analyze all positions to get evaluations and best moves
    def analyzeAllPositions(index: Int, results: List[AnalysisResult]): Future[List[AnalysisResult]] = {
      if (index >= fens.length) {
        Future.successful(results.reverse)
      } else {
        analyzePositionWithFuture(fens(index), depth, multiPV = 1).flatMap { result =>
          analyzeAllPositions(index + 1, result :: results)
        }
      }
    }
    
    // Then classify each move based on the evaluations
    analyzeAllPositions(0, List.empty).map { allResults =>
      val analyses = moves.zipWithIndex.map { case (move, index) =>
        val isWhiteMove = index % 2 == 0
        
        // CLASSIFICATION LOGIC:
        // For move at index i:
        // - Position BEFORE the move = allResults(i) = fens(i)
        // - Position AFTER the move = allResults(i+1) = fens(i+1)
        // 
        // Example: Move 10 (Nxd5), index=9
        // - evalBefore = allResults(9) = position after move 9 (exd5) = BEFORE Nxd5
        // - evalAfter = allResults(10) = position after move 10 (Nxd5) = AFTER Nxd5
        // - bestMove = allResults(9).bestMove = Stockfish's best move in position before Nxd5
        
        // evalBefore = evaluation of position BEFORE this move was played
        val evalBefore = allResults(index).evaluation
        
        // evalAfter = evaluation AFTER this move
        val actualMove = move.notation.getOrElse("")
        val evalAfter = if (actualMove.endsWith("#")) {
          // Checkmate delivered - MateIn(0) means the game is over NOW
          // The sign doesn't matter for MateIn(0) since it's a terminal state
          PositionEvaluation.MateIn(0)  // Checkmate - game over
        } else if (index + 1 < allResults.length) {
          allResults(index + 1).evaluation
        } else {
          allResults(index).evaluation  // Fallback for non-mate last moves
        }
        
        // bestMove = Stockfish's recommendation from the position BEFORE the move
        val bestMove = allResults(index).bestMove.getOrElse("")
        
        // Create UCI notation from the Move object for accurate comparison with Stockfish
        // UCI format: e2e4, g1f3, e7e8q (with promotion)
        val actualMoveUCI = {
          val fromSquare = move.from.name.toLowerCase
          val toSquare = move.to.name.toLowerCase
          val promotion = move.promotedPieceType.map {
            case PieceType.Queen => "q"
            case PieceType.Rook => "r"
            case PieceType.Bishop => "b"
            case PieceType.Knight => "n"
            case _ => ""
          }.getOrElse("")
          s"$fromSquare$toSquare$promotion"
        }
        
        // bestMoveEval should be the BEST outcome possible from evalBefore position
        val bestMoveEval = allResults(index).evaluation
        
        // Get second best move evaluation for Great/Brilliant detection
        val topMoveEvals = allResults(index).topMoveEvals
        val secondBestEval: Option[PositionEvaluation] = topMoveEvals.lift(1)
        
        // Detect material sacrifice with detailed info
        val directSacrificeInfo = detectSacrificeInfo(move, evalBefore, evalAfter, isWhiteMove)
        
        // Get moved piece's destination square for objective sacrifice detection
        // Convert algebraic notation (e.g., "e4") to FEN coordinates (row, col)
        val movedPieceSquare: Option[(Int, Int)] = {
          val toSquare = move.to.name.toLowerCase
          if (toSquare.length == 2) {
            val col = toSquare.charAt(0) - 'a'  // a=0, b=1, ..., h=7
            val row = 8 - toSquare.charAt(1).asDigit  // 8=0, 7=1, ..., 1=7
            Some((row, col))
          } else None
        }
        
        // Check for OBJECTIVE sacrifice: analyze board state AFTER the move
        // This is independent of what opponent actually does
        // Returns: (sacrificeValue, captureIsLegal, isIndirectSacrifice)
        val (objectiveSacrificeValue, objSacCaptureIsLegal, objSacIsIndirect) = detectObjectiveSacrifice(
          fens(index + 1),  // FEN after move
          isWhiteMove,
          movedPieceSquare
        )
        
        // Only count as sacrifice if position is good (Stockfish confirms it's a real sacrifice, not blunder)
        val evalAfterPlayer = if (isWhiteMove) evalAfter.toNumeric else -evalAfter.toNumeric
        val objectiveSacrificeIfGood = if (objectiveSacrificeValue >= 200 && evalAfterPlayer >= 0.5) {
          objectiveSacrificeValue
        } else {
          0
        }
        
        // Combine direct sacrifice and objective sacrifice
        // Priority: direct > objective (use the highest value)
        val sacrificeInfo = if (directSacrificeInfo.materialSacrificed > 0) {
          // Direct sacrifice: captured with more valuable piece on protected square
          Some(directSacrificeInfo)
        } else if (objectiveSacrificeIfGood >= 300) {
          // Objective sacrifice: piece is hanging/attackable after the move
          // Include captureIsLegal and isIndirectSacrifice info for Brilliant move check
          Some(SacrificeInfo(
            materialSacrificed = objectiveSacrificeIfGood, 
            isTrade = false, 
            captureIsLegal = objSacCaptureIsLegal,
            isIndirectSacrifice = objSacIsIndirect
          ))
        } else {
          Some(directSacrificeInfo)
        }
        
        // Classify using the proper evaluator (using UCI for accurate comparison)
        val classification = MoveEvaluator.classifyMove(
          evalBefore,
          evalAfter,
          bestMoveEval,
          actualMoveUCI,  // Use UCI notation for comparison
          bestMove,       // Stockfish also uses UCI notation
          isWhiteMove,
          materialSacrificed = false,  // Legacy param, not used anymore
          secondBestEval = secondBestEval,
          sacrificeInfo = sacrificeInfo
        )
        
        // For DISPLAY:
        // - Last move (checkmate): show "1-0" or "0-1"
        // - Other moves: show evaluation AFTER opponent's response
        val evalAfterStr = if (index == moves.length - 1 && actualMove.endsWith("#")) {
          // Checkmate - show game result
          if (isWhiteMove) "1-0" else "0-1"
        } else {
          formatEval(evalAfter)
        }
        val moveColor = if (isWhiteMove) "White" else "Black"
        
        println(f"${index + 1}%2d. $moveColor%-5s ${actualMove}%-8s | Eval: $evalAfterStr%-8s | ${classification.description}")
        
        // Get top moves from the position analysis (before the move was played)
        val topMoves = allResults(index).topMoves
        val pv = allResults(index).principalVariation
        
        MoveAnalysis(
          move = move,
          classification = classification,
          evalBefore = evalBefore,
          evalAfter = evalAfter,
          bestMove = Some(bestMove),
          bestMoveEval = evalBefore,
          isWhiteMove = isWhiteMove,
          topMoves = topMoves,
          principalVariation = pv
        )
      }
      
      // Print summary
      println(s"\n=== Analyysi valmis: ${analyses.length} siirtoa ===\n")
      
      analyses
    }
  }
  
  /**
   * Analyze an entire game with progress callback
   * Same as analyzeGame but reports progress after each position is analyzed
   * 
   * @param moves List of moves in the game
   * @param fens List of FEN positions (one for each move, plus starting position)
   * @param depth Analysis depth
   * @param progressCallback Called with current move number after each analysis
   * @return Future with list of move analyses
   */
  def analyzeGameWithProgress(
    moves: List[Move],
    fens: List[String],
    depth: Int = 15,
    progressCallback: Int => Unit
  ): Future[List[MoveAnalysis]] = {
    
    if (fens.length != moves.length + 1) {
      return Future.failed(new IllegalArgumentException(
        s"FEN count (${fens.length}) must be moves count + 1 (${moves.length + 1})"
      ))
    }
    
    println(s"\n=== Analysoidaan ${moves.length} siirtoa (syvyys: $depth) ===\n")
    
    // First, analyze all positions to get evaluations and best moves
    // Use multiPV = 3 to get top 3 moves for arrow display
    def analyzeAllPositions(index: Int, results: List[AnalysisResult]): Future[List[AnalysisResult]] = {
      if (index >= fens.length) {
        Future.successful(results.reverse)
      } else {
        analyzePositionWithFuture(fens(index), depth, multiPV = 3).flatMap { result =>
          // Report progress (index is 0-based, report 1-based move number)
          if (index > 0) progressCallback(index)
          analyzeAllPositions(index + 1, result :: results)
        }
      }
    }
    
    // Then classify each move based on the evaluations
    analyzeAllPositions(0, List.empty).map { allResults =>
      val analyses = moves.zipWithIndex.map { case (move, index) =>
        val isWhiteMove = index % 2 == 0
        
        val evalBefore = allResults(index).evaluation
        val actualMove = move.notation.getOrElse("")
        val evalAfter = if (actualMove.endsWith("#")) {
          PositionEvaluation.MateIn(0)  // Checkmate delivered - game over
        } else if (index + 1 < allResults.length) {
          allResults(index + 1).evaluation
        } else {
          allResults(index).evaluation
        }
        
        val bestMove = allResults(index).bestMove.getOrElse("")
        val actualMoveUCI = {
          val fromSquare = move.from.name.toLowerCase
          val toSquare = move.to.name.toLowerCase
          val promotion = move.promotedPieceType.map {
            case PieceType.Queen => "q"
            case PieceType.Rook => "r"
            case PieceType.Bishop => "b"
            case PieceType.Knight => "n"
            case _ => ""
          }.getOrElse("")
          s"$fromSquare$toSquare$promotion"
        }
        val bestMoveEval = allResults(index).evaluation
        
        // Get second best move evaluation for Great/Brilliant detection
        val topMoveEvals = allResults(index).topMoveEvals
        val secondBestEval: Option[PositionEvaluation] = topMoveEvals.lift(1)
        
        // Detect material sacrifice with detailed info
        val directSacrificeInfo = detectSacrificeInfo(move, evalBefore, evalAfter, isWhiteMove)
        
        // Get moved piece's destination square for objective sacrifice detection
        val movedPieceSquareProgress: Option[(Int, Int)] = {
          val toSquare = move.to.name.toLowerCase
          if (toSquare.length == 2) {
            val col = toSquare.charAt(0) - 'a'
            val row = 8 - toSquare.charAt(1).asDigit
            Some((row, col))
          } else None
        }
        
        // Check for OBJECTIVE sacrifice: analyze board state AFTER the move
        // Returns: (sacrificeValue, captureIsLegal, isIndirectSacrifice)
        val (objectiveSacrificeValueProgress, objSacCaptureIsLegalProgress, objSacIsIndirectProgress) = detectObjectiveSacrifice(
          fens(index + 1),
          isWhiteMove,
          movedPieceSquareProgress
        )
        
        // Only count as sacrifice if position is good
        val evalAfterPlayerProgress = if (isWhiteMove) evalAfter.toNumeric else -evalAfter.toNumeric
        val objectiveSacrificeIfGoodProgress = if (objectiveSacrificeValueProgress >= 200 && evalAfterPlayerProgress >= 0.5) {
          objectiveSacrificeValueProgress
        } else {
          0
        }
        
        // Combine direct sacrifice and objective sacrifice
        val sacrificeInfo = if (directSacrificeInfo.materialSacrificed > 0) {
          Some(directSacrificeInfo)
        } else if (objectiveSacrificeIfGoodProgress >= 300) {
          Some(SacrificeInfo(
            materialSacrificed = objectiveSacrificeIfGoodProgress, 
            isTrade = false, 
            captureIsLegal = objSacCaptureIsLegalProgress,
            isIndirectSacrifice = objSacIsIndirectProgress
          ))
        } else {
          Some(directSacrificeInfo)
        }
        
        val classification = MoveEvaluator.classifyMove(
          evalBefore, evalAfter, bestMoveEval, actualMoveUCI, bestMove, isWhiteMove,
          materialSacrificed = false,
          secondBestEval = secondBestEval,
          sacrificeInfo = sacrificeInfo
        )
        
        val evalAfterStr = if (index == moves.length - 1 && actualMove.endsWith("#")) {
          if (isWhiteMove) "1-0" else "0-1"
        } else {
          formatEval(evalAfter)
        }
        val moveColor = if (isWhiteMove) "White" else "Black"
        println(f"${index + 1}%2d. $moveColor%-5s ${actualMove}%-8s | Eval: $evalAfterStr%-8s | ${classification.description}")
        
        // Get top moves from the position analysis
        val topMoves = allResults(index).topMoves
        val pv = allResults(index).principalVariation
        
        MoveAnalysis(
          move = move,
          classification = classification,
          evalBefore = evalBefore,
          evalAfter = evalAfter,
          bestMove = Some(bestMove),
          bestMoveEval = evalBefore,
          isWhiteMove = isWhiteMove,
          topMoves = topMoves,
          principalVariation = pv
        )
      }
      
      println(s"\n=== Analyysi valmis: ${analyses.length} siirtoa ===\n")
      analyses
    }
  }
}

/**
 * Result of analyzing a single move
 */
case class MoveAnalysis(
  move: Move,
  classification: MoveClassification,
  evalBefore: PositionEvaluation,
  evalAfter: PositionEvaluation,
  bestMove: Option[String],
  bestMoveEval: PositionEvaluation,
  isWhiteMove: Boolean,
  topMoves: List[String] = List.empty,  // Top engine moves (best first) in UCI notation
  principalVariation: List[String] = List.empty  // Full PV line (alternating moves) for Best Response
) {
  /** Get a human-readable summary */
  def summary: String = {
    val moveStr = move.notation.getOrElse("???")
    val classStr = classification.description
    val symbol = classification.symbol
    val symbolStr = if (symbol.nonEmpty) s" $symbol" else ""
    
    s"$moveStr$symbolStr - $classStr"
  }
  
  /** Get evaluation change in centipawns */
  def evalChange: Double = {
    val before = if (isWhiteMove) evalBefore.toNumeric else -evalBefore.toNumeric
    val after = if (isWhiteMove) evalAfter.toNumeric else -evalAfter.toNumeric
    after - before
  }
  
  /**
   * Get scaled centipawn loss for ACPL calculation.
   * 
   * In winning positions (≥+3.0), uses Win% drop scaled to CPL:
   *   - Best (0-0.3% drop) → 0-2 cpl
   *   - Excellent (0.3-1% drop) → 2-12 cpl
   *   - Good (1-3% drop) → 13-20 cpl
   *   - Inaccuracy (3-8% drop) → 21-80 cpl
   *   - Mistake (8-15% drop) → 81-200 cpl
   *   - Blunder (15%+ drop) → 201-1000 cpl
   * 
   * In balanced/losing positions, uses raw centipawn difference.
   */
  def scaledCpLoss: Double = {
    val playerEvalBefore = if (isWhiteMove) evalBefore.toNumeric else -evalBefore.toNumeric
    val winningThreshold = 3.0  // Same as in MoveEvaluator.classifyMove
    
    // Check if in winning position (use same logic as classifyMove)
    val isForcedMate = evalBefore match {
      case PositionEvaluation.MateIn(moves) => 
        if (isWhiteMove) moves > 0 else moves < 0
      case _ => false
    }
    val useWinProbabilitySystem = playerEvalBefore >= winningThreshold || isForcedMate
    
    if (useWinProbabilitySystem) {
      // Calculate Win% before and after
      val winPctBefore = if (isWhiteMove) evalBefore.toWinProbability else 100.0 - evalBefore.toWinProbability
      val winPctAfter = if (isWhiteMove) evalAfter.toWinProbability else 100.0 - evalAfter.toWinProbability
      val winPctDrop = Math.max(0, winPctBefore - winPctAfter)
      
      // Convert to scaled CPL
      PositionEvaluation.winPctDropToScaledCpl(winPctDrop).toDouble
    } else {
      // Standard centipawn loss
      Math.max(0, -evalChange * 100.0)
    }
  }
}

object GameAnalyzer {
  /**
   * Create a GameAnalyzer with a new Stockfish engine
   */
  def create(config: EngineConfig = EngineConfig.DEFAULT)(implicit ec: ExecutionContext): Option[GameAnalyzer] = {
    val engine = new StockfishEngine(config)
    if (engine.start()) {
      Some(new GameAnalyzer(engine))
    } else {
      None
    }
  }
}
