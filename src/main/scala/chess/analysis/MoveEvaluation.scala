package chess.analysis

/** Represents a chess position evaluation from Stockfish */
sealed trait PositionEvaluation {
  /** Convert to numeric value for comparison (from white's perspective) */
  def toNumeric: Double
  
  /** Get evaluation category */
  def category: EvaluationCategory
  
  /** Convert to win probability (0-100%) using Lichess formula */
  def toWinProbability: Double
}

object PositionEvaluation {
  // Mate is treated as ±10 pawns (±1000 centipawns) for cpLoss calculations
  // This prevents NaN/Infinity issues while still representing a decisive advantage
  val MATE_VALUE_PAWNS = 10.0
  
  /**
   * Convert centipawns to win probability using Lichess formula.
   * Based on real game data from 2300+ rated players.
   * Formula: Win% = 50 + 50 * (2 / (1 + exp(-0.00368208 * centipawns)) - 1)
   * 
   * For ACPL purposes, evaluations ≥30 pawns (3000cp) are treated as equivalent to mate (100%/0%)
   * 
   * Examples:
   *   0 cp → 50%
   * +100 cp → ~63%
   * +300 cp → ~83%
   * +500 cp → ~92%
   * +700 cp → ~96%
   * +900 cp → ~98%
   * +2000 cp → 100% (treated as mate)
   */
  def centipawnsToWinProbability(centipawns: Double): Double = {
    // Treat ±20 pawns (±2000cp) or higher as equivalent to mate for ACPL
    if (centipawns >= 2000) return 100.0
    if (centipawns <= -2000) return 0.0
    
    val clamped = Math.max(-1500, Math.min(1500, centipawns))  // Clamp to reasonable range
    50.0 + 50.0 * (2.0 / (1.0 + Math.exp(-0.00368208 * clamped)) - 1.0)
  }
  
  /**
   * Convert Win% drop to scaled centipawn loss for ACPL calculation.
   * Uses linear interpolation within each classification range.
   * 
   * Classification ranges:
   *   Best:       0-0.3% drop  → 0-2 cpl
   *   Excellent:  0.3-1% drop  → 2-12 cpl
   *   Good:       1-3% drop    → 13-20 cpl
   *   Inaccuracy: 3-8% drop    → 21-80 cpl
   *   Mistake:    8-15% drop   → 81-200 cpl
   *   Blunder:    15-100% drop → 201-1000 cpl
   * 
   * @param winPctDrop The win probability drop (0-100)
   * @return Scaled centipawn loss as integer (0-1000)
   */
  def winPctDropToScaledCpl(winPctDrop: Double): Int = {
    val drop = Math.max(0, winPctDrop)  // Ensure non-negative
    
    val cpl = if (drop <= 0.3) {
      // Best: 0-0.3% → 0-2 cpl
      (drop / 0.3) * 2.0
    } else if (drop <= 1.0) {
      // Excellent: 0.3-1% → 2-12 cpl
      2.0 + ((drop - 0.3) / 0.7) * 10.0
    } else if (drop <= 3.0) {
      // Good: 1-3% → 13-20 cpl
      13.0 + ((drop - 1.0) / 2.0) * 7.0
    } else if (drop <= 8.0) {
      // Inaccuracy: 3-8% → 21-80 cpl
      21.0 + ((drop - 3.0) / 5.0) * 59.0
    } else if (drop <= 15.0) {
      // Mistake: 8-15% → 81-200 cpl
      81.0 + ((drop - 8.0) / 7.0) * 119.0
    } else {
      // Blunder: 15-100% → 201-1000 cpl
      val remaining = Math.min(drop, 100.0) - 15.0  // Cap at 100%
      201.0 + (remaining / 85.0) * 799.0
    }
    
    Math.round(cpl).toInt
  }
  
  /** Centipawn evaluation (e.g., +150 means white is 1.5 pawns ahead) */
  case class Centipawns(value: Double) extends PositionEvaluation {
    def toNumeric: Double = value / 100.0
    def category: EvaluationCategory = EvaluationCategory.fromCentipawns(value)
    def toWinProbability: Double = centipawnsToWinProbability(value)
  }
  
  /** Mate in N moves (positive = white wins, negative = black wins) */
  case class MateIn(moves: Int) extends PositionEvaluation {
    // Use finite values for calculations: ±10 pawns
    // Closer mates are slightly more valuable (but all are decisive)
    def toNumeric: Double = {
      val sign = if (moves > 0) 1.0 else -1.0
      val mateBonus = Math.max(0, 10 - Math.abs(moves)) * 0.01  // Small bonus for faster mates
      sign * (MATE_VALUE_PAWNS + mateBonus)
    }
    def category: EvaluationCategory = 
      if (Math.abs(moves) <= 8) EvaluationCategory.ForcedMate
      else EvaluationCategory.Winning
    
    // Mate = 100% (or 0% for opponent)
    def toWinProbability: Double = if (moves > 0) 100.0 else 0.0
  }
  
  /** Parse Stockfish evaluation string like "cp 150" or "mate 5" */
  def parse(evalString: String): Option[PositionEvaluation] = {
    evalString.trim.toLowerCase.split("\\s+").toList match {
      case "cp" :: value :: _ => 
        value.toDoubleOption.map(Centipawns(_))
      case "mate" :: moves :: _ => 
        moves.toIntOption.map(MateIn(_))
      case _ => None
    }
  }
}

/** Evaluation categories based on position assessment (internal use only) */
sealed trait EvaluationCategory {
  def order: Int
}

object EvaluationCategory {
  case object ForcedMate extends EvaluationCategory { val order = 6 }
  case object Winning extends EvaluationCategory { val order = 5 }
  case object MassiveAdvantage extends EvaluationCategory { val order = 4 }
  case object LargeAdvantage extends EvaluationCategory { val order = 3 }
  case object Advantage extends EvaluationCategory { val order = 2 }
  case object SlightAdvantage extends EvaluationCategory { val order = 1 }
  case object Balanced extends EvaluationCategory { val order = 0 }
  case object SlightDisadvantage extends EvaluationCategory { val order = -1 }
  case object Disadvantage extends EvaluationCategory { val order = -2 }
  case object LargeDisadvantage extends EvaluationCategory { val order = -3 }
  case object MassiveDisadvantage extends EvaluationCategory { val order = -4 }
  case object Losing extends EvaluationCategory { val order = -5 }
  case object ForcedLoss extends EvaluationCategory { val order = -6 }
  
  /** Get category from centipawn evaluation (from white's perspective) */
  def fromCentipawns(cp: Double): EvaluationCategory = {
    val pawns = Math.abs(cp) / 100.0
    
    if (pawns >= 5.0) Winning
    else if (pawns >= 2.0) MassiveAdvantage
    else if (pawns >= 1.0) LargeAdvantage
    else if (pawns >= 0.5) Advantage
    else if (pawns >= 0.3) SlightAdvantage
    else Balanced
  }
}

/** Classification of a chess move based on how it affects the evaluation */
sealed trait MoveClassification {
  def symbol: String
  def description: String
}

object MoveClassification {
  case object Brilliant extends MoveClassification {
    val symbol = "!!"
    val description = "Uskomaton"
  }
  
  case object Great extends MoveClassification {
    val symbol = "!"
    val description = "Loistava"
  }
  
  case object Best extends MoveClassification {
    val symbol = "★"
    val description = "Paras"
  }
  
  case object Excellent extends MoveClassification {
    val symbol = "○"
    val description = "Erinomainen"
  }
  
  case object Good extends MoveClassification {
    val symbol = "✓"
    val description = "Hyvä"
  }
  
  case object Inaccuracy extends MoveClassification {
    val symbol = "?!"
    val description = "Epätarkkuus"
  }
  
  case object Mistake extends MoveClassification {
    val symbol = "?"
    val description = "Virhe"
  }
  
  case object Blunder extends MoveClassification {
    val symbol = "??"
    val description = "Munaus"
  }
}

/** Evaluator for classifying moves based on position evaluations */
object MoveEvaluator {
  
  /** 
   * Classify a move based on evaluations before and after opponent's response
   * 
   * CRITICAL: Stockfish ALWAYS evaluates from WHITE's perspective!
   * - Positive eval = good for White
   * - Negative eval = good for Black
   * - M3 = White mates in 3
   * - -M3 = Black mates in 3
   * 
   * LOGIC:
   * - For WHITE moves: Higher eval = better (want positive to increase or stay same)
   * - For BLACK moves: Lower eval = better (want negative/smaller positive)
   * 
   * Example Move 14...Kg8:
   * - evalBefore: +1.19 (White slightly better, Black's perspective = -1.19)
   * - evalAfter: M3 (White wins!, Black's perspective = -M3 = DISASTER!)
   * - Change from Black's view: -1.19 → -M3 = Massive worsening = BLUNDER
   * 
   * @param evalBefore Evaluation before this move (from previous move's result)
   * @param evalAfter Evaluation after this move and opponent's response
   * @param bestMoveEval What would happen with best move from the current position
   * @param actualMove The move that was played (in UCI notation)
   * @param bestMove Stockfish's recommended best move (in UCI notation)
   * @param isWhiteMove Whether this is a white move
   * @param materialSacrificed Whether significant material was sacrificed
   * @param secondBestEval Optional evaluation of the second best move (for Great/Brilliant detection)
   * @param sacrificeInfo Optional info about material sacrificed (for Brilliant detection)
   * @return Move classification
   */
  def classifyMove(
    evalBefore: PositionEvaluation,
    evalAfter: PositionEvaluation,
    bestMoveEval: PositionEvaluation,
    actualMove: String,
    bestMove: String,
    isWhiteMove: Boolean,
    materialSacrificed: Boolean = false,
    secondBestEval: Option[PositionEvaluation] = None,
    sacrificeInfo: Option[SacrificeInfo] = None
  ): MoveClassification = {
    
    import MoveClassification._
    
    // Both actualMove and bestMove are now in UCI notation (e.g., "e2e4", "g1f3")
    val actualMoveTrimmed = actualMove.trim.toLowerCase
    val bestMoveTrimmed = bestMove.trim.toLowerCase
    
    // Direct UCI comparison - exact match required
    val isStockfishBest = actualMoveTrimmed.nonEmpty && bestMoveTrimmed.nonEmpty && 
                          actualMoveTrimmed == bestMoveTrimmed
    
    // Check if this move DELIVERS checkmate (game ends immediately)
    // MateIn(0) means checkmate is delivered by this move
    // MateIn(1) means next move will be checkmate - NOT the same thing!
    val isCheckmate = evalAfter match {
      case PositionEvaluation.MateIn(moves) => moves == 0  // Only actual checkmate
      case _ => false
    }
    
    // SPECIAL CASE: Delivering checkmate is always "Best", never Brilliant
    // The sacrifice/brilliance was in the PREVIOUS move that SET UP the mate
    if (isCheckmate && isStockfishBest) {
      return Best
    }
    
    // Get numeric values in pawns (always from White's perspective)
    val cpAfter = evalAfter.toNumeric
    
    // Calculate evaluation from player's perspective
    val playerEvalAfter = if (isWhiteMove) cpAfter else -cpAfter
    
    // Win probability calculations (always use these now)
    val winPctAfter = if (isWhiteMove) evalAfter.toWinProbability else 100.0 - evalAfter.toWinProbability
    val winPctBest = if (isWhiteMove) bestMoveEval.toWinProbability else 100.0 - bestMoveEval.toWinProbability
    
    // Calculate second best move's win probability if available
    val winPctSecondBest: Option[Double] = secondBestEval.map { eval =>
      if (isWhiteMove) eval.toWinProbability else 100.0 - eval.toWinProbability
    }
    
    // ==========================================================================
    // GREAT MOVE DETECTION
    // Criteria: 
    // 1. Must be the best move (Stockfish's #1 choice)
    // 2. Must be the ONLY move that maintains position - all other moves drop win% by at least 10%
    // ==========================================================================
    val isGreatMove = isStockfishBest && winPctSecondBest.exists { secondWinPct =>
      val winPctGapToSecond = winPctBest - secondWinPct
      winPctGapToSecond >= 10.0  // Second best is at least 10% worse
    }
    
    // Check if this move leads to forced mate (M1, M2, M3, etc.)
    val leadingToMate = evalAfter match {
      case PositionEvaluation.MateIn(moves) => moves > 0 && moves <= 5  // Mate in 1-5 for the player
      case _ => false
    }
    
    // ==========================================================================
    // BRILLIANT MOVE DETECTION
    // 
    // Kriteeri 1: "Tappava uhraus"
    // - 200cp+ uhraus, jonka ottaminen johtaisi hävittyyn positioon tai mattiin
    // - Great Move -kriteerit täyttyvät
    // - Ei shakkimatti (matti on "Paras", ei "Uskomaton")
    // - Uhrauksen ottaminen on laillinen siirto
    //
    // Kriteeri 2: "Pakkouhri" (epäsuora uhraus)
    // - Great Move -kriteerit täyttyvät  
    // - Siirto paljastaa materiaalin (indirect sacrifice), jonka vastustajan
    //   on pakko ottaa tai hän häviää (eval pysyy hyvänä ≥+0.0)
    // - Ei shakkimatti
    // - Siirrot ovat laillisia
    //
    // Approksimoidaan "ottaminen johtaisi häviöön" seuraavasti:
    // - Jos eval siirron jälkeen on matti TAI ≥+5.0, uhrauksen ottaminen ei kannata
    // ==========================================================================
    val isBrilliantMove = sacrificeInfo.exists { info =>
      val significantSacrifice = info.materialSacrificed >= 200 && !info.isTrade
      val canBeCaptured = info.captureIsLegal  // Must be a real sacrifice (opponent can take it)
      
      // Kriteeri 1: "Tappava uhraus" (suora tai epäsuora)
      // Ottaminen johtaisi häviöön = eval on matti tai ≥+5.0 etu
      val capturingLeadsToLoss = leadingToMate || playerEvalAfter >= 5.0
      val meetsCriteria1 = isGreatMove && significantSacrifice && capturingLeadsToLoss && canBeCaptured && !isCheckmate
      
      // Kriteeri 2: "Pakkouhri" - kun siirto paljastaa materiaalin, jonka vastustajan on pakko ottaa
      // Tämä kattaa sekä suorat että epäsuorat uhraukset kunhan:
      // - Great Move (ainoa hyvä siirto)
      // - Merkittävä uhraus (≥200cp)
      // - Tilanne pysyy hyvänä (vastustaja häviäisi jos ei ota)
      // - Uhraus on laillinen ottaa
      val positionRemainsGood = playerEvalAfter >= 0.0
      val meetsCriteria2 = isGreatMove && significantSacrifice && positionRemainsGood && canBeCaptured && !isCheckmate
      
      meetsCriteria1 || meetsCriteria2
    }
    
    // DEBUG: Print Brilliant criteria for all Great moves
    if (isGreatMove) {
      val sacInfo = sacrificeInfo.getOrElse(SacrificeInfo(0, false, true, false))
      val capturingLeadsToLoss = leadingToMate || playerEvalAfter >= 5.0
      val positionRemainsGood = playerEvalAfter >= 0.0
      println(s"  DEBUG BRILLIANT CHECK for Great move:")
      println(s"    isGreatMove=$isGreatMove, isCheckmate=$isCheckmate")
      println(s"    leadingToMate=$leadingToMate, playerEvalAfter=$playerEvalAfter")
      println(s"    sacrificeInfo: material=${sacInfo.materialSacrificed}cp, isTrade=${sacInfo.isTrade}, captureIsLegal=${sacInfo.captureIsLegal}, isIndirect=${sacInfo.isIndirectSacrifice}")
      println(s"    Kriteeri 1 (Tappava): significantSac=${sacInfo.materialSacrificed >= 200 && !sacInfo.isTrade}, capturingLeadsToLoss=$capturingLeadsToLoss, canCapture=${sacInfo.captureIsLegal}")
      println(s"    Kriteeri 2 (Pakkouhri): significantSac=${sacInfo.materialSacrificed >= 200 && !sacInfo.isTrade}, positionRemainsGood=$positionRemainsGood, canCapture=${sacInfo.captureIsLegal}")
      println(s"    Result: isBrilliant=$isBrilliantMove")
    }
    
    // Return classification based on precedence
    if (isBrilliantMove) {
      return Brilliant
    }
    
    if (isGreatMove) {
      return Great
    }
    
    if (isStockfishBest) {
      return Best
    }
    
    // ==========================================================================
    // WIN PROBABILITY SYSTEM (used for all positions now)
    // Based on Lichess formula derived from real game data
    // ==========================================================================
    
    // Win probability drop (how much winning chance we lost)
    val winPctDrop = winPctBest - winPctAfter
    
    // Special case: if position is still clearly decided (win% >= 95% or <= 5%), be lenient
    val stillClearlyDecided = winPctAfter >= 95.0 || winPctAfter <= 5.0
    
    // Thresholds for win probability drop
    // Adjusted: Inaccuracy ≤10% (was 8%), Mistake ≤20% (was 15%), Blunder >20% (was >15%)
    if (winPctDrop <= 1.0) {
      if (winPctDrop <= 0.3) Best else Excellent
    } else if (winPctDrop <= 3.0) {
      Good
    } else if (winPctDrop <= 10.0 || stillClearlyDecided) {
      Inaccuracy
    } else if (winPctDrop <= 20.0) {
      Mistake
    } else {
      Blunder
    }
  }
}

/**
 * Result of position analysis from engine
 * 
 * @param evaluation Position evaluation (centipawns or mate)
 * @param bestMove Best move in UCI notation (e.g., "e2e4")
 * @param principalVariation List of moves in the principal variation
 * @param topMoves Top moves from multiPV analysis (best first) in UCI notation
 * @param topMoveEvals Evaluations for top moves (parallel to topMoves list)
 */
case class AnalysisResult(
  evaluation: PositionEvaluation,
  bestMove: Option[String],
  principalVariation: List[String] = List.empty,
  topMoves: List[String] = List.empty,
  topMoveEvals: List[PositionEvaluation] = List.empty
)

/** Information about a potential sacrifice */
case class SacrificeInfo(
  materialSacrificed: Int,  // Centipawns of material given up (positive = sacrifice)
  isTrade: Boolean,         // True if opponent immediately recaptures equivalent material
  captureIsLegal: Boolean = true,  // True if opponent can legally capture the sacrificed material
                                    // False if check prevents capture (e.g., hanging piece during check)
  isIndirectSacrifice: Boolean = false  // True if sacrifice is from a DIFFERENT piece than the one moved
                                         // (e.g., moving rook exposes bishop to capture)
)
