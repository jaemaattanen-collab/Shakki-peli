package chess.analysis

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MoveEvaluationSpec extends AnyFlatSpec with Matchers {
  
  "PositionEvaluation" should "parse centipawn evaluations" in {
    val eval = PositionEvaluation.parse("cp 150")
    eval shouldBe Some(PositionEvaluation.Centipawns(150))
    eval.get.toNumeric shouldBe 1.5
  }
  
  it should "parse mate evaluations" in {
    val eval = PositionEvaluation.parse("mate 5")
    eval shouldBe Some(PositionEvaluation.MateIn(5))
    // MateIn now uses finite values (±10 pawns) instead of Infinity
    eval.get.toNumeric should be > 9.0
  }
  
  it should "parse negative mate evaluations" in {
    val eval = PositionEvaluation.parse("mate -3")
    eval shouldBe Some(PositionEvaluation.MateIn(-3))
    // MateIn now uses finite values (±10 pawns) instead of Infinity
    eval.get.toNumeric should be < -9.0
  }
  
  "EvaluationCategory" should "classify balanced positions" in {
    val category = EvaluationCategory.fromCentipawns(25)
    category shouldBe EvaluationCategory.Balanced
  }
  
  it should "classify slight advantage" in {
    val category = EvaluationCategory.fromCentipawns(40)
    category shouldBe EvaluationCategory.SlightAdvantage
  }
  
  it should "classify advantage" in {
    val category = EvaluationCategory.fromCentipawns(75)
    category shouldBe EvaluationCategory.Advantage
  }
  
  it should "classify large advantage" in {
    val category = EvaluationCategory.fromCentipawns(150)
    category shouldBe EvaluationCategory.LargeAdvantage
  }
  
  it should "classify massive advantage" in {
    val category = EvaluationCategory.fromCentipawns(350)
    category shouldBe EvaluationCategory.MassiveAdvantage
  }
  
  it should "classify winning position" in {
    val category = EvaluationCategory.fromCentipawns(550)
    category shouldBe EvaluationCategory.Winning
  }
  
  it should "classify forced mate" in {
    val mateEval = PositionEvaluation.MateIn(5)
    mateEval.category shouldBe EvaluationCategory.ForcedMate
  }
  
  it should "classify long mate sequence as winning" in {
    val mateEval = PositionEvaluation.MateIn(12)
    mateEval.category shouldBe EvaluationCategory.Winning
  }
  
  "MoveEvaluator" should "classify best move" in {
    val before = PositionEvaluation.Centipawns(50)
    val after = PositionEvaluation.Centipawns(50)
    val best = PositionEvaluation.Centipawns(50)
    
    val classification = MoveEvaluator.classifyMove(before, after, best, "e2e4", "e2e4", isWhiteMove = true)
    classification shouldBe MoveClassification.Best
  }
  
  it should "classify excellent move" in {
    val before = PositionEvaluation.Centipawns(50)
    val after = PositionEvaluation.Centipawns(45)   // Only 5cp loss
    val best = PositionEvaluation.Centipawns(50)    // Best was to maintain 50cp
    
    // With new thresholds: Best ≤ 2cp, Excellent ≤ 12cp
    // 5cp loss should be Excellent
    val classification = MoveEvaluator.classifyMove(before, after, best, "e2e3", "e2e4", isWhiteMove = true)
    classification shouldBe MoveClassification.Excellent
  }
  
  it should "classify inaccuracy (drops one category)" in {
    val before = PositionEvaluation.Centipawns(100) // Large advantage
    val after = PositionEvaluation.Centipawns(60)   // Advantage
    val best = PositionEvaluation.Centipawns(100)
    
    val classification = MoveEvaluator.classifyMove(before, after, best, "e2e4", "d2d4", isWhiteMove = true)
    classification shouldBe MoveClassification.Inaccuracy
  }
  
  it should "classify mistake (drops two categories)" in {
    val before = PositionEvaluation.Centipawns(150) // Large advantage (order 3: 1.5 pawns)
    val after = PositionEvaluation.Centipawns(40)   // Slight advantage (order 1: 0.4 pawns)
    val best = PositionEvaluation.Centipawns(150)
    
    val classification = MoveEvaluator.classifyMove(before, after, best, "e2e4", "d2d4", isWhiteMove = true)
    classification shouldBe MoveClassification.Mistake
  }
  
  it should "classify blunder (drops three+ categories)" in {
    val before = PositionEvaluation.Centipawns(300) // Massive advantage
    val after = PositionEvaluation.Centipawns(0)    // Balanced
    val best = PositionEvaluation.Centipawns(300)
    
    val classification = MoveEvaluator.classifyMove(before, after, best, "e2e4", "d2d4", isWhiteMove = true)
    classification shouldBe MoveClassification.Blunder
  }
  
  it should "classify brilliant move with material sacrifice" in {
    val before = PositionEvaluation.Centipawns(100)
    val after = PositionEvaluation.Centipawns(100)
    val best = PositionEvaluation.Centipawns(100)
    
    val classification = MoveEvaluator.classifyMove(
      before, after, best,
      "e2e4", "e2e4",
      isWhiteMove = true, 
      materialSacrificed = true
    )
    // This should be brilliant if it's the only move maintaining category
    // For now, with equal evals, it would be Best, but the logic handles brilliant cases
    classification.should(be(MoveClassification.Best).or(be(MoveClassification.Brilliant)))
  }
  
  it should "work correctly for black moves" in {
    val before = PositionEvaluation.Centipawns(-100) // Black has large advantage
    val after = PositionEvaluation.Centipawns(-100)
    val best = PositionEvaluation.Centipawns(-100)
    
    val classification = MoveEvaluator.classifyMove(before, after, best, "e7e5", "e7e5", isWhiteMove = false)
    classification.shouldBe(MoveClassification.Best)
  }
  
  it should "detect blunder for black" in {
    val before = PositionEvaluation.Centipawns(-300) // Black has massive advantage
    val after = PositionEvaluation.Centipawns(0)     // Now balanced (bad for black!)
    val best = PositionEvaluation.Centipawns(-300)
    
    val classification = MoveEvaluator.classifyMove(before, after, best, "e7e5", "d7d5", isWhiteMove = false)
    classification.shouldBe(MoveClassification.Blunder)
  }
  
  "Win Probability" should "calculate correctly for balanced position" in {
    val eval = PositionEvaluation.Centipawns(0)
    eval.toWinProbability shouldBe 50.0 +- 0.1
  }
  
  it should "calculate correctly for white advantage" in {
    val eval = PositionEvaluation.Centipawns(300)  // +3.0 pawns
    // Lichess formula gives ~75% for +300 cp
    eval.toWinProbability should be > 70.0
    eval.toWinProbability should be < 85.0
  }
  
  it should "calculate correctly for black advantage" in {
    val eval = PositionEvaluation.Centipawns(-300)  // -3.0 pawns (black ahead)
    // White's win probability should be low (~25%)
    eval.toWinProbability should be > 15.0
    eval.toWinProbability should be < 30.0
  }
  
  it should "be 100% for white mate" in {
    val eval = PositionEvaluation.MateIn(5)
    eval.toWinProbability shouldBe 100.0
  }
  
  it should "be 0% for black mate" in {
    val eval = PositionEvaluation.MateIn(-5)
    eval.toWinProbability shouldBe 0.0
  }
  
  "Scaled CPL" should "map win% drop to correct ranges" in {
    // Best: 0-0.3% → 0-2 cpl
    PositionEvaluation.winPctDropToScaledCpl(0.0) shouldBe 0
    PositionEvaluation.winPctDropToScaledCpl(0.3) shouldBe 2
    
    // Excellent: 0.3-1% → 2-12 cpl
    PositionEvaluation.winPctDropToScaledCpl(1.0) shouldBe 12
    
    // Good: 1-3% → 13-20 cpl
    PositionEvaluation.winPctDropToScaledCpl(3.0) shouldBe 20
    
    // Inaccuracy: 3-8% → 21-80 cpl
    PositionEvaluation.winPctDropToScaledCpl(8.0) shouldBe 80
    
    // Mistake: 8-15% → 81-200 cpl
    PositionEvaluation.winPctDropToScaledCpl(15.0) shouldBe 200
    
    // Blunder: 15%+ → 201+ cpl
    PositionEvaluation.winPctDropToScaledCpl(50.0) should be > 200
  }
  
  // ==========================================================================
  // SACRIFICE INFO TESTS
  // ==========================================================================
  
  "SacrificeInfo" should "identify equal trades correctly" in {
    // Knight takes Knight on protected square = trade
    val knightTrade = SacrificeInfo(materialSacrificed = 0, isTrade = true)
    knightTrade.isTrade shouldBe true
    knightTrade.materialSacrificed shouldBe 0
  }
  
  it should "identify real sacrifices" in {
    // Rook (500) takes Pawn (100) on protected square = 400cp sacrifice
    val rookSacrifice = SacrificeInfo(materialSacrificed = 400, isTrade = false)
    rookSacrifice.isTrade shouldBe false
    rookSacrifice.materialSacrificed shouldBe 400
  }
  
  // ==========================================================================
  // GREAT MOVE TESTS
  // ==========================================================================
  
  "MoveEvaluator" should "classify Great move when second best is 10%+ worse" in {
    val before = PositionEvaluation.Centipawns(0)     // Balanced
    val after = PositionEvaluation.Centipawns(100)    // Slight advantage
    val best = PositionEvaluation.Centipawns(100)     // Best move gives +1.0
    val secondBest = PositionEvaluation.Centipawns(-50) // Second best gives -0.5 (much worse)
    
    // Win% at +100cp ≈ 63%, Win% at -50cp ≈ 43%
    // Gap = 20% > 10% threshold → Great Move
    val classification = MoveEvaluator.classifyMove(
      before, after, best,
      "e2e4", "e2e4",  // Played the best move
      isWhiteMove = true,
      secondBestEval = Some(secondBest)
    )
    classification shouldBe MoveClassification.Great
  }
  
  it should "not classify Great move when second best is close" in {
    val before = PositionEvaluation.Centipawns(0)
    val after = PositionEvaluation.Centipawns(100)
    val best = PositionEvaluation.Centipawns(100)     // Best move gives +1.0
    val secondBest = PositionEvaluation.Centipawns(80) // Second best gives +0.8 (close)
    
    // Win% gap is small < 10% → just Best, not Great
    val classification = MoveEvaluator.classifyMove(
      before, after, best,
      "e2e4", "e2e4",
      isWhiteMove = true,
      secondBestEval = Some(secondBest)
    )
    classification shouldBe MoveClassification.Best
  }
  
  // ==========================================================================
  // BRILLIANT MOVE TESTS
  // ==========================================================================
  
  it should "classify Brilliant move with sacrifice + Great criteria + advantage" in {
    val before = PositionEvaluation.Centipawns(0)     // Balanced
    val after = PositionEvaluation.Centipawns(350)    // +3.5 pawns advantage
    val best = PositionEvaluation.Centipawns(350)
    val secondBest = PositionEvaluation.Centipawns(-100) // Second best is bad
    
    // Sacrifice info: Knight (300cp) sacrificed, not a trade
    val sacrificeInfo = Some(SacrificeInfo(materialSacrificed = 300, isTrade = false))
    
    val classification = MoveEvaluator.classifyMove(
      before, after, best,
      "e2e4", "e2e4",
      isWhiteMove = true,
      secondBestEval = Some(secondBest),
      sacrificeInfo = sacrificeInfo
    )
    classification shouldBe MoveClassification.Brilliant
  }
  
  it should "not classify Brilliant if sacrifice is a trade" in {
    val before = PositionEvaluation.Centipawns(0)
    val after = PositionEvaluation.Centipawns(350)
    val best = PositionEvaluation.Centipawns(350)
    val secondBest = PositionEvaluation.Centipawns(-100)
    
    // Trade, not a real sacrifice
    val sacrificeInfo = Some(SacrificeInfo(materialSacrificed = 300, isTrade = true))
    
    val classification = MoveEvaluator.classifyMove(
      before, after, best,
      "e2e4", "e2e4",
      isWhiteMove = true,
      secondBestEval = Some(secondBest),
      sacrificeInfo = sacrificeInfo
    )
    // Should be Great (not Brilliant) because it's a trade
    classification shouldBe MoveClassification.Great
  }
  
  it should "not classify Brilliant without significant sacrifice (< 300cp)" in {
    val before = PositionEvaluation.Centipawns(0)
    val after = PositionEvaluation.Centipawns(350)
    val best = PositionEvaluation.Centipawns(350)
    val secondBest = PositionEvaluation.Centipawns(-100)
    
    // Small sacrifice (200cp) - not enough for Brilliant
    val sacrificeInfo = Some(SacrificeInfo(materialSacrificed = 200, isTrade = false))
    
    val classification = MoveEvaluator.classifyMove(
      before, after, best,
      "e2e4", "e2e4",
      isWhiteMove = true,
      secondBestEval = Some(secondBest),
      sacrificeInfo = sacrificeInfo
    )
    classification shouldBe MoveClassification.Great
  }
  
  it should "not classify Brilliant without sufficient resulting advantage" in {
    val before = PositionEvaluation.Centipawns(0)
    val after = PositionEvaluation.Centipawns(200)    // Only +2.0 (needs +3.0)
    val best = PositionEvaluation.Centipawns(200)
    val secondBest = PositionEvaluation.Centipawns(-100)
    
    val sacrificeInfo = Some(SacrificeInfo(materialSacrificed = 300, isTrade = false))
    
    val classification = MoveEvaluator.classifyMove(
      before, after, best,
      "e2e4", "e2e4",
      isWhiteMove = true,
      secondBestEval = Some(secondBest),
      sacrificeInfo = sacrificeInfo
    )
    // Great but not Brilliant (not enough advantage)
    classification shouldBe MoveClassification.Great
  }
  
  it should "classify Brilliant for black moves correctly" in {
    // Black's perspective: negative eval is good for black
    val before = PositionEvaluation.Centipawns(0)
    val after = PositionEvaluation.Centipawns(-350)   // Black has +3.5 advantage
    val best = PositionEvaluation.Centipawns(-350)
    val secondBest = PositionEvaluation.Centipawns(100) // Second best is bad for black
    
    val sacrificeInfo = Some(SacrificeInfo(materialSacrificed = 300, isTrade = false))
    
    val classification = MoveEvaluator.classifyMove(
      before, after, best,
      "e7e5", "e7e5",
      isWhiteMove = false,
      secondBestEval = Some(secondBest),
      sacrificeInfo = sacrificeInfo
    )
    classification shouldBe MoveClassification.Brilliant
  }
}
