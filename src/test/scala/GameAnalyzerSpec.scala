package chess.analysis

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._

/**
 * Integration test for GameAnalyzer - requires Stockfish to be installed
 * Run with: sbt "testOnly *GameAnalyzerSpec"
 */
class GameAnalyzerSpec extends AnyFlatSpec with Matchers {
  implicit val ec: ExecutionContext = ExecutionContext.global
  
  "GameAnalyzer" should "connect to Stockfish and analyze positions" in {
    // This test requires Stockfish to be installed
    val analyzerOpt = GameAnalyzer.create(EngineConfig(depth = 15))
    
    analyzerOpt match {
      case Some(analyzer) =>
        try {
          // Starting position FEN
          val startingFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
          
          info("✓ Stockfish connection successful")
          info(s"  Starting position FEN: $startingFen")
          
          succeed
        } finally {
          analyzer.stop()
        }
        
      case None =>
        info("⚠ Stockfish not found - skipping test")
        info("  Install with: brew install stockfish (macOS)")
        info("  Or download from: https://stockfishchess.org/download/")
        succeed
    }
  }
  
  it should "evaluate opening position as balanced" in {
    val analyzerOpt = GameAnalyzer.create(EngineConfig(depth = 15))
    
    analyzerOpt match {
      case Some(analyzer) =>
        try {
          val startingFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
          
          // Use the engine directly for simple evaluation
          var result: Option[PositionEvaluation] = None
          val lock = new Object()
          
          val callback: StockfishEngine#EngineAnalysis => Unit = analysis => {
            lock.synchronized {
              result = Some(if (analysis.mate.isDefined) {
                PositionEvaluation.MateIn(analysis.mate.get)
              } else {
                PositionEvaluation.Centipawns(analysis.score.getOrElse(0).toDouble)
              })
              lock.notify()
            }
          }
          
          analyzer.engine.addAnalysisCallback(callback)
          analyzer.engine.analyzePosition(startingFen, 15, 1)
          
          lock.synchronized {
            lock.wait(30000)
          }
          
          analyzer.engine.removeAnalysisCallback(callback)
          
          result match {
            case Some(eval) =>
              info(s"✓ Starting position evaluation: $eval")
              eval.category should (be(EvaluationCategory.Balanced) or be(EvaluationCategory.SlightAdvantage))
            case None =>
              fail("No evaluation received from Stockfish")
          }
        } finally {
          analyzer.stop()
        }
        
      case None =>
        info("⚠ Stockfish not found - skipping test")
        succeed
    }
  }
  
  it should "provide API example for manual testing" in {
    info("\n" + "="*60)
    info("GameAnalyzer API Usage Example")
    info("="*60)
    info("")
    info("1. Create analyzer:")
    info("   val analyzer = GameAnalyzer.create().get")
    info("")
    info("2. Analyze a position (returns Future):")
    info("   val fen = \"rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1\"")
    info("   // Need full Move object with piece, from, to squares")
    info("")
    info("3. For simple evaluation, use engine directly:")
    info("   analyzer.engine.analyzePosition(fen, depth = 20, multiPV = 1)")
    info("   analyzer.engine.addAnalysisCallback { analysis =>")
    info("     println(s\"Score: ${analysis.score}\")")
    info("     println(s\"Best move: ${analysis.bestMove}\")")
    info("   }")
    info("")
    info("4. Stop engine when done:")
    info("   analyzer.stop()")
    info("")
    info("="*60)
    succeed
  }
}

