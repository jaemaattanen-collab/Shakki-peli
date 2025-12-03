package chess.analysis

import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Example application showing how to use GameAnalyzer
 * Run with: sbt "runMain chess.analysis.AnalysisExample"
 */
object AnalysisExample extends App {
  
  println("\n" + "="*70)
  println("  Chess Game Analysis Example")
  println("="*70)
  
  // Create analyzer
  val analyzerOpt = GameAnalyzer.create(EngineConfig(depth = 20, threads = 2))
  
  analyzerOpt match {
    case Some(analyzer) =>
      try {
        println("\n✓ Stockfish engine started")
        
        // Example 1: Analyze opening position
        println("\n--- Example 1: Starting Position ---")
        val startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        
        var result: Option[StockfishEngine#EngineAnalysis] = None
        val lock = new Object()
        
        val callback: StockfishEngine#EngineAnalysis => Unit = analysis => {
          lock.synchronized {
            result = Some(analysis)
            lock.notify()
          }
        }
        
        analyzer.engine.addAnalysisCallback(callback)
        analyzer.engine.analyzePosition(startFen, 20, 1)
        
        lock.synchronized {
          lock.wait(30000)
        }
        
        analyzer.engine.removeAnalysisCallback(callback)
        
        result.foreach { analysis =>
          val eval = if (analysis.mate.isDefined) {
            s"Mate in ${analysis.mate.get}"
          } else {
            s"${analysis.score.getOrElse(0)} centipawns"
          }
          
          println(s"FEN: $startFen")
          println(s"Evaluation: $eval")
          println(s"Best move: ${analysis.bestMove.getOrElse("N/A")}")
          println(s"Principal variation: ${analysis.pv.take(5).mkString(" ")}")
          
          // Show evaluation
          val posEval = if (analysis.mate.isDefined) {
            PositionEvaluation.MateIn(analysis.mate.get)
          } else {
            PositionEvaluation.Centipawns(analysis.score.getOrElse(0).toDouble)
          }
          
          val winPct = posEval.toWinProbability
          println(f"Win probability (White): $winPct%.1f%%")
        }
        
        // Example 2: Analyze position after 1.e4
        println("\n--- Example 2: After 1.e4 ---")
        val afterE4Fen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
        
        result = None
        analyzer.engine.addAnalysisCallback(callback)
        analyzer.engine.analyzePosition(afterE4Fen, 20, 1)
        
        lock.synchronized {
          lock.wait(30000)
        }
        
        analyzer.engine.removeAnalysisCallback(callback)
        
        result.foreach { analysis =>
          val eval = if (analysis.mate.isDefined) {
            s"Mate in ${analysis.mate.get}"
          } else {
            s"${analysis.score.getOrElse(0)} centipawns"
          }
          
          println(s"FEN: $afterE4Fen")
          println(s"Evaluation: $eval")
          println(s"Best move: ${analysis.bestMove.getOrElse("N/A")}")
          
          val posEval = if (analysis.mate.isDefined) {
            PositionEvaluation.MateIn(analysis.mate.get)
          } else {
            PositionEvaluation.Centipawns(analysis.score.getOrElse(0).toDouble)
          }
          
          val winPctWhite = posEval.toWinProbability
          val winPctBlack = 100.0 - winPctWhite
          println(f"Win probability (White): $winPctWhite%.1f%%")
          println(f"Win probability (Black): $winPctBlack%.1f%%")
        }
        
        // Example 3: Multi-PV analysis
        println("\n--- Example 3: Multi-PV Analysis (Top 3 Moves) ---")
        
        val multiPvResults = scala.collection.mutable.ListBuffer[StockfishEngine#Variation]()
        
        val multiPvCallback: StockfishEngine#EngineAnalysis => Unit = analysis => {
          lock.synchronized {
            multiPvResults.clear()
            multiPvResults ++= analysis.variations
            if (analysis.variations.size >= 3 || analysis.depth >= 20) {
              lock.notify()
            }
          }
        }
        
        analyzer.engine.addAnalysisCallback(multiPvCallback)
        analyzer.engine.analyzePosition(startFen, 20, 3)
        
        lock.synchronized {
          lock.wait(30000)
        }
        
        analyzer.engine.removeAnalysisCallback(multiPvCallback)
        
        multiPvResults.zipWithIndex.foreach { case (variation, idx) =>
          val eval = if (variation.mate.isDefined) {
            s"Mate in ${variation.mate.get}"
          } else {
            s"${variation.score.getOrElse(0)} cp"
          }
          println(s"${idx + 1}. ${variation.pv.take(3).mkString(" ")} - $eval")
        }
        
        println("\n" + "="*70)
        println("  Analysis complete!")
        println("="*70 + "\n")
        
      } finally {
        analyzer.stop()
        println("✓ Engine stopped\n")
      }
      
    case None =>
      println("\n✗ Could not start Stockfish engine")
      println("  Make sure Stockfish is installed:")
      println("  - macOS: brew install stockfish")
      println("  - Or download from: https://stockfishchess.org/download/\n")
  }
}
