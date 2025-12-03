package chess.analysis

import scala.sys.process._
import java.io._
import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Stockfish UCI (Universal Chess Interface) engine wrapper
 * Handles communication with Stockfish for position analysis
 */
class StockfishEngine(var config: EngineConfig = EngineConfig.DEFAULT) {
  private val engineId = java.util.UUID.randomUUID().toString.take(8)
  private var process: Option[Process] = None
  private var output: Option[BufferedReader] = None
  private var input: Option[PrintWriter] = None
  private var isReady = false
  private var analyzing = false
  private var sideToMove: String = "w"  // Track whose turn it is ("w" or "b")
  private var lastAnalysis: Option[EngineAnalysis] = None  // Track last analysis to preserve evaluation
  private val currentVariations = mutable.Map[Int, Variation]()  // Map of multiPV index to variation
  private var currentDepth = 0
  
  case class Variation(
    score: Option[Int],     // Centipawns (positive = white advantage)
    mate: Option[Int],      // Mate in X moves
    pv: List[String],       // Principal variation moves
    multiPvIndex: Int       // Which variation this is (1 = best, 2 = second best, etc.)
  )
  
  case class EngineAnalysis(
    depth: Int,
    score: Option[Int],     // Centipawns for best move
    mate: Option[Int],      // Mate in X moves for best move
    bestMove: Option[String],
    pv: List[String],       // Best line
    nps: Long,              // Nodes per second
    variations: List[Variation]  // All variations from multiPV
  )
  
  private val analysisCallbacks = mutable.ListBuffer[(EngineAnalysis) => Unit]()
  
  def addAnalysisCallback(callback: EngineAnalysis => Unit): Unit = {
    analysisCallbacks += callback
  }
  
  def removeAnalysisCallback(callback: EngineAnalysis => Unit): Unit = {
    analysisCallbacks -= callback
  }
  
  /**
   * Start the Stockfish engine
   */
  def start(): Boolean = {
    try {
      val processBuilder = Process(config.stockfishPath)
      val pio = new ProcessIO(
        in => {
          input = Some(new PrintWriter(in, true))
        },
        out => {
          output = Some(new BufferedReader(new InputStreamReader(out)))
          // Start reading output in a separate thread
          Future {
            try {
              var line = output.get.readLine()
              while (line != null) {
                processLine(line)
                line = output.get.readLine()
              }
            } catch {
              case _: IOException => // Stream closed
            }
          }
        },
        err => {
          val errReader = new BufferedReader(new InputStreamReader(err))
          Future {
            try {
              var line = errReader.readLine()
              while (line != null) {
                System.err.println(s"Stockfish error: $line")
                line = errReader.readLine()
              }
            } catch {
              case _: IOException => // Stream closed
            }
          }
        }
      )
      
      process = Some(processBuilder.run(pio))
      
      // Wait a bit for engine to start
      Thread.sleep(100)
      
      // Initialize UCI
      sendCommand("uci")
      Thread.sleep(200)
      
      // Set options from config
      sendCommand(s"setoption name Hash value ${config.hashSizeMB}")
      sendCommand(s"setoption name Threads value ${config.threads}")
      
      sendCommand("isready")
      
      // Wait for readyok
      val startTime = System.currentTimeMillis()
      while (!isReady && System.currentTimeMillis() - startTime < 5000) {
        Thread.sleep(50)
      }
      
      if (isReady) {
        println(s"✓ Stockfish engine [$engineId] started successfully")
        println(s"  Settings: Depth=${config.depth}, Threads=${config.threads}, Hash=${config.hashSizeMB}MB")
        true
      } else {
        println(s"✗ Stockfish engine [$engineId] failed to start")
        stop()
        false
      }
    } catch {
      case e: Exception =>
        println(s"✗ Failed to start Stockfish: ${e.getMessage}")
        println(s"   Make sure Stockfish is installed at: ${config.stockfishPath}")
        println(s"   Install with: brew install stockfish")
        false
    }
  }
  
  /**
   * Stop the engine
   */
  def stop(): Unit = {
    if (analyzing) {
      stopAnalysis()
    }
    sendCommand("quit")
    Thread.sleep(100)
    process.foreach(_.destroy())
    process = None
    output = None
    input = None
    isReady = false
  }
  
  /**
   * Analyze a position given in FEN notation
   * Uses depth and multiPV from config if not specified
   */
  def analyzePosition(fen: String, depth: Int = -1, multiPV: Int = -1): Unit = {
    // Extract side to move from FEN (second field: "w" or "b")
    val fenParts = fen.split(" ")
    if (fenParts.length >= 2) {
      sideToMove = fenParts(1)
    }
    
    // Stop current analysis if running
    if (analyzing) {
      sendCommand("stop")
      Thread.sleep(100) // Give engine time to finish current analysis
    }
    
    // Wait for engine to be ready
    var retries = 0
    while (!isReady && retries < 50) {
      sendCommand("isready")
      Thread.sleep(20)
      retries += 1
    }
    
    val actualDepth = if (depth > 0) depth else config.depth
    val actualMultiPV = if (multiPV > 0) multiPV else config.multiPV
     
    // Clear previous analysis before starting new one
    lastAnalysis = None
    currentVariations.clear()
    
    sendCommand(s"setoption name MultiPV value $actualMultiPV")
    sendCommand(s"position fen $fen")
    sendCommand(s"go depth $actualDepth")
    analyzing = true
    isReady = false  // Engine will be busy during analysis
  }
  
  /**
   * Update engine configuration and restart if running
   */
  def updateConfig(newConfig: EngineConfig): Unit = {
    val wasRunning = isRunning
    if (wasRunning) {
      stop()
    }
    config = newConfig
    if (wasRunning) {
      start()
    }
  }
  
  /**
   * Stop current analysis
   */
  def stopAnalysis(): Unit = {
    if (analyzing) {
      sendCommand("stop")
      analyzing = false
    }
  }
  
  /**
   * Get best move for a position
   */
  def getBestMove(fen: String, timeMs: Int = 1000): Future[Option[String]] = {
    val promise = Promise[Option[String]]()
    
    if (!isReady) {
      promise.success(None)
      return promise.future
    }
    
    var bestMove: Option[String] = None
    val callback = (analysis: EngineAnalysis) => {
      if (analysis.bestMove.isDefined && !promise.isCompleted) {
        bestMove = analysis.bestMove
      }
    }
    
    addAnalysisCallback(callback)
    
    Future {
      sendCommand(s"position fen $fen")
      sendCommand(s"go movetime $timeMs")
      Thread.sleep(timeMs + 200)
      removeAnalysisCallback(callback)
      promise.success(bestMove)
    }
    
    promise.future
  }
  
  private def sendCommand(command: String): Unit = {
    input.foreach { writer =>
      writer.println(command)
      writer.flush()
    }
  }
  
  private def processLine(line: String): Unit = {
    if (line.startsWith("uciok")) {
      // Engine initialized
    } else if (line.startsWith("readyok")) {
      isReady = true
    } else if (line.startsWith("info")) {
      parseInfoLine(line)
    } else if (line.startsWith("bestmove")) {
      parseBestMove(line)
    }
  }
  
  private def parseInfoLine(line: String): Unit = {
    val parts = line.split("\\s+")
    var depth = 0
    var score: Option[Int] = None
    var mate: Option[Int] = None
    var pv = List[String]()
    var nps = 0L
    var multiPvIndex = 1  // Default to first variation
    
    var i = 1
    while (i < parts.length) {
      parts(i) match {
        case "depth" =>
          i += 1
          if (i < parts.length) depth = parts(i).toIntOption.getOrElse(0)
        case "multipv" =>
          i += 1
          if (i < parts.length) multiPvIndex = parts(i).toIntOption.getOrElse(1)
        case "score" =>
          i += 1
          if (i < parts.length) {
            parts(i) match {
              case "cp" =>
                i += 1
                if (i < parts.length) score = parts(i).toIntOption
              case "mate" =>
                i += 1
                if (i < parts.length) mate = parts(i).toIntOption
              case _ =>
            }
          }
        case "nps" =>
          i += 1
          if (i < parts.length) nps = parts(i).toLongOption.getOrElse(0L)
        case "pv" =>
          i += 1
          while (i < parts.length && !parts(i).startsWith("info")) {
            pv = pv :+ parts(i)
            i += 1
          }
          i -= 1  // Back up one since the while loop will increment
        case _ =>
      }
      i += 1
    }
    
    // Flip score perspective if Black to move
    // Stockfish gives scores from side-to-move perspective, but we want White's perspective
    if (sideToMove == "b") {
      score = score.map(s => -s)
      mate = mate.map(m => -m)
    }
    
    if (depth > 0 && (score.isDefined || mate.isDefined) && pv.nonEmpty) {
      // Store this variation
      currentVariations(multiPvIndex) = Variation(score, mate, pv, multiPvIndex)
      currentDepth = depth
      
      // If this is the last variation for this depth (multiPV count), send all variations
      // We send on each update to keep UI responsive
      val sortedVariations = currentVariations.values.toList.sortBy(_.multiPvIndex)
      val bestVariation = sortedVariations.headOption
      
      // NOTE: bestMove is NOT set here - it comes from "bestmove" line in parseBestMove
      // pv.headOption is NOT the same as bestMove - it's just the first move of the PV at current depth
      val analysis = EngineAnalysis(
        depth,
        bestVariation.flatMap(_.score),
        bestVariation.flatMap(_.mate),
        None,  // bestMove only set by parseBestMove
        bestVariation.map(_.pv).getOrElse(List()),
        nps,
        sortedVariations
      )
      lastAnalysis = Some(analysis)
      notifyCallbacks(analysis)
    }
  }
  
  private def parseBestMove(line: String): Unit = {
    val parts = line.split("\\s+")
    if (parts.length >= 2) {
      val bestMove = parts(1)
      // Preserve last evaluation score/mate/variations when sending bestmove
      val analysis = lastAnalysis match {
        case Some(last) =>
          // Update with bestMove but keep full PV from last analysis
          EngineAnalysis(last.depth, last.score, last.mate, Some(bestMove), last.pv, last.nps, last.variations)
        case None =>
          // Fallback if no previous analysis (shouldn't happen normally)
          EngineAnalysis(0, None, None, Some(bestMove), List(bestMove), 0, List())
      }
      notifyCallbacks(analysis)
      analyzing = false
      // Clear variations for next analysis
      currentVariations.clear()
    }
  }
  
  private def notifyCallbacks(analysis: EngineAnalysis): Unit = {
    analysisCallbacks.foreach(callback => {
      try {
        callback(analysis)
      } catch {
        case e: Exception =>
          println(s"Error in analysis callback: ${e.getMessage}")
      }
    })
  }
  
  def isRunning: Boolean = process.isDefined
}
