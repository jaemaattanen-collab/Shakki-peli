package chess.analysis


/**
 * Configuration for Stockfish engine
 */
case class EngineConfig(
  stockfishPath: String = "/opt/homebrew/bin/stockfish",
  depth: Int = 22,
  threads: Int = 2,
  hashSizeMB: Int = 128,
  multiPV: Int = 3,  // Number of principal variations to analyze (default: 3 for arrows)
  moveTime: Int = 1000  // Time per move in milliseconds for quick analysis
) {
  
  def isValid: Boolean = {
    depth > 0 && depth <= 100 &&
    threads > 0 && threads <= 16 &&
    hashSizeMB > 0 && hashSizeMB <= 4096 &&
    multiPV > 0 && multiPV <= 5 &&
    moveTime > 0
  }
  
  def depthDescription: String =
    depth match
      case d if d <= 10 => "Fast (weak)"
      case d if d <= 15 => "Medium"
      case d if d <= 20 => "Strong"
      case d if d <= 25 => "Very Strong"
      case _ => "Maximum (slow)"
}

object EngineConfig {
  // Preset configurations
  val FAST = EngineConfig(depth = 12, threads = 1, hashSizeMB = 64, multiPV = 3)
  val BALANCED = EngineConfig(depth = 18, threads = 2, hashSizeMB = 128, multiPV = 3)
  val STRONG = EngineConfig(depth = 22, threads = 4, hashSizeMB = 256, multiPV = 3)
  val MAXIMUM = EngineConfig(depth = 30, threads = 4, hashSizeMB = 512, multiPV = 3)
  
  val DEFAULT = BALANCED
}
