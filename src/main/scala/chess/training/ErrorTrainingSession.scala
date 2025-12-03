package chess.training

import chess.analysis.{MoveClassification, PositionEvaluation}

/**
 * Represents a position where the player made a mistake that can be trained.
 * 
 * @param fen The FEN string of the position before the mistake
 * @param playerColor The color that made the mistake ("white" or "black")
 * @param playedMove The move that was played (the mistake)
 * @param bestMove The best move according to Stockfish (in UCI format, e.g., "e2e4")
 * @param classification The classification of the played move (Mistake, Blunder, etc.)
 * @param evalBefore Evaluation before the mistake
 * @param evalAfter Evaluation after the mistake
 * @param gameIndex Which game this position came from (0-indexed)
 * @param moveNumber The move number in the game
 * @param previousMove The opponent's move that led to this position (UCI format, e.g., "e7e5")
 */
case class ErrorPosition(
  fen: String,
  playerColor: String,
  playedMove: String,
  bestMove: String,
  classification: MoveClassification,
  evalBefore: PositionEvaluation,
  evalAfter: PositionEvaluation,
  gameIndex: Int,
  moveNumber: Int,
  previousMove: Option[String] = None
) {
  /** Get the centipawn loss of this mistake */
  def centipawnLoss: Double = {
    val before = evalBefore.toNumeric * 100
    val after = evalAfter.toNumeric * 100
    if (playerColor == "white") {
      before - after
    } else {
      after - before
    }
  }
  
  /** Check if this is a critical mistake (blunder or missed win) */
  def isCritical: Boolean = classification match {
    case MoveClassification.Blunder => true
    case _ => false
  }
}

/**
 * Difficulty settings for error training.
 * 
 * @param movesToPlay How many moves the player needs to play from the error position
 * @param maxWinProbabilityDrop Maximum allowed drop in win probability (0.0 to 1.0, e.g., 0.15 = 15%)
 * @param minClassification Minimum classification required for the first move (only for Easy mode)
 * @param useWinProbability Whether to use win probability check (false = use classification for Easy)
 */
case class TrainingDifficulty(
  movesToPlay: Int,
  maxWinProbabilityDrop: Double,
  minClassification: MoveClassification,
  useWinProbability: Boolean = true
) {
  def name: String = this match {
    case TrainingDifficulty.Easy => "Helppo"
    case TrainingDifficulty.Medium => "Keskitaso"
    case TrainingDifficulty.Hard => "Vaikea"
    case TrainingDifficulty.Expert => "Ekspertti"
    case _ => s"Mukautettu ($movesToPlay siirtoa)"
  }
  
  def description: String = this match {
    case TrainingDifficulty.Easy => "1 siirto, vähintään 'Hyvä' siirto"
    case TrainingDifficulty.Medium => s"$movesToPlay siirtoa, max ${(maxWinProbabilityDrop * 100).toInt}% pudotus"
    case TrainingDifficulty.Hard => s"$movesToPlay siirtoa, max ${(maxWinProbabilityDrop * 100).toInt}% pudotus"
    case TrainingDifficulty.Expert => s"$movesToPlay siirtoa, max ${(maxWinProbabilityDrop * 100).toInt}% pudotus"
    case _ => s"$movesToPlay siirtoa"
  }
}

object TrainingDifficulty {
  // Easy: 1 move, must be at least "Good" classification
  val Easy = TrainingDifficulty(
    movesToPlay = 1,
    maxWinProbabilityDrop = 0.15,  // Not used in Easy mode
    minClassification = MoveClassification.Good,
    useWinProbability = false  // Use classification instead
  )
  
  // Medium: 3 moves, max 15% win probability drop
  val Medium = TrainingDifficulty(
    movesToPlay = 3,
    maxWinProbabilityDrop = 0.15,
    minClassification = MoveClassification.Excellent,
    useWinProbability = true
  )
  
  // Hard: 5 moves, max 15% win probability drop
  val Hard = TrainingDifficulty(
    movesToPlay = 5,
    maxWinProbabilityDrop = 0.15,
    minClassification = MoveClassification.Best,
    useWinProbability = true
  )
  
  // Expert: 7 moves, max 15% win probability drop
  val Expert = TrainingDifficulty(
    movesToPlay = 7,
    maxWinProbabilityDrop = 0.15,
    minClassification = MoveClassification.Best,
    useWinProbability = true
  )
  
  val all: Seq[TrainingDifficulty] = Seq(Easy, Medium, Hard, Expert)
}

/**
 * Result of a single training attempt.
 */
case class TrainingAttemptResult(
  errorPosition: ErrorPosition,
  movesPlayed: Seq[String],  // Moves played by the user (UCI format)
  stockfishMoves: Seq[String],  // Stockfish's responses
  evaluations: Seq[PositionEvaluation],  // Evaluation after each pair of moves
  success: Boolean,  // Did the user maintain/improve the evaluation?
  finalEvalDrop: Double  // How much the eval dropped (negative = improved)
)

/**
 * Statistics for an error training session.
 */
case class TrainingSessionStats(
  totalPositions: Int,
  attemptedPositions: Int,
  successfulAttempts: Int,
  averageEvalChange: Double,
  mistakesCorrected: Int,
  blundersCorrected: Int,
  // Simple counters for UI
  correctCount: Int = 0,
  totalAttempts: Int = 0,
  positionsCompleted: Int = 0
) {
  def successRate: Double = 
    if (totalAttempts == 0) 0.0 
    else correctCount.toDouble / totalAttempts * 100
    
  def completionRate: Double =
    if (totalPositions == 0) 0.0
    else attemptedPositions.toDouble / totalPositions * 100
}

/**
 * Manages an error training session across multiple games.
 */
class ErrorTrainingSession {
  private var errorPositions: Vector[ErrorPosition] = Vector.empty
  private var currentIndex: Int = 0
  private var attempts: Vector[TrainingAttemptResult] = Vector.empty
  private var difficulty: TrainingDifficulty = TrainingDifficulty.Easy
  
  // Simple counters for quick stats
  private var simpleCorrectCount: Int = 0
  private var simpleTotalAttempts: Int = 0
  private var simplePositionsCompleted: Int = 0
  
  /** Add error positions from analyzed games */
  def addErrorPositions(positions: Seq[ErrorPosition]): Unit = {
    errorPositions = errorPositions ++ positions
  }
  
  /** Clear all positions and reset session */
  def reset(): Unit = {
    currentIndex = 0
    attempts = Vector.empty
    simpleCorrectCount = 0
    simpleTotalAttempts = 0
    simplePositionsCompleted = 0
  }
  
  /** Clear everything including positions */
  def resetAll(): Unit = {
    errorPositions = Vector.empty
    reset()
  }
  
  /** Set the training difficulty */
  def setDifficulty(diff: TrainingDifficulty): Unit = {
    difficulty = diff
  }
  
  def getDifficulty: TrainingDifficulty = difficulty
  
  /** Get total number of error positions */
  def totalPositions: Int = errorPositions.size
  
  /** Get current position index (0-based) */
  def currentPositionIndex: Int = currentIndex
  
  /** Get current error position, if any */
  def currentPosition: Option[ErrorPosition] = {
    if (currentIndex >= 0 && currentIndex < errorPositions.size) {
      Some(errorPositions(currentIndex))
    } else None
  }
  
  /** Move to next position */
  def nextPosition(): Option[ErrorPosition] = {
    if (currentIndex < errorPositions.size - 1) {
      currentIndex += 1
      currentPosition
    } else None
  }
  
  /** Move to previous position */
  def previousPosition(): Option[ErrorPosition] = {
    if (currentIndex > 0) {
      currentIndex -= 1
      currentPosition
    } else None
  }
  
  /** Jump to specific position */
  def goToPosition(index: Int): Option[ErrorPosition] = {
    if (index >= 0 && index < errorPositions.size) {
      currentIndex = index
      currentPosition
    } else None
  }
  
  /** Record a training attempt result */
  def recordAttempt(result: TrainingAttemptResult): Unit = {
    attempts = attempts :+ result
  }
  
  /** Simple method to record correct/incorrect attempt for UI */
  def recordSimpleAttempt(correct: Boolean): Unit = {
    simpleTotalAttempts += 1
    if (correct) {
      simpleCorrectCount += 1
      simplePositionsCompleted += 1
    }
  }
  
  /** Get all attempts for current position */
  def attemptsForCurrentPosition: Seq[TrainingAttemptResult] = {
    currentPosition match {
      case Some(pos) => attempts.filter(_.errorPosition == pos)
      case None => Seq.empty
    }
  }
  
  /** Check if current position has been successfully completed */
  def isCurrentPositionCompleted: Boolean = {
    attemptsForCurrentPosition.exists(_.success)
  }
  
  /** Get session statistics */
  def getStats: TrainingSessionStats = {
    val uniquePositionsAttempted = attempts.map(_.errorPosition).distinct.size
    val successfulPositions = errorPositions.filter { pos =>
      attempts.filter(_.errorPosition == pos).exists(_.success)
    }
    
    val avgEvalChange = if (attempts.isEmpty) 0.0 else {
      attempts.map(_.finalEvalDrop).sum / attempts.size
    }
    
    val mistakesCorrected = successfulPositions.count(_.classification == MoveClassification.Mistake)
    val blundersCorrected = successfulPositions.count(_.classification == MoveClassification.Blunder)
    
    TrainingSessionStats(
      totalPositions = errorPositions.size,
      attemptedPositions = uniquePositionsAttempted,
      successfulAttempts = successfulPositions.size,
      averageEvalChange = avgEvalChange,
      mistakesCorrected = mistakesCorrected,
      blundersCorrected = blundersCorrected,
      correctCount = simpleCorrectCount,
      totalAttempts = simpleTotalAttempts,
      positionsCompleted = simplePositionsCompleted
    )
  }
  
  /** Get all error positions sorted by severity */
  def getPositionsBySeverity: Seq[ErrorPosition] = {
    errorPositions.sortBy { pos =>
      pos.classification match {
        case MoveClassification.Blunder => 0
        case MoveClassification.Mistake => 1
        case MoveClassification.Inaccuracy => 2
        case _ => 3
      }
    }
  }
  
  /** Get only uncompleted positions */
  def getUncompletedPositions: Seq[ErrorPosition] = {
    errorPositions.filterNot { pos =>
      attempts.filter(_.errorPosition == pos).exists(_.success)
    }
  }
  
  /** Filter positions by classification */
  def filterByClassification(classifications: Set[MoveClassification]): Seq[ErrorPosition] = {
    errorPositions.filter(p => classifications.contains(p.classification))
  }
}
