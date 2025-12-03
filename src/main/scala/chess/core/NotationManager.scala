package chess.core


/**
 * Simple move notation manager - logs moves in basic algebraic notation
 * No complex PGN parsing or variations - just straightforward move recording
 */
class NotationManager {
  
  // Simple list of moves in algebraic notation
  private var moves: List[String] = List.empty
  
  /**
   * Add a move to the notation
   */
  def addMove(moveNotation: String): Unit = {
    moves = moves :+ moveNotation
  }
  
  /**
   * Trim the notation list to the given size (number of half-moves).
   * If the requested size is larger than the current list, the call is ignored.
   */
  def trimToSize(size: Int): Unit = {
    if (size <= 0) {
      moves = List.empty
    } else if (size < moves.length) {
      moves = moves.take(size)
    }
  }

  /**
   * Get all moves
   */
  def getMoves(): List[String] = moves
  
  /**
   * Get the full notation as a simple string
   */
  def getNotation(): String = 
    moves.mkString(" ")
  
  /**
   * Clear all moves
   */
  def clear(): Unit = {
    moves = List.empty
  }
  
  /**
   * Get the number of moves
   */
  def getMoveCount(): Int = moves.length
  
  // Stub methods for compatibility - these do nothing in the simplified version
  
  def reset(): Unit = 
    clear()
}
