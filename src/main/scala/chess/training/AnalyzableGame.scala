package chess.training

import chess.controllers.Move
import chess.pgn.PGNGame

/**
 * Represents a game that has been prepared for analysis.
 * Contains the parsed PGN data plus the Move objects and FEN positions
 * that are needed for Stockfish analysis.
 */
case class AnalyzableGame(
  pgnGame: PGNGame,
  moves: List[Move],
  fens: List[String],
  gameIndex: Int
) {
  /** Get white player name */
  def whiteName: String = pgnGame.headers.getOrElse("White", "Unknown")
  
  /** Get black player name */
  def blackName: String = pgnGame.headers.getOrElse("Black", "Unknown")
  
  /** Get game result */
  def result: String = pgnGame.result.getOrElse("*")
  
  /** Get event name */
  def event: String = pgnGame.headers.getOrElse("Event", "Unknown")
  
  /** Get date */
  def date: String = pgnGame.headers.getOrElse("Date", "????.??.??")
  
  /** Number of moves in the game */
  def moveCount: Int = moves.length
  
  /** Check if a player name matches (case-insensitive partial match) */
  def hasPlayer(name: String): Boolean = {
    val lowerName = name.toLowerCase
    whiteName.toLowerCase.contains(lowerName) || 
    blackName.toLowerCase.contains(lowerName)
  }
  
  /** Determine which color the given player was */
  def playerColor(name: String): Option[String] = {
    val lowerName = name.toLowerCase
    if (whiteName.toLowerCase.contains(lowerName)) Some("white")
    else if (blackName.toLowerCase.contains(lowerName)) Some("black")
    else None
  }
  
  /** Get a short description of the game */
  def shortDescription: String = {
    s"$whiteName vs $blackName ($date) - $result"
  }
}
