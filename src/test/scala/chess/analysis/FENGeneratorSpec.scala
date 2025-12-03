package chess.analysis

import org.scalatest.funsuite.AnyFunSuite
import chess.board.Board
import chess.controllers.GameController

class FENGeneratorSpec extends AnyFunSuite {

  test("FENGenerator should generate correct starting position") {
    val controller = new GameController()
    val board = new Board(controller)
    controller.board = Some(board)
    
    // Set fullmove counter to 1 (standard starting position)
    controller.fullmoveNumber = 1
    
    val fen = FENGenerator.generateFEN(board, controller)
    
    // Starting position FEN
    val expectedFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    assert(fen == expectedFEN, s"Expected: $expectedFEN\nGot: $fen")
  }
  
  test("FENGenerator should generate FEN after e4") {
    val controller = new GameController()
    val board = new Board(controller)
    controller.board = Some(board)
    
    // Make the move e4
    val e2 = board.squares(6)(4)  // e2
    val e4 = board.squares(4)(4)  // e4
    
    e2.occupiedBy.foreach { pawn =>
      pawn.moveTo(e4)
      pawn.hasMoved = true
    }
    
    controller.currentPlayer = "black"
    controller.halfmoveClock = 0
    controller.fullmoveNumber = 1
    
    val fen = FENGenerator.generateFEN(board, controller)
    
    // After e4
    val expectedFEN = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
    assert(fen == expectedFEN, s"Expected: $expectedFEN\nGot: $fen")
  }
  
  test("FENGenerator should parse FEN components") {
    val fen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
    val parsed = FENGenerator.parseFEN(fen)
    
    assert(parsed("position") == "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR")
    assert(parsed("activeColor") == "b")
    assert(parsed("castling") == "KQkq")
    assert(parsed("enPassant") == "e3")
    assert(parsed("halfmove") == "0")
    assert(parsed("fullmove") == "1")
  }
}
