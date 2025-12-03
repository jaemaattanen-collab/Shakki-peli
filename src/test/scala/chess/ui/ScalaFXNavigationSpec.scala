package chess.ui

import org.scalatest.funsuite.AnyFunSuite
import chess.controllers.GameController
import chess.board.Board
/**
 * Simple non-UI regression test to ensure that move history replay produces
 * consistent board state. This doesn't hit ScalaFX directly, but validates the
 * core assumption used by ScalaFX navigation helpers: that starting from a
 * reset Board and replaying moves in order reconstructs the same final
 * positions.
 */
class ScalaFXNavigationSpec extends AnyFunSuite {

  test("replaying moves from history reconstructs final positions") {
    val controller = new GameController()
    val board = new Board(controller)
    controller.board = Some(board)

    // Play a short deterministic sequence of legal opening moves
    // e2e4, e7e5, g1f3, b8c6
    def moveByName(from: String, to: String): Unit = {
      val fromSq = board.getSquareByName(from).getOrElse(fail(s"No square $from"))
      val toSq = board.getSquareByName(to).getOrElse(fail(s"No square $to"))
      val piece = fromSq.occupiedBy.getOrElse(fail(s"No piece on $from"))
      assert(controller.handleAction(piece, toSq), s"Move $from$to should be legal")
    }

    moveByName("e2", "e4")
    moveByName("e7", "e5")
    moveByName("g1", "f3")
    moveByName("b8", "c6")

    val history = controller.moveHistoryManager.getMoveHistory
    assert(history.size >= 4)

    // Snapshot final piece positions
    val finalPositions = board.squares.flatten.flatMap { sq =>
      sq.occupiedBy.map(p => sq.name -> p)
    }.toMap

    // Reset board and replay moves
    board.resetBoard()
    history.foreach { m =>
      assert(controller.replayMove(m), s"Replaying move ${m.from.name}${m.to.name} should succeed")
    }

    val replayedPositions = board.squares.flatten.flatMap { sq =>
      sq.occupiedBy.map(p => sq.name -> p)
    }.toMap

    assert(finalPositions.keySet == replayedPositions.keySet)
  }

  test("replaying moves does not duplicate history") {
    val controller = new GameController()
    val board = new Board(controller)
    controller.board = Some(board)

    def moveByName(from: String, to: String): Unit = {
      val fromSq = board.getSquareByName(from).getOrElse(fail(s"No square $from"))
      val toSq = board.getSquareByName(to).getOrElse(fail(s"No square $to"))
      val piece = fromSq.occupiedBy.getOrElse(fail(s"No piece on $from"))
      assert(controller.handleAction(piece, toSq), s"Move $from$to should be legal")
    }

    // Play a few opening moves
    moveByName("e2", "e4")
    moveByName("e7", "e5")
    moveByName("g1", "f3")

    val historyBefore = controller.moveHistoryManager.getMoveHistory
    val sizeBefore = historyBefore.size
    assert(sizeBefore >= 3)

    // Reset board and replay using replayMove
    board.resetBoard()
    historyBefore.foreach { m =>
      assert(controller.replayMove(m), s"Replaying move ${m.from.name}${m.to.name} should succeed")
    }

    val historyAfter = controller.moveHistoryManager.getMoveHistory
    val sizeAfter = historyAfter.size

    // Navigation-style replay must not append new moves to history
    assert(sizeAfter == sizeBefore, s"History size changed from $sizeBefore to $sizeAfter after replay")
  }

  test("variation line with final castle remains playable") {
    val controller = new GameController()
    val board = new Board(controller)
    controller.board = Some(board)

    def moveByName(from: String, to: String): Unit = {
      val fromSq = board.getSquareByName(from).getOrElse(fail(s"No square $from"))
      val toSq = board.getSquareByName(to).getOrElse(fail(s"No square $to"))
      val piece = fromSq.occupiedBy.getOrElse(fail(s"No piece on $from"))
      assert(controller.handleAction(piece, toSq), s"Move $from$to should be legal")
    }

    moveByName("e2", "e4")
    moveByName("e7", "e5")
    moveByName("g1", "f3")
    moveByName("b8", "c6")
    moveByName("f1", "c4")
    moveByName("g8", "f6")
    moveByName("d2", "d3")
    moveByName("g7", "g6")

    // White castles
    moveByName("e1", "g1")

    moveByName("f8", "g7")
    moveByName("c1", "g5")

    // Black castles kingside; must succeed for the variation to complete
    moveByName("e8", "g8")
  }
}