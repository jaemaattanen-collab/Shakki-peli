package chess.pgn

import chess.controllers.GameController
import chess.board.Board
import org.scalatest.funsuite.AnyFunSuite

class PGNGameReplayerSpec extends AnyFunSuite {

  test("replay rebuilds controller state from mainline moves") {
    val controller = new GameController()
    val board = new Board(controller)
    controller.board = Some(board)

    val sanMoves = Seq("e4", "e5", "Nf3", "Nc6")
    val game = PGNGame(
      headers = Map.empty,
      moves = PGNMoveBuilder.fromSanSequence(startingMoveNumber = 1, startingColor = "white", sanMoves = sanMoves),
      result = Some("1/2-1/2"),
      variations = Nil
    )

    val result = PGNGameReplayer.replay(game, controller, board)
    assert(result.isRight, s"Replay failed: ${result.left.getOrElse("unknown error")}")
    assert(controller.moveHistoryManager.getMoveNotations == sanMoves.toList)
    assert(controller.winner.contains("draw"))
  }

  test("replay returns descriptive error for illegal SAN") {
    val controller = new GameController()
    val board = new Board(controller)
    controller.board = Some(board)

    val invalidGame = PGNGame(
      headers = Map.empty,
      moves = List(
        PGNMove(moveNumber = 1, whiteMove = Some("e4"), blackMove = Some("e6")),
        PGNMove(moveNumber = 2, whiteMove = Some("??"), blackMove = None)
      ),
      result = None,
      variations = Nil
    )

    val result = PGNGameReplayer.replay(invalidGame, controller, board)
    assert(result.isLeft, "Replay should fail for illegal SAN sequence")
    val errorMessage = result.left.getOrElse("")
    assert(errorMessage.contains("??"))
  }
}
