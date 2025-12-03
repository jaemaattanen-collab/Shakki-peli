package chess.pgn

import chess.controllers.{Move, MoveHistoryManager}
import org.scalatest.funsuite.AnyFunSuite

class DepthIndexedPGNBuilderSpec extends AnyFunSuite {

  test("builder extracts mainline and variations from depth-indexed history") {
    val history = new MoveHistoryManager()

    val e4 = move(tob = 1000L, parent = -1L, depth = 1, san = "e4")
    val e5 = move(tob = 2000L, parent = e4.tob, depth = 2, san = "e5")
    val nf3 = move(tob = 3000L, parent = e5.tob, depth = 3, san = "Nf3")
    val nc6 = move(tob = 4000L, parent = nf3.tob, depth = 4, san = "Nc6")

    val d4 = move(tob = 1500L, parent = -1L, depth = 1, san = "d4")
    val c5 = move(tob = 2500L, parent = e4.tob, depth = 2, san = "c5")
    val nc3 = move(tob = 2600L, parent = c5.tob, depth = 3, san = "Nc3")
    val bc4 = move(tob = 3100L, parent = e5.tob, depth = 3, san = "Bc4")

    Seq(e4, e5, nf3, nc6, d4, c5, nc3, bc4).foreach(history.addMove)

    val result = DepthIndexedPGNBuilder.build(history)

    assert(result.mainline.length == 2)

    val firstMove = result.mainline.head
    assert(firstMove.moveNumber == 1)
    assert(firstMove.whiteMove.contains("e4"))
    assert(firstMove.blackMove.contains("e5"))
    assert(firstMove.variations.length == 1)

    val blackVariation = firstMove.variations.head
    assert(blackVariation.originMoveNumber.contains(1))
    assert(blackVariation.originColor.contains("black"))
    assert(blackVariation.startingColor == "black")
    assert(blackVariation.moves.head.blackMove.contains("c5"))
    assert(blackVariation.moves.lift(1).flatMap(_.whiteMove).contains("Nc3"))

    val secondMove = result.mainline(1)
    assert(secondMove.moveNumber == 2)
    assert(secondMove.whiteMove.contains("Nf3"))
    assert(secondMove.blackMove.contains("Nc6"))
    assert(secondMove.variations.exists(_.originColor.contains("white")))
    val whiteVariation = secondMove.variations.find(_.originColor.contains("white")).get
    assert(whiteVariation.moves.head.whiteMove.contains("Bc4"))

    assert(result.rootVariations.length == 1)
    val rootVariation = result.rootVariations.head
    // Root variations are now properly tagged with their origin (move 1 white in this case)
    assert(rootVariation.originMoveNumber.contains(1))
    assert(rootVariation.originColor.contains("white"))
    assert(rootVariation.startingColor == "white")
    assert(rootVariation.moves.head.whiteMove.contains("d4"))
  }

  private def move(tob: Long, parent: Long, depth: Int, san: String): Move =
    Move(
      piece = null,
      from = null,
      to = null,
      capturedPiece = None,
      promotedPieceType = None,
      tob = tob,
      parentTob = parent,
      halfmoveDistanceFromStart = depth,
      notation = Some(san)
    )
}
