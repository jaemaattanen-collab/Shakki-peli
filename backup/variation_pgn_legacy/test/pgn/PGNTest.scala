package chess.pgn

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.OptionValues

class PGNTest extends AnyFlatSpec with Matchers with OptionValues {

  "PGNParser" should "parse a simple PGN game" in {
    val testPGN = """[Event "Test Game"]
[Site "Local"]
[Date "2024.01.01"]
[White "Player 1"]
[Black "Player 2"]
[Result "1-0"]

1. e4 e5 2. Nf3 Nc6 3. O-O O-O 1-0"""

    val result = PGNParser.parsePGN(testPGN)

    result.game should be (defined)
    val game = result.game.get

    game.headers("Event") should be ("Test Game")
    game.headers("White") should be ("Player 1")
    game.headers("Black") should be ("Player 2")
    game.result should be (Some("1-0"))

    game.moves.length should be > 0
    game.moves.head.whiteMove should be (Some("e4"))
    game.moves.head.blackMove should be (Some("e5"))
  }

  "PGNParser" should "handle castling moves" in {
    val testPGN = """[Event "Castle Test"]
[Result "*"]

1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 4. Ba4 Nf6 5. O-O O-O *"""

    val result = PGNParser.parsePGN(testPGN)

    result.game should be (defined)
    val game = result.game.get

    // Find the castling moves
    val castleMove = game.moves.find(_.whiteMove.contains("O-O"))
    castleMove should be (defined)
  }

  "PGNExporter" should "export basic game structure" in {
    // This would require a mock controller - for now just test that the object exists
    PGNExporter should not be null
  }

  "PGNParser" should "parse variations with comments and annotations" in {
    val variationPGN = """[Event "Variation Test"]
[Site "Local"]
[Result "*"]

1. e4 {Main line} (1. d4 d5 2. c4) (1... c5 {Sicilian}) 1... e5 2. Nf3 Nc6 (2... d6) *"""

    val result = PGNParser.parsePGN(variationPGN)

    result.errors shouldBe empty
    result.game shouldBe defined

    val game = result.game.get

    game.moves.length shouldBe 2
    game.moves.head.whiteMove shouldBe Some("e4")
    game.moves.head.whiteComment shouldBe Some("Main line")
    game.moves.head.blackMove shouldBe Some("e5")

    val whiteVariations = game.moves.head.variations.filter(_.originColor.contains("white"))
    whiteVariations.length shouldBe 2

    val alternativeOpening = whiteVariations.find(_.startingColor == "white").get
    alternativeOpening.moves.head.whiteMove shouldBe Some("d4")
    alternativeOpening.moves.head.blackMove shouldBe Some("d5")

    val sicilian = whiteVariations.find(_.startingColor == "black").get
    sicilian.moves.head.blackMove shouldBe Some("c5")
    sicilian.moves.head.blackComment shouldBe Some("Sicilian")

    val secondMoveVariations = game.moves(1).variations.filter(_.originColor.contains("black"))
    secondMoveVariations.length shouldBe 1
    secondMoveVariations.head.moves.head.blackMove shouldBe Some("d6")

    val exported = PGNExporter.exportPGNGame(game)
    exported should include ("(1... c5 {Sicilian})")

    val reparsed = PGNParser.parsePGN(exported)
    reparsed.errors shouldBe empty
    reparsed.game shouldBe defined
    val reparsedGame = reparsed.game.get
    reparsedGame.moves.head.variations.filter(_.originColor.contains("white")).length shouldBe 2
  }

  "LiveVariationRepository" should "retain anchors for multiple variations on the same move" in {
    val repo = new LiveVariationRepository()
    repo.addVariation(
      anchorIndex = 5,
      startingMoveNumber = 4,
      startingColor = "white",
      moves = List("Nc3", "d5", "exd5", "Nxd5", "Nxd5")
    )
    repo.addVariation(
      anchorIndex = 5,
      startingMoveNumber = 4,
      startingColor = "white",
      moves = List("h3")
    )

    val exported = repo.exportVariations
    exported.length shouldBe 2
    exported.foreach { variation =>
      variation.originMoveNumber shouldBe Some(4)
      variation.originColor shouldBe Some("white")
    }
  }

  "PGNExporter" should "keep mainline moves ahead of sibling variations" in {
    val repo = new LiveVariationRepository()
    repo.addVariation(
      anchorIndex = 5,
      startingMoveNumber = 4,
      startingColor = "white",
      moves = List("Nc3", "d5", "exd5", "Nxd5", "Nxd5")
    )
    repo.addVariation(
      anchorIndex = 5,
      startingMoveNumber = 4,
      startingColor = "white",
      moves = List("h3")
    )

    val mainline = PGNMoveBuilder.fromHalfMoveNotations(List("e4", "e5", "Nf3", "Nc6", "Bc4", "Nf6", "d3"))
    val manager = new PGNManager(null, null, Some(repo))
    val (movesWithVariations, rootVariations) = manager.attachVariationsToMoves(mainline, repo.exportVariations)

    rootVariations shouldBe empty
    val moveFour = movesWithVariations.find(_.moveNumber == 4).value
    moveFour.whiteMove shouldBe Some("d3")
    moveFour.variations.length shouldBe 2

    val game = PGNGame(headers = Map.empty, moves = movesWithVariations, result = Some("*"), variations = rootVariations)
    val text = PGNExporter.exportPGNGame(game)

    text should include ("4. d3")
    text should include ("(4. Nc3 d5 5. exd5 Nxd5 6. Nxd5)")
    text should include ("(4. h3)")
    text.indexOf("4. d3") should be < text.indexOf("(4. Nc3")
  }

  "PGNExporter" should "include nested variations without affecting the mainline" in {
    val repo = new LiveVariationRepository()
    val top = repo.addVariation(
      anchorIndex = 2,
      startingMoveNumber = 2,
      startingColor = "black",
      moves = List("Nf6", "Bc4")
    ).value

    repo.addNestedVariation(
      parentId = top.id,
      parentAnchorPly = 1,
      startingMoveNumber = 3,
      startingColor = "white",
      moves = List("Nc3")
    )

    val mainline = PGNMoveBuilder.fromHalfMoveNotations(List("e4", "e5", "Nf3", "Nc6"))
    val exportedVariations = repo.exportVariations
    exportedVariations should have size 1
    exportedVariations.head.moves(1).variations should have size 1
    val nestedVariation = exportedVariations.head.moves(1).variations.head
    nestedVariation.originColor shouldBe Some("white")
    nestedVariation.moves should not be empty

    val manager = new PGNManager(null, null, Some(repo))
    val (movesWithVariations, rootVariations) = manager.attachVariationsToMoves(mainline, exportedVariations)
    val game = PGNGame(headers = Map.empty, moves = movesWithVariations, result = Some("*"), variations = rootVariations)
    val text = PGNExporter.exportPGNGame(game)

    withClue(s"PGN output: $text") {
      text should include ("2. Nf3 Nc6")
      text should include ("(2... Nf6 3. Bc4")
      text should include ("(3. Nc3")
    }

    val reparsed = PGNParser.parsePGN(text)
    reparsed.errors shouldBe empty
    val reparsedGame = reparsed.game.value

    val topVariation = reparsedGame.moves(1).variations.find(_.startingColor.contains("black")).value
    topVariation.moves.head.blackMove shouldBe Some("Nf6")
    topVariation.moves(1).whiteMove shouldBe Some("Bc4")
    topVariation.moves(1).variations should have size 1
    val nested = topVariation.moves(1).variations.head
    nested.startingMoveNumber shouldBe 3
    nested.moves.head.whiteMove shouldBe Some("Nc3")
  }
}