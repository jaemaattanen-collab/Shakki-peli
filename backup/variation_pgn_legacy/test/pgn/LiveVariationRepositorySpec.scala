package chess.pgn

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.OptionValues


class LiveVariationRepositorySpec extends AnyFlatSpec with Matchers with OptionValues {

  "LiveVariationRepository" should "store nested variations under their parent" in {
    val repo = new LiveVariationRepository()

    repo.addVariation(anchorIndex = 5, startingMoveNumber = 4, startingColor = "white", moves = List("Nc3", "d5"))
    val root = repo.getVariationTree.headOption.value

    repo.addNestedVariation(
      parentId = root.id,
      parentAnchorPly = 1,
      startingMoveNumber = 4,
      startingColor = "black",
      moves = List("exd4")
    )

    val updatedRoot = repo.getVariationTree.headOption.value
    updatedRoot.children should have size 1
    val child = updatedRoot.children.head
    child.parentId.value shouldBe root.id
    child.parentAnchorPly.value shouldBe 1
    child.anchorIndex shouldBe root.anchorIndex + child.parentAnchorPly.value
  }

  it should "collect nested variations when querying by anchor" in {
    val repo = new LiveVariationRepository()

    val parent = repo.addVariation(anchorIndex = 5, startingMoveNumber = 4, startingColor = "white", moves = List("Nc3", "d5")).value

    val child = repo.addNestedVariation(
      parentId = parent.id,
      parentAnchorPly = 1,
      startingMoveNumber = 4,
      startingColor = "black",
      moves = List("exd4", "O-O")
    ).value

    child.anchorIndex shouldBe parent.anchorIndex + 1

    val rootCollected = repo.variationsAtAnchor(parent.anchorIndex)
    rootCollected.map(_.id) should contain theSameElementsInOrderAs List(parent.id)

    val nestedCollected = repo.variationsAtAnchor(child.anchorIndex)
    nestedCollected.map(_.id) should contain theSameElementsInOrderAs List(child.id)
  }

  it should "preserve anchors when importing nested PGN lines" in {
    val repo = new LiveVariationRepository()

    val pgn = """[Result "*"]
[Annotator "Chess Application"]
[Round "-"]
[Date "2025-11-24"]
[PlyCount "11"]
[Event "Casual Game"]
[Black "Player"]
[White "Player"]
[Site "Local Game"]

1. e4 e5 2. Nf3 Nc6 3. Bc4 d6 
( 3... Nf6 4. d3 g6 5. O-O Bg7 6. Bg5 O-O ) 
( 3... g6 4. O-O Bg7 5. Nc3 Nf6 ( 5... Nge7 6. d3 O-O 7. Be3 ) 6. d3 O-O ) 
4. Nc3 Bg4 5. O-O Nf6 6. h3 Bd7 7. d3 g6 8. Bg5 Bg7 9. Nd5 O-O 10. Nxf6+ 
( 10. Nc3 b5 11. Bb3 a5 12. a4 bxa4 13. Bxa4 ) 
10... Bxf6 11. Bxf6 Qxf6 *"""

    val parsed = PGNParser.parsePGN(pgn)
    val game = parsed.game.value

    repo.replaceWithPGNVariations(game)

    val variations = repo.getVariations

    val nf6Line = variations.find(_.moves.headOption.contains("Nf6")).value
    nf6Line.moves.lastOption.value shouldBe "O-O"

    val g6Line = variations.find(_.moves.headOption.contains("g6")).value
    val nested = g6Line.children.find(_.moves.headOption.contains("Nge7")).value

    nested.parentAnchorPly.value should be > (0)
    nested.anchorIndex shouldBe g6Line.anchorIndex + nested.parentAnchorPly.value
  }

  it should "export nested variations in PGN format" in {
    val repo = new LiveVariationRepository()
    repo.addVariation(anchorIndex = 3, startingMoveNumber = 3, startingColor = "white", moves = List("Bc4", "Nf6"))
    val parent = repo.getVariationTree.headOption.value
    repo.addNestedVariation(
      parentId = parent.id,
      parentAnchorPly = 1,
      startingMoveNumber = 4,
      startingColor = "white",
      moves = List("Ng5")
    )

    val exported = repo.exportVariations
    exported should have size 1
    val top = exported.head
    top.moves.flatMap(_.variations) should have size 1
    val nested = top.moves.headOption.value.variations.head
    nested.startingMoveNumber shouldBe 4
    nested.startingColor shouldBe "white"
  }

  it should "round-trip nested variations through PGN reconstruction" in {
    val repo = new LiveVariationRepository()
    repo.addVariation(anchorIndex = 3, startingMoveNumber = 3, startingColor = "white", moves = List("Bc4", "Nf6"))
    val parent = repo.getVariationTree.headOption.value
    repo.addNestedVariation(
      parentId = parent.id,
      parentAnchorPly = 1,
      startingMoveNumber = 4,
      startingColor = "white",
      moves = List("Ng5", "d5")
    )

    val game = PGNGame(
      headers = Map.empty,
      moves = PGNMoveBuilder.fromHalfMoveNotations(List("e4", "e5", "Nf3", "Nc6", "Bc4", "Nf6")),
      result = Some("*"),
      variations = repo.exportVariations
    )

    val roundTripRepo = new LiveVariationRepository()
    roundTripRepo.replaceWithPGNVariations(game)

    val roots = roundTripRepo.getVariationTree
    roots should have size 1
    val rtParent = roots.head
    rtParent.children should have size 1
    val rtChild = rtParent.children.head
    rtChild.moves should contain inOrderOnly ("Ng5", "d5")
    rtChild.parentAnchorPly.value shouldBe 1
  }

  it should "persist draft snapshots incrementally" in {
    val repo = new LiveVariationRepository()

    repo.beginDraft(
      anchorIndex = 3,
      baseNotationSize = 4,
      startingMoveNumber = 3,
      startingColor = "white",
      originalFutureMoves = Nil
    )

    repo.updateDraftMoves(List("Nc3"))
    repo.persistDraftSnapshot()

    val firstSave = repo.getVariations
    firstSave should have size 1
    firstSave.head.moves should contain theSameElementsInOrderAs List("Nc3")

    repo.updateDraftMoves(List("Nc3", "d5"))
    repo.persistDraftSnapshot()

    val secondSave = repo.getVariations
    secondSave.head.moves should contain theSameElementsInOrderAs List("Nc3", "d5")

    repo.commitDraft()
    val finalState = repo.getVariations
    finalState.head.moves should contain theSameElementsInOrderAs List("Nc3", "d5")
  }

  it should "restore original variation when autosaved draft is discarded" in {
    val repo = new LiveVariationRepository()
    repo.addVariation(anchorIndex = 3, startingMoveNumber = 3, startingColor = "white", moves = List("Nc3", "d5"))
    val original = repo.getVariations.headOption.value

    repo.beginDraft(
      anchorIndex = original.anchorIndex,
      baseNotationSize = 4,
      startingMoveNumber = original.startingMoveNumber,
      startingColor = original.startingColor,
      originalFutureMoves = Nil,
      resumedFrom = Some(original)
    )

    repo.updateDraftMoves(List("Nc3", "d5", "exd5"))
    repo.persistDraftSnapshot()
    val updated = repo.getVariations.headOption.value
    updated.moves should contain theSameElementsInOrderAs List("Nc3", "d5", "exd5")

    repo.discardDraftAndRevert()

    val reverted = repo.getVariations.headOption.value
    reverted.moves should contain theSameElementsInOrderAs List("Nc3", "d5")
  }

  it should "preserve mainline continuation while extending a draft variation" in {
    val repo = new LiveVariationRepository()
    val anchorIndex = 2
    val baseNotationSize = math.max(anchorIndex + 1, 0)
    val startingMoveNumber = ((anchorIndex + 1) / 2) + 1
    val startingColor = if ((anchorIndex + 1) % 2 == 0) "white" else "black"
    val originalFuture = List("Nc6")

    repo.beginDraft(
      anchorIndex = anchorIndex,
      baseNotationSize = baseNotationSize,
      startingMoveNumber = startingMoveNumber,
      startingColor = startingColor,
      originalFutureMoves = originalFuture
    )

    repo.updateDraftMoves(List("Nf6"))
    repo.persistDraftSnapshot()

    val firstOverlay = repo.overlayMainline(List("e4", "e5", "Nf3", "Nf6"))
    firstOverlay should contain theSameElementsInOrderAs List("e4", "e5", "Nf3", "Nc6")

    val activeVariation = repo.getVariations.headOption.value
    repo.beginDraft(
      anchorIndex = anchorIndex,
      baseNotationSize = baseNotationSize,
      startingMoveNumber = startingMoveNumber,
      startingColor = startingColor,
      originalFutureMoves = originalFuture,
      resumedFrom = Some(activeVariation)
    )

    repo.updateDraftMoves(List("Nf6", "d4"))
    repo.persistDraftSnapshot()

    val continuationOverlay = repo.overlayMainline(List("e4", "e5", "Nf3", "Nf6", "d4"))
    continuationOverlay should contain theSameElementsInOrderAs List("e4", "e5", "Nf3", "Nc6")
  }
}
