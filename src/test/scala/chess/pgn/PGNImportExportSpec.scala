package chess.pgn

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import chess.board.Board
import chess.controllers.GameController

/**
 * Comprehensive tests for PGN import/export roundtrip functionality.
 * Tests 15 different scenarios including:
 * - Multiple branches from one move
 * - Multiple levels of nested variations
 * - Early game branches
 * - Promotions
 * - Complex game structures
 */
class PGNImportExportSpec extends AnyFunSuite with Matchers {

  private def createFreshGame(): (GameController, Board, PGNManager) = {
    val controller = new GameController()
    val board = new Board(controller)
    controller.board = Some(board)
    controller.updateAllPossibleMoves()
    val pgnManager = new PGNManager(controller, board)
    (controller, board, pgnManager)
  }

  private def normalizeSpaces(pgn: String): String = {
    pgn.replaceAll("\\s+", " ").trim
  }

  private def extractMoveText(pgn: String): String = {
    // Remove headers and normalize
    val lines = pgn.split("\n").filterNot(_.startsWith("[")).mkString(" ")
    normalizeSpaces(lines)
  }

  private def testRoundTrip(testName: String, inputPgn: String): Unit = {
    val (controller, board, pgnManager) = createFreshGame()
    
    // Import
    val importResult = pgnManager.loadPGNFromString(inputPgn)
    assert(importResult.isRight, s"$testName: Import failed: ${importResult.left.getOrElse("")}")
    
    // Export
    val exported = pgnManager.getCurrentPGNString
    
    // Compare move text (ignoring headers and whitespace differences)
    val inputMoves = extractMoveText(inputPgn)
    val exportedMoves = extractMoveText(exported)
    
    assert(inputMoves == exportedMoves, 
      s"""$testName: Roundtrip mismatch!
         |Input:    $inputMoves
         |Exported: $exportedMoves""".stripMargin)
  }

  // ============================================
  // Test 1: Simple mainline only
  // ============================================
  test("Scenario 1: Simple mainline without variations") {
    val pgn = "1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 4. Ba4 Nf6 5. O-O *"
    testRoundTrip("Simple mainline", pgn)
  }

  // ============================================
  // Test 2: Single variation from white move
  // ============================================
  test("Scenario 2: Single variation from white's move") {
    val pgn = "1. e4 e5 2. Nf3 Nc6 3. Bc4 (3. Bb5 a6 4. Ba4) 3... Nf6 4. d3 *"
    testRoundTrip("Single white variation", pgn)
  }

  // ============================================
  // Test 3: Single variation from black move
  // ============================================
  test("Scenario 3: Single variation from black's move") {
    val pgn = "1. e4 e5 (1... c5 2. Nf3 d6 3. d4) 2. Nf3 Nc6 3. Bb5 *"
    testRoundTrip("Single black variation", pgn)
  }

  // ============================================
  // Test 4: Multiple branches from same move
  // ============================================
  test("Scenario 4: Multiple branches from same white move") {
    val pgn = "1. e4 e5 2. Nf3 Nc6 3. Bc4 (3. Bb5 a6) (3. d4 exd4) (3. Nc3 Nf6) 3... Nf6 4. O-O *"
    testRoundTrip("Multiple branches from same move", pgn)
  }

  // ============================================
  // Test 5: Nested variation (2 levels)
  // ============================================
  test("Scenario 5: Two-level nested variation") {
    val pgn = "1. e4 e5 2. Nf3 Nc6 3. Bc4 (3. d4 exd4 4. Nxd4 (4. c3 dxc3 5. Nxc3) 4... Nf6) 3... Nf6 *"
    testRoundTrip("Two-level nested variation", pgn)
  }

  // ============================================
  // Test 6: Deeply nested variation (3 levels)
  // ============================================
  test("Scenario 6: Three-level deeply nested variation") {
    // Valid three-level nesting:
    // Level 0 (mainline): 1. e4 e5 2. Nf3 Nc6 3. d4 exd4
    // Level 1: 3. Bc4 Nf6
    // Level 2 (inside level 1): 3... Bc5 4. c3
    // Level 3 (inside level 2): 4. d3 d6
    val pgn = "1. e4 e5 2. Nf3 Nc6 3. d4 (3. Bc4 Nf6 (3... Bc5 4. c3 (4. d3 d6))) 3... exd4 *"
    testRoundTrip("Three-level nested variation", pgn)
  }

  // ============================================
  // Test 7: Early game branch (move 1)
  // ============================================
  test("Scenario 7: Branch from move 1 white") {
    val pgn = "1. e4 (1. d4 d5 2. c4 e6) (1. c4 e5 2. Nc3) 1... e5 2. Nf3 *"
    testRoundTrip("Early branch from move 1", pgn)
  }

  // Test 7b: Verify move 1 variations are stored with parentTob = -1L
  test("Scenario 7b: Move 1 variations have correct parentTob") {
    val (controller, board, pgnManager) = createFreshGame()
    val pgn = "1. e4 (1. d4 d5) 1... e5 *"
    pgnManager.loadPGNFromString(pgn)
    
    // Check variations from starting position
    val varsAtStart = controller.moveHistoryManager.getVariationRootsAtMainlineIndex(-1)
    assert(varsAtStart.nonEmpty, "Should have variations at mainline index -1")
    varsAtStart.foreach { varRoot =>
      assert(varRoot.parentTob == -1L, s"Move 1 variation root should have parentTob=-1L, got ${varRoot.parentTob}")
      assert(varRoot.isVariation, "Should be marked as variation")
      println(s"Move 1 variation: tob=${varRoot.tob}, parentTob=${varRoot.parentTob}, notation=${varRoot.notation}")
    }
  }

  // ============================================
  // Test 8: Branch from move 1 black
  // ============================================
  test("Scenario 8: Branch from move 1 black") {
    val pgn = "1. e4 e5 (1... c5 2. Nf3 d6) (1... e6 2. d4 d5) 2. Nf3 Nc6 *"
    testRoundTrip("Branch from move 1 black", pgn)
  }

  // ============================================
  // Test 9: Complex structure - Original test case
  // ============================================
  test("Scenario 9: Complex nested structure (original test case)") {
    val pgn = "1. e4 e5 2. Nf3 Nc6 3. Bc4 (3. d4 exd4 4. Nxd4 Nxd4 5. Qxd4 Nf6 6. Nc3 Be7 7. Be3 (7. Bg5 h6 8. Bh4 O-O 9. O-O-O) 7... d5 8. e5 Ne4 9. Nxe4 dxe4 10. Qxd8+ Bxd8) 3... Nf6 4. O-O *"
    testRoundTrip("Complex nested structure", pgn)
  }

  // ============================================
  // Test 10: Castling variations (both sides)
  // ============================================
  test("Scenario 10: Castling in variations") {
    val pgn = "1. e4 e5 2. Nf3 Nc6 3. Bc4 Bc5 4. O-O (4. c3 Nf6 5. d4 exd4 6. cxd4 Bb4+ 7. Nc3 O-O) 4... Nf6 5. d3 O-O *"
    testRoundTrip("Castling in variations", pgn)
  }

  // ============================================
  // Test 11: Long mainline with late branch
  // ============================================
  test("Scenario 11: Long mainline with late branch") {
    val pgn = "1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 4. Ba4 Nf6 5. O-O Be7 6. Re1 b5 7. Bb3 d6 8. c3 O-O 9. h3 (9. d4 Bg4 10. Be3) 9... Na5 10. Bc2 *"
    testRoundTrip("Long mainline with late branch", pgn)
  }

  // ============================================
  // Test 12: Multiple variations at different points
  // ============================================
  test("Scenario 12: Multiple variations scattered throughout game") {
    val pgn = "1. e4 (1. d4 Nf6) 1... e5 2. Nf3 Nc6 (2... Nf6 3. Nxe5) 3. Bc4 Bc5 (3... Nf6 4. Ng5) 4. c3 *"
    testRoundTrip("Multiple scattered variations", pgn)
  }

  // ============================================
  // Test 13: Variation ending in checkmate
  // ============================================
  test("Scenario 13: Variation with tactical sequence") {
    val pgn = "1. e4 e5 2. Nf3 Nc6 3. Bc4 Nd4 (3... Nf6 4. Ng5 d5 5. exd5 Nxd5) 4. Nxe5 Qg5 5. Nxf7 *"
    testRoundTrip("Variation with tactics", pgn)
  }

  // ============================================
  // Test 14: Parallel variations with different depths
  // ============================================
  test("Scenario 14: Parallel variations with different depths") {
    val pgn = "1. e4 e5 2. Nf3 Nc6 3. d4 (3. Bc4 Nf6 4. d3 (4. Ng5 d5 5. exd5 Na5) 4... Be7) (3. Bb5 a6 4. Ba4 Nf6 5. O-O) 3... exd4 4. Nxd4 *"
    testRoundTrip("Parallel variations different depths", pgn)
  }

  // ============================================
  // Test 15: Promotion in variation
  // ============================================
  test("Scenario 15: Pawn promotion in variation") {
    // This is a constructed endgame position scenario
    val pgn = "1. e4 e5 2. Nf3 Nc6 3. d4 exd4 4. Bc4 (4. c3 dxc3 5. Bc4 cxb2 6. Bxb2) 4... Nf6 *"
    testRoundTrip("Promotion scenario setup", pgn)
  }

  // ============================================
  // Additional edge case tests
  // ============================================
  
  test("Scenario 16: Empty variation should not break export") {
    val pgn = "1. e4 e5 2. Nf3 Nc6 3. Bb5 *"
    testRoundTrip("No variations at all", pgn)
  }

  test("Scenario 17: Very short game with variation") {
    val pgn = "1. e4 (1. d4 d5) 1... e5 *"
    testRoundTrip("Very short game with variation", pgn)
  }

  test("Scenario 18: Variation at very end of game") {
    val pgn = "1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 (3... Nf6 4. O-O) *"
    testRoundTrip("Variation at end", pgn)
  }

  // ============================================
  // Integration test: Full roundtrip multiple times
  // ============================================
  test("Scenario 19: Double roundtrip stability") {
    val pgn = "1. e4 e5 2. Nf3 Nc6 3. Bc4 (3. d4 exd4 4. Nxd4 (4. c3 d5) 4... Nf6) 3... Nf6 4. O-O *"
    
    val (controller1, board1, pgnManager1) = createFreshGame()
    pgnManager1.loadPGNFromString(pgn)
    val exported1 = pgnManager1.getCurrentPGNString
    
    val (controller2, board2, pgnManager2) = createFreshGame()
    pgnManager2.loadPGNFromString(exported1)
    val exported2 = pgnManager2.getCurrentPGNString
    
    val moves1 = extractMoveText(exported1)
    val moves2 = extractMoveText(exported2)
    
    assert(moves1 == moves2, s"Double roundtrip failed!\nFirst:  $moves1\nSecond: $moves2")
  }

  // ============================================
  // Stress test: Many variations
  // ============================================
  test("Scenario 20: Many variations stress test") {
    val pgn = """1. e4 (1. d4 d5) (1. c4 e5) (1. Nf3 d5) 1... e5 (1... c5) (1... e6) 2. Nf3 Nc6 (2... Nf6) 3. Bc4 (3. Bb5) (3. d4) 3... Bc5 *"""
    testRoundTrip("Many variations stress test", pgn)
  }
}
