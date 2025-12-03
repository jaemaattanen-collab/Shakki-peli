package chess.controllers

import scala.collection.mutable.{ListBuffer, Map as MutableMap}
import chess.board.*
import chess.pieces.{Piece, King as KingPiece}
import chess.types.PieceType
import chess.state.Position
import chess.core.NotationManager

class MoveHistoryManager {
  // Depth-partitioned storage for efficient parent/child lookups
  private val moveHistoryByDepth: MutableMap[Int, ListBuffer[Move]] = MutableMap()
  // Fast index from TOB to move instance
  private val moveIndexByTob: MutableMap[Long, Move] = MutableMap()
  // Index from parentTob to all children (supports branching)
  private val childrenByParentTob: MutableMap[Long, ListBuffer[Move]] = MutableMap()
  private var maxRecordedDepth: Int = 0
  private var totalHalfMoves: Int = 0

  // Notation manager - PRIMARY storage system for mainline (always used)
  val notationManager: NotationManager = new NotationManager()

  private val positionCount = scala.collection.mutable.Map[Position, Int]().withDefaultValue(0)

  def getMoveHistory: List[Move] =
    moveHistoryByDepth.toSeq
      .sortBy(_._1)
      .flatMap { case (_, bucket) => bucket.toList }
      .toList

  /** Get only mainline moves (non-variation moves) */
  def getMainlineMoves: List[Move] =
    getLastMainlineMove.map(collectLineTo).getOrElse(Nil)

  /** Get the last move that is part of the mainline by following the tree from root */
  def getLastMainlineMove: Option[Move] =
    if totalHalfMoves == 0 then None
    else
      // Start from the root (depth 1) and follow mainline children
      // Mainline is defined by following non-variation children from root
      val StartingTob = -1L
      
      @annotation.tailrec
      def followMainline(parentTob: Long): Option[Move] =
        // Get the mainline child (first non-variation child)
        getMainlineChildOf(parentTob) match
          case Some(child) =>
            // Check if this child has a mainline child
            getMainlineChildOf(child.tob) match
              case Some(_) => followMainline(child.tob)  // Continue deeper
              case None => Some(child)  // This is the last mainline move
          case None => 
            // No mainline child - if parentTob is StartingTob, mainline is empty
            if parentTob == StartingTob then None
            else getMoveByTob(parentTob)  // Return the parent as the last mainline move
      
      followMainline(StartingTob)

  def getMainlineMoveAt(index: Int): Option[Move] =
    if index < 0 then None
    else getMainlineMoves.lift(index)

  def getLastMove: Option[Move] =
    if totalHalfMoves == 0 then None
    else moveHistoryByDepth.get(maxRecordedDepth).flatMap(_.lastOption)

  def getMoveCount: Int = totalHalfMoves

  def clearHistory() =
    moveHistoryByDepth.clear()
    moveIndexByTob.clear()
    childrenByParentTob.clear()
    maxRecordedDepth = 0
    totalHalfMoves = 0
    positionCount.clear()
    notationManager.clear()

  def getMoveNotations: List[String] =
    // Return the notations from the notation manager (mainline only)
    notationManager.getMoves()

  /**
   * Trim both the linear move list and notation list to the exact half-move count provided.
   * Only affects mainline moves - variations are preserved.
   */
  def trimToSize(size: Int): Unit =
    val target = math.max(size, 0)
    while totalHalfMoves > target do
      removeLastMove()
    totalHalfMoves = math.min(totalHalfMoves, target)
    notationManager.trimToSize(target)
    positionCount.clear()

  def addMove(move: Move): Unit =
    val depthBucket = moveHistoryByDepth.getOrElseUpdate(move.halfmoveDistanceFromStart, ListBuffer())
    depthBucket += move
    moveIndexByTob.update(move.tob, move)
    
    // Index children by parent
    val childBucket = childrenByParentTob.getOrElseUpdate(move.parentTob, ListBuffer())
    childBucket += move
    
    if !move.isVariation then
      totalHalfMoves += 1
    if move.halfmoveDistanceFromStart > maxRecordedDepth then
      maxRecordedDepth = move.halfmoveDistanceFromStart

  /**
   * Add a variation move - has a parent in the mainline/another variation but branches off
   */
  def addVariationMove(move: Move): Unit =
    val variationMove = move.copy(isVariation = true)
    addMove(variationMove)

  /**
   * Add a move to the notation (PRIMARY storage) and depth index
   */
  def addMoveToNotation(move: Move, notation: String, moveNumber: Int, isWhite: Boolean): Unit =
    // Add to notation manager (only for mainline)
    if !move.isVariation then
      notationManager.addMove(notation)
    // Add to linear history for compatibility
    addMove(move)

  def addPosition(pos: Position): Unit =
    positionCount(pos) = positionCount(pos) + 1

  def isThreefoldRepetition: Boolean =
    positionCount.values.exists(_ >= 3)

  def getMoveByTob(tob: Long): Option[Move] =
    moveIndexByTob.get(tob)

  def getMovesAtDepth(depth: Int): List[Move] =
    moveHistoryByDepth.get(depth).map(_.toList).getOrElse(Nil)

  /** Get all children of a parent move (supports multiple branches) */
  def getChildrenOf(parentTob: Long): List[Move] =
    childrenByParentTob.get(parentTob).map(_.toList).getOrElse(Nil)

  /** Get mainline child (first non-variation child) */
  def getMainlineChildOf(parentTob: Long): Option[Move] =
    childrenByParentTob.get(parentTob).flatMap(_.find(!_.isVariation))

  /** Get variation children (all variation branches from this parent) */
  def getVariationChildrenOf(parentTob: Long): List[Move] =
    val allChildren = childrenByParentTob.get(parentTob)
    val variationChildren = allChildren.map(_.filter(_.isVariation).toList).getOrElse(Nil)
    variationChildren

  def findChildAtDepth(parentTob: Long, depth: Int): Option[Move] =
    // Only find mainline children (non-variation moves) for navigation
    moveHistoryByDepth.get(depth).flatMap(_.find(m => m.parentTob == parentTob && !m.isVariation))

  /** Collect the line from start to this move, following parentTob links */
  def collectLineTo(move: Move): List[Move] =
    val stack = ListBuffer[Move]()
    val visited = scala.collection.mutable.Set[Long]()
    var current: Option[Move] = Some(move)

    while current.isDefined do
      val m = current.get
      if visited.contains(m.tob) then
        current = None
      else
        stack.prepend(m)
        visited += m.tob
        current =
          if m.parentTob == -1L then None
          else getMoveByTob(m.parentTob)

    stack.toList

  /** Collect all moves in a variation starting from variationRootTob */
  def collectVariationLine(variationRootTob: Long): List[Move] =
    getMoveByTob(variationRootTob) match
      case Some(rootMove) =>
        val result = ListBuffer[Move](rootMove)
        var current = rootMove
        var continue = true
        while continue do
          // Find the next move in this variation (child with same variationRootTob)
          val children = getChildrenOf(current.tob)
          children.find(_.variationRootTob == Some(variationRootTob)) match
            case Some(next) =>
              result += next
              current = next
            case None =>
              continue = false
        result.toList
      case None => 
        Nil

  /** Get all variation roots branching from a specific parent */
  def getVariationRootsFrom(parentTob: Long): List[Move] =
    val children = getVariationChildrenOf(parentTob)
    val roots = children.filter(m => m.variationRootTob.contains(m.tob))
    roots

  /** Remove a variation and all its descendants */
  def removeVariation(variationRootTob: Long): Unit =
    val toRemove = collectVariationDescendants(variationRootTob)
    toRemove.foreach { move =>
      moveIndexByTob -= move.tob
      childrenByParentTob.get(move.parentTob).foreach(_ -= move)
      moveHistoryByDepth.get(move.halfmoveDistanceFromStart).foreach(_ -= move)
    }

  private def collectVariationDescendants(rootTob: Long): List[Move] =
    val result = ListBuffer[Move]()
    val queue = scala.collection.mutable.Queue[Long](rootTob)
    while queue.nonEmpty do
      val tob = queue.dequeue()
      getMoveByTob(tob).foreach { move =>
        result += move
        getChildrenOf(tob).foreach(child => queue.enqueue(child.tob))
      }
    result.toList

  // =========================================================================
  // VARIATION API - For unified variation handling via Move tree
  // =========================================================================

  /**
   * Get all variation root moves that branch from a mainline position.
   * A variation root is a Move where isVariation=true and variationRootTob points to itself.
   * 
   * @param mainlineIndex The half-move index in mainline (0-based)
   * @return List of variation root Moves branching from that position
   */
  def getVariationRootsAtMainlineIndex(mainlineIndex: Int): List[Move] =
    getMainlineMoveAt(mainlineIndex) match
      case Some(mainlineMove) =>
        val roots = getVariationRootsFrom(mainlineMove.tob)
        roots
      case None =>
        // Check for variations from starting position (mainlineIndex = -1)
        if mainlineIndex < 0 then
          val roots = getVariationRootsFrom(-1L)  // Starting position parent tob
          roots
        else
          Nil

  /**
   * Get a preview string for a variation (like "1. e4 e5 d4...")
   * 
   * @param variationRootTob The TOB of the variation's first move
   * @param maxMoves Maximum number of moves to show in preview
   * @return Formatted preview string
   */
  def getVariationPreview(variationRootTob: Long, maxMoves: Int = 3): String =
    getMoveByTob(variationRootTob) match
      case Some(rootMove) =>
        val line = collectVariationLine(variationRootTob)
        val notations = line.flatMap(_.notation).take(maxMoves)
        if notations.isEmpty then "<no moves>"
        else
          // Calculate move number from the root move's depth
          val moveNumber = (rootMove.halfmoveDistanceFromStart / 2) + 1
          val isWhiteFirst = rootMove.halfmoveDistanceFromStart % 2 == 0
          val prefix = if isWhiteFirst then s"$moveNumber." else s"$moveNumber..."
          val movesStr = notations.mkString(" ")
          val ellipsis = if line.size > maxMoves then " ..." else ""
          s"$prefix $movesStr$ellipsis".trim
      case None => "<variation not found>"

  /**
   * Generate variation indicators for UI display.
   * Returns Map[mainlineIndex -> List[preview strings]] for root-level variations.
   */
  def variationIndicatorsFromMoves(): Map[Int, List[String]] =
    // Find all ROOT-LEVEL variation root moves (isVariation=true AND variationRootTob == own tob AND parent is mainline)
    val allVariationRoots = moveIndexByTob.values
      .filter { m => 
        m.isVariation && m.variationRootTob.contains(m.tob) && {
          // Only include if parent is mainline (not another variation)
          getMoveByTob(m.parentTob) match
            case Some(parentMove) => !parentMove.isVariation
            case None => m.parentTob == -1L  // From starting position
        }
      }
      .toList
    
    // Group by parent's mainline index
    allVariationRoots
      .groupBy { rootMove =>
        // Find which mainline index this branches from
        if rootMove.parentTob == -1L then -1  // From starting position
        else
          getMoveByTob(rootMove.parentTob) match
            case Some(parentMove) if !parentMove.isVariation =>
              // Parent is mainline - find its index
              getMainlineMoves.indexWhere(_.tob == parentMove.tob)
            case _ => -1
      }
      .view
      .mapValues(roots => roots.map(r => getVariationPreview(r.tob)))
      .toMap

  /**
   * Generate variation indicators for children of a specific variation.
   * Used when viewing a variation to show nested sub-variations.
   * 
   * @param parentVariationRootTob The root TOB of the parent variation
   * @return Map[plyWithinParent -> List[preview strings]]
   */
  def childVariationIndicatorsFromMoves(parentVariationRootTob: Long): Map[Int, List[String]] =
    val parentLine = collectVariationLine(parentVariationRootTob)
    if parentLine.isEmpty then return Map.empty
    
    // Find all nested variations that branch from moves in this variation
    val nestedRoots = parentLine.flatMap { parentMove =>
      getVariationRootsFrom(parentMove.tob)
    }
    
    // Group by ply within parent variation
    nestedRoots
      .groupBy { rootMove =>
        // Find the index of the parent move within the parent variation line
        parentLine.indexWhere(_.tob == rootMove.parentTob)
      }
      .view
      .mapValues(roots => roots.map(r => getVariationPreview(r.tob)))
      .toMap

  /**
   * Get all variations (flattened list of all variation root moves).
   */
  def getAllVariationRoots: List[Move] =
    moveIndexByTob.values
      .filter(m => m.isVariation && m.variationRootTob.contains(m.tob))
      .toList

  /**
   * Find a variation root Move by its mainline anchor index and first move notation.
   * Used to bridge from LiveVariation (SAN-based) to Move-based navigation.
   */
  def findVariationRootByNotation(mainlineAnchorIndex: Int, firstMoveNotation: String): Option[Move] =
    val variationRoots = getVariationRootsAtMainlineIndex(mainlineAnchorIndex)
    variationRoots.find(_.notation.contains(firstMoveNotation))

  /**
   * Get variation roots that branch from a specific move within a variation.
   * Used for nested variations.
   * 
   * @param parentVariationRootTob The root TOB of the parent variation
   * @param plyWithinParent The position within the parent variation (0-based)
   * @return List of nested variation root moves
   */
  def getNestedVariationRootsAt(parentVariationRootTob: Long, plyWithinParent: Int): List[Move] =
    val parentLine = collectVariationLine(parentVariationRootTob)
    if plyWithinParent < 0 || plyWithinParent >= parentLine.size then Nil
    else
      val parentMove = parentLine(plyWithinParent)
      getVariationRootsFrom(parentMove.tob)

  /**
   * Get the mainline index (anchor) where a variation root branches from.
   * Returns -1 if branching from starting position.
   */
  def getMainlineAnchorIndexForVariation(variationRootTob: Long): Int =
    getMoveByTob(variationRootTob) match
      case Some(rootMove) =>
        if rootMove.parentTob == -1L then -1
        else
          getMoveByTob(rootMove.parentTob) match
            case Some(parentMove) if !parentMove.isVariation =>
              getMainlineMoves.indexWhere(_.tob == parentMove.tob)
            case Some(parentMove) =>
              // Parent is in a variation - return the parent variation's anchor
              parentMove.variationRootTob.flatMap(rootTob => 
                getMoveByTob(rootTob).map(root => 
                  getMainlineAnchorIndexForVariation(root.tob)
                )
              ).getOrElse(-1)
            case None => -1
      case None => -1

  /**
   * Check if a variation is nested (its parent is in another variation).
   * Returns Some(parentVariationRootTob) if nested, None if root-level.
   */
  def getParentVariationRootTob(variationRootTob: Long): Option[Long] =
    getMoveByTob(variationRootTob).flatMap { rootMove =>
      if rootMove.parentTob == -1L then None
      else getMoveByTob(rootMove.parentTob).flatMap { parentMove =>
        if parentMove.isVariation then parentMove.variationRootTob
        else None
      }
    }

  /**
   * Get the index within parent variation where a nested variation branches.
   * Returns the position in the parent variation line where the branch occurs.
   */
  def getParentVariationBranchIndex(variationRootTob: Long): Int =
    getMoveByTob(variationRootTob).flatMap { rootMove =>
      if rootMove.parentTob == -1L then None
      else getMoveByTob(rootMove.parentTob).flatMap { parentMove =>
        if parentMove.isVariation then
          parentMove.variationRootTob.map { parentRootTob =>
            val parentLine = collectVariationLine(parentRootTob)
            parentLine.indexWhere(_.tob == parentMove.tob)
          }
        else None
      }
    }.getOrElse(-1)

  /**
   * Get starting color for a variation root move.
   * halfmoveDistanceFromStart: 1=white, 2=black, 3=white, 4=black...
   */
  def getVariationStartingColor(variationRootTob: Long): String =
    getMoveByTob(variationRootTob) match
      case Some(move) => if move.halfmoveDistanceFromStart % 2 == 1 then "white" else "black"
      case None => "white"

  /**
   * Get starting move number for a variation root move.
   */
  def getVariationStartingMoveNumber(variationRootTob: Long): Int =
    getMoveByTob(variationRootTob) match
      case Some(move) => (move.halfmoveDistanceFromStart / 2) + 1
      case None => 1

  /**
   * Get move notations for a variation line.
   */
  def getVariationMoveNotations(variationRootTob: Long): List[String] =
    collectVariationLine(variationRootTob).flatMap(_.notation)

  // =========================================================================

  private def removeLastMove(): Unit =
    if moveHistoryByDepth.isEmpty then return

    val depth = maxRecordedDepth
    moveHistoryByDepth.get(depth) match
      case Some(buffer) if buffer.nonEmpty =>
        // Remove only mainline moves when trimming
        buffer.filter(!_.isVariation).lastOption match
          case Some(removed) =>
            buffer -= removed
            moveIndexByTob -= removed.tob
            childrenByParentTob.get(removed.parentTob).foreach(_ -= removed)
            totalHalfMoves = math.max(totalHalfMoves - 1, 0)
            if buffer.isEmpty then
              moveHistoryByDepth -= depth
              maxRecordedDepth = moveHistoryByDepth.keysIterator.maxOption.getOrElse(0)
          case None =>
            // No mainline moves at this depth, adjust maxRecordedDepth
            if buffer.isEmpty then
              moveHistoryByDepth -= depth
            maxRecordedDepth = moveHistoryByDepth.keysIterator
              .filter(d => moveHistoryByDepth(d).exists(!_.isVariation))
              .maxOption.getOrElse(0)
      case _ =>
        moveHistoryByDepth -= depth
        maxRecordedDepth = moveHistoryByDepth.keysIterator.maxOption.getOrElse(0)

  def generateAlgebraicNotationWithDisambiguation(move: Move, board: Board, disambiguation: String): String =
    if (isCastlingMove(move)) then
      if (isKingSideCastling(move)) then "O-O" else "O-O-O"
    else
       val pieceSymbol = move.piece.pieceType match
          case PieceType.Pawn    => if isCapture(move) then move.from.name(0) else ""
          case PieceType.Knight  => "N"
          case PieceType.Bishop  => "B"
          case PieceType.Rook    => "R"
          case PieceType.Queen   => "Q"
          case PieceType.King    => "K"

       val captureSymbol = if isCapture(move) then "x" else ""
       var checkSymbol = ""
       val startingChecks = isCheck(board, move.piece)
       if startingChecks._1 && startingChecks._2.occupiedBy.exists(piece =>
         !board.hasValidMoves(piece.color)) then checkSymbol = "#"
       else if startingChecks._1 then checkSymbol = "+"
       else checkSymbol = ""
       var promotionMark = ""
       if move.promotedPieceType.isDefined then
         promotionMark = move.promotedPieceType match
           case Some(PieceType.Knight)  => "=N"
           case Some(PieceType.Bishop)  => "=B"
           case Some(PieceType.Rook)   => "=R"
           case Some(PieceType.Queen)  => "=Q"
           case _ => "=Q"
       val targetSquare = move.to.name.toLowerCase
         s"${pieceSymbol}${disambiguation}${captureSymbol}${targetSquare}${promotionMark}${checkSymbol}"
  end generateAlgebraicNotationWithDisambiguation

  def isCastlingMove(move: Move): Boolean =
    move.piece.isInstanceOf[KingPiece] && math.abs(move.from.getCoordinates._1 - move.to.getCoordinates._1) == 2

  def isKingSideCastling(move: Move): Boolean =
    move.to.getCoordinates._1 > move.from.getCoordinates._1

  def isCapture(move: Move): Boolean =
    move.capturedPiece.exists(_.color != move.piece.color)

  def isCheck(board:Board, piece: Piece): (Boolean, Square) =
    board.isKingInCheck(if piece.color == "white" then "black" else "white")

}