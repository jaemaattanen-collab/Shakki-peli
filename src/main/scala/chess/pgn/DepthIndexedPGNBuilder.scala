package chess.pgn

import chess.controllers.{Move, MoveHistoryManager}

import scala.collection.mutable

/**
 * Builds PGN data structures (mainline + variations) from the depth-indexed
 * move history stored in [[MoveHistoryManager]].
 */
object DepthIndexedPGNBuilder {

  /** PGN conversion outcome. */
  final case class Result(
    mainline: List[PGNMove],
    rootVariations: List[PGNVariation]
  )

  private case class MoveNode(move: Move, children: List[MoveNode])
  private case class ConvertedLine(moves: List[PGNMove], leadingVariations: List[PGNVariation])

  /**
   * Convert the recorded move history into PGN move/variation structures.
   */
  def build(manager: MoveHistoryManager): Result = {
    val allMoves = manager.getMoveHistory
    if allMoves.isEmpty then return Result(Nil, Nil)

    // DEBUG: Print all moves and their variation status
    allMoves.foreach { m =>
    }

    val childrenByParent: Map[Long, List[Move]] =
      allMoves
        .groupBy(_.parentTob)
        .view
        .mapValues(_.sortBy(_.tob))
        .toMap
        .withDefaultValue(Nil)

    val nodeCache = mutable.Map.empty[Long, MoveNode]

    def buildNode(move: Move): MoveNode =
      nodeCache.getOrElseUpdate(move.tob, {
        val children = childrenByParent(move.tob).map(buildNode)
        MoveNode(move, children)
      })

    val rootChildren = childrenByParent(-1L).map(buildNode)

    val mainlineNodes: List[MoveNode] =
      manager.getLastMainlineMove
        .map(manager.collectLineTo)
        .getOrElse(Nil)
        .flatMap(move => nodeCache.get(move.tob)) match
        case Nil =>
          rootChildren.headOption.map(collectPrimaryLine).getOrElse(Nil)
        case nodes => nodes
    
    mainlineNodes.foreach { n =>
    }

    val mainlineConversion = convertLine(mainlineNodes)
    val rootVariationsBuffer = mutable.ListBuffer.empty[PGNVariation]
    rootVariationsBuffer ++= mainlineConversion.leadingVariations

    val mainlineFirstTob = mainlineNodes.headOption.map(_.move.tob)
    val rootBranchChildren = rootChildren.filter(child => !mainlineFirstTob.contains(child.move.tob))
    rootBranchChildren.foreach { child =>
      val branchLine = collectPrimaryLine(child)
      val converted = convertLine(branchLine)
      // Root variations are alternatives to the first mainline move
      // So their origin is move 1, with the same color as the first mainline move
      val originMoveNum = mainlineNodes.headOption.map(n => moveNumber(n.move)).getOrElse(1)
      val originCol = mainlineNodes.headOption.map(n => moveColor(n.move)).getOrElse("white")
      rootVariationsBuffer += PGNVariation(
        startingMoveNumber = moveNumber(child.move),
        startingColor = moveColor(child.move),
        moves = converted.moves,
        comment = None,
        originMoveNumber = Some(originMoveNum),
        originColor = Some(originCol),
        leadingVariations = converted.leadingVariations
      )
    }

    Result(mainlineConversion.moves, rootVariationsBuffer.toList)
  }

  private def convertLine(nodes: List[MoveNode]): ConvertedLine = {
    if nodes.isEmpty then return ConvertedLine(Nil, Nil)

    val movesMap = mutable.LinkedHashMap.empty[Int, PGNMove]

    nodes.zipWithIndex.foreach { case (node, idx) =>
      val moveNum = moveNumber(node.move)
      val color = moveColor(node.move)
      val notation = node.move.notation.getOrElse("?")

      val existing = movesMap.getOrElse(moveNum, PGNMove(moveNumber = moveNum, whiteMove = None, blackMove = None))
      val updated = color match
        case "white" => existing.copy(whiteMove = Some(notation))
        case "black" => existing.copy(blackMove = Some(notation))
        case _        => existing
      movesMap.update(moveNum, updated)

      val nextChildTob = nodes.lift(idx + 1).map(_.move.tob)
      val variationChildren = node.children.filterNot(ch => nextChildTob.contains(ch.move.tob))
      if variationChildren.nonEmpty then
        val anchor = Some(node)
        val variations = variationChildren.map(child => buildVariation(child, anchor))
        variations.foreach { variation =>
          val targetMoveNumber = variation.originMoveNumber.getOrElse(moveNum)
          val target = movesMap.getOrElse(targetMoveNumber, PGNMove(moveNumber = targetMoveNumber, whiteMove = None, blackMove = None))
          movesMap.update(targetMoveNumber, target.copy(variations = target.variations :+ variation))
        }
    }

    ConvertedLine(movesMap.values.toList, Nil)
  }

  private def buildVariation(child: MoveNode, @annotation.unused anchor: Option[MoveNode]): PGNVariation = {
    val branchLine = collectPrimaryLine(child)
    val converted = convertLine(branchLine)
    // Origin is based on the FIRST move of the variation (what it's an alternative to)
    val childMoveNumber = moveNumber(child.move)
    val childColor = moveColor(child.move)

    PGNVariation(
      startingMoveNumber = childMoveNumber,
      startingColor = childColor,
      moves = converted.moves,
      comment = None,
      originMoveNumber = Some(childMoveNumber),
      originColor = Some(childColor),
      leadingVariations = converted.leadingVariations
    )
  }

  private def collectPrimaryLine(start: MoveNode): List[MoveNode] = {
    val buffer = mutable.ListBuffer.empty[MoveNode]
    val visited = mutable.Set.empty[Long]
    var current: Option[MoveNode] = Some(start)

    while current.nonEmpty do
      val node = current.get
      if visited.contains(node.move.tob) then
        current = None
      else
        buffer += node
        visited += node.move.tob
        val next = node.children.sortBy(_.move.tob).headOption
        current = next

    buffer.toList
  }

  private def moveNumber(move: Move): Int =
    ((move.halfmoveDistanceFromStart + 1) / 2)

  private def moveColor(move: Move): String =
    if move.halfmoveDistanceFromStart % 2 == 1 then "white" else "black"
}
