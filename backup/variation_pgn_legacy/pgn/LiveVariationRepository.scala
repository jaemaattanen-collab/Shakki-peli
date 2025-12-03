package chess.pgn

import chess.pgn.LiveVariationRepository.*

/**
 * Tracks ad-hoc variations created during live analysis/editing.
 */
class LiveVariationRepository {

  type VariationId = Long

  private var nextVariationId: VariationId = 1
  private def allocateId(): VariationId =
    val id = nextVariationId
    nextVariationId += 1
    id

  case class LiveVariation(
    anchorIndex: Int,             // Half-move index where the variation branches ( -1 => start position )
    startingMoveNumber: Int,      // Move number of the first move inside the variation
    startingColor: String,        // "white" or "black" – colour to play the first move in the variation
    moves: List[String],          // SAN moves comprising the variation line
    comment: Option[String] = None,
    id: VariationId = allocateId(),
    parentId: Option[VariationId] = None,
    parentAnchorPly: Option[Int] = None,
    children: List[LiveVariation] = Nil
  ) {
    def preview(maxMoves: Int = 3): String = {
      val prefix =
        if (startingColor == "white") s"${startingMoveNumber}."
        else s"${startingMoveNumber}..."
      val head = moves.take(maxMoves).mkString(" ")
      val ellipsis = if (moves.length > maxMoves) " ..." else ""
      s"$prefix $head$ellipsis".trim
    }
  }

  private case class ActiveDraft(
    origin: VariationOrigin,
    baseNotationSize: Int,
    startingMoveNumber: Int,
    startingColor: String,
    originalFutureMoves: List[String],
    moves: List[String],
    resumedFrom: Option[LiveVariation],
    currentVariationId: Option[VariationId]
  ) {
    def anchorIndex: Int = origin.anchorIndex
    def parentContext: Option[(VariationId, Int)] = origin match
      case VariationOrigin.Mainline(_)                => None
      case VariationOrigin.Nested(_, parentId, ply) => Some(parentId -> ply)

    def toLiveVariation: Option[LiveVariation] =
      if moves.nonEmpty then
        val commentOpt = resumedFrom.flatMap(_.comment)
        val (parentIdOpt, parentPlyOpt) = parentContext match
          case Some((pid, ply)) => (Some(pid), Some(ply))
          case None             => (None, None)
        Some(
          LiveVariation(
            anchorIndex = origin.anchorIndex,
            startingMoveNumber = startingMoveNumber,
            startingColor = startingColor,
            moves = moves,
            comment = commentOpt,
            id = currentVariationId.getOrElse(0),
            parentId = parentIdOpt,
            parentAnchorPly = parentPlyOpt
          )
        )
      else None
  }

  private var variations: List[LiveVariation] = Nil
  private var activeDraft: Option[ActiveDraft] = None
  private var mainlineSnapshots: Map[(Int, Option[(VariationId, Int)]), List[String]] = Map.empty
  private def trace(action: String)(details: => String): Unit = ()

  private def describeVariation(variation: LiveVariation): String =
    val parentPart = variation.parentId.map(id => s"parent=$id ply=${variation.parentAnchorPly.getOrElse(-1)}").getOrElse("parent=<none>")
    val movesPart = if variation.moves.isEmpty then "<empty>" else variation.moves.mkString(" ")
    s"id=${variation.id} anchor=${variation.anchorIndex} start=${variation.startingMoveNumber}.${variation.startingColor} moves=$movesPart $parentPart"

  private def describeDraft(draft: ActiveDraft): String =
    val originPart = draft.origin match
      case VariationOrigin.Mainline(idx)            => s"Mainline(anchor=$idx)"
      case VariationOrigin.Nested(idx, pid, ply) => s"Nested(anchor=$idx parent=$pid ply=$ply)"
    val movesPart = if draft.moves.isEmpty then "<empty>" else draft.moves.mkString(" ")
    val futurePart = if draft.originalFutureMoves.isEmpty then "<empty>" else draft.originalFutureMoves.mkString(" ")
    val resumedPart = draft.resumedFrom.map(v => s"resumed=${v.id}").getOrElse("resumed=<none>")
    val currentId = draft.currentVariationId.map(_.toString).getOrElse("<none>")
    s"origin=$originPart base=${draft.baseNotationSize} start=${draft.startingMoveNumber}.${draft.startingColor} moves=$movesPart future=$futurePart $resumedPart currentId=$currentId"

  private def formatMoveList(moves: List[String]): String =
    if moves.isEmpty then "<empty>" else moves.mkString(" ")

  private def snapshotKey(anchorIndex: Int, parentContext: Option[(VariationId, Int)]): (Int, Option[(VariationId, Int)]) =
    anchorIndex -> parentContext

  private def recordMainlineSnapshot(anchorIndex: Int, parentContext: Option[(VariationId, Int)], moves: List[String]): Unit =
    val key = snapshotKey(anchorIndex, parentContext)
    val existing = mainlineSnapshots.get(key)
    if !existing.contains(moves) then
      mainlineSnapshots = mainlineSnapshots.updated(key, moves)
      trace("mainlineSnapshot.store") {
        val parentDesc = parentContext.map { case (pid, ply) => s"parent=$pid ply=$ply" }.getOrElse("parent=<none>")
        s"anchor=$anchorIndex moves=${formatMoveList(moves)} $parentDesc"
      }

  def addVariation(
    anchorIndex: Int,
    startingMoveNumber: Int,
    startingColor: String,
    moves: List[String],
    comment: Option[String] = None,
    parentId: Option[VariationId] = None,
    parentAnchorPly: Option[Int] = None
  ): Option[LiveVariation] =
    if moves.nonEmpty then
      val parentOpt = parentId.flatMap(id => findVariation(variations, id))
      val resolvedParentId = parentOpt.map(_.id)
      val offset = parentAnchorPly.getOrElse(0)
      val resolvedAnchorIndex = parentOpt match
        case Some(parentVariation) => parentVariation.anchorIndex + offset
        case None                  => anchorIndex
      val resolvedParentPly = parentOpt.flatMap(_ => parentAnchorPly)
      val variation = LiveVariation(resolvedAnchorIndex, startingMoveNumber, startingColor, moves, comment, parentId = resolvedParentId, parentAnchorPly = resolvedParentPly)
      trace("addVariation") {
        val parentDesc = resolvedParentId.map(id => s"parent=$id ply=${resolvedParentPly.getOrElse(-1)}").getOrElse("parent=<none>")
        s"anchor=$anchorIndex resolvedAnchor=$resolvedAnchorIndex start=${startingMoveNumber}.${startingColor} moves=${if moves.isEmpty then "<empty>" else moves.mkString(" ")} $parentDesc"
      }
      variations = appendVariation(variations, variation)
      trace("addVariation.store") {
        s"stored=${describeVariation(variation)} total=${getVariations.length}"
      }
      Some(variation)
    else None

  def addNestedVariation(
    parentId: VariationId,
    parentAnchorPly: Int,
    startingMoveNumber: Int,
    startingColor: String,
    moves: List[String],
    comment: Option[String] = None
  ): Option[LiveVariation] =
    val anchor = findVariation(variations, parentId).map(_.anchorIndex).getOrElse(-1)
    trace("addNestedVariation") {
      s"parent=$parentId anchor=$anchor ply=$parentAnchorPly start=${startingMoveNumber}.${startingColor} moves=${if moves.isEmpty then "<empty>" else moves.mkString(" ")}"
    }
    addVariation(anchor, startingMoveNumber, startingColor, moves, comment, Some(parentId), Some(parentAnchorPly))

  def clear(): Unit =
    trace("clear()")("clearing all live variations and active draft")
    variations = Nil
    activeDraft = None
    mainlineSnapshots = Map.empty

  def isEmpty: Boolean = variations.isEmpty

  def getVariations: List[LiveVariation] = flatten(variations)

  def getVariationTree: List[LiveVariation] = variations

  def variationsAtAnchor(anchorIndex: Int): List[LiveVariation] =
    def collect(nodes: List[LiveVariation]): List[LiveVariation] =
      nodes.flatMap { variation =>
        val self = if variation.anchorIndex == anchorIndex then List(variation) else Nil
        self ++ collect(variation.children)
      }
    collect(variations)

  def getVariationsIncludingDraft: List[LiveVariation] =
    val flattened = getVariations
    activeDraft match
      case Some(draft) if draft.currentVariationId.isEmpty =>
        val withoutEditingTarget = draft.resumedFrom match
          case Some(original) => flattened.filterNot(_.id == original.id)
          case None           => flattened
        withoutEditingTarget ++ draft.toLiveVariation.toList
      case _ => flattened

  def beginDraft(
    anchorIndex: Int,
    baseNotationSize: Int,
    startingMoveNumber: Int,
    startingColor: String,
    originalFutureMoves: List[String],
    resumedFrom: Option[LiveVariation] = None,
    parentContext: Option[(VariationId, Int)] = None
  ): Unit =
    trace("beginDraft.request") {
      val resumedDesc = resumedFrom.map(v => s"resumed=${describeVariation(v)}").getOrElse("resumed=<none>")
      val parentDesc = parentContext.map { case (pid, ply) => s"parent=$pid ply=$ply" }.getOrElse("parent=<none>")
      val futureDesc = if originalFutureMoves.isEmpty then "future=<empty>" else s"future=${originalFutureMoves.mkString(" ")}"
      s"anchor=$anchorIndex base=$baseNotationSize start=${startingMoveNumber}.${startingColor} $futureDesc $resumedDesc $parentDesc"
    }
    val origin = resumedFrom match
      case Some(existing) if existing.parentId.isDefined && existing.parentAnchorPly.isDefined =>
        VariationOrigin.Nested(existing.anchorIndex, existing.parentId.get, existing.parentAnchorPly.get)
      case Some(existing) =>
        VariationOrigin.Mainline(existing.anchorIndex)
      case None =>
        parentContext match
          case Some((parentId, ply)) =>
            val anchor = findVariation(variations, parentId).map(_.anchorIndex).getOrElse(anchorIndex)
            VariationOrigin.Nested(anchor, parentId, ply)
          case None =>
            VariationOrigin.Mainline(anchorIndex)

    activeDraft = Some(ActiveDraft(
      origin = origin,
      baseNotationSize = baseNotationSize,
      startingMoveNumber = startingMoveNumber,
      startingColor = startingColor,
      originalFutureMoves = originalFutureMoves,
      moves = Nil,
      resumedFrom = resumedFrom,
      currentVariationId = resumedFrom.map(_.id)
    ))
    activeDraft.foreach { draft =>
      recordMainlineSnapshot(draft.anchorIndex, draft.parentContext, draft.originalFutureMoves)
      trace("beginDraft.state") {
        describeDraft(draft)
      }
    }

  def updateDraftMoves(moves: List[String]): Unit =
    activeDraft = activeDraft.map { draft =>
      trace("updateDraftMoves") {
        val moveDesc = if moves.isEmpty then "<empty>" else moves.mkString(" ")
        s"currentId=${draft.currentVariationId.map(_.toString).getOrElse("<none>")} moves=$moveDesc"
      }
      draft.copy(moves = moves)
    }

  def updateDraftResumedFrom(variation: Option[LiveVariation]): Unit =
    activeDraft = activeDraft.map { draft =>
      val updatedOrigin = variation match
        case Some(existing) if existing.parentId.isDefined && existing.parentAnchorPly.isDefined =>
          VariationOrigin.Nested(existing.anchorIndex, existing.parentId.get, existing.parentAnchorPly.get)
        case Some(existing) =>
          VariationOrigin.Mainline(existing.anchorIndex)
        case None =>
          draft.origin
      val updatedId = variation.map(_.id).orElse(draft.currentVariationId)
      trace("updateDraftResumedFrom") {
        val resumedDesc = variation.map(describeVariation).getOrElse("<none>")
        s"previous=${describeDraft(draft)} resumed=$resumedDesc newId=${updatedId.getOrElse("<none>")}"
      }
      draft.copy(resumedFrom = variation, origin = updatedOrigin, currentVariationId = updatedId)
    }

  def discardDraft(): Unit =
    trace("discardDraft()") {
      activeDraft.map(describeDraft).getOrElse("no active draft")
    }
    activeDraft = None

  def discardDraftAndRevert(): Unit =
    activeDraft match
      case Some(draft) =>
        trace("discardDraftAndRevert.begin") {
          describeDraft(draft)
        }
        draft.currentVariationId.foreach { id =>
          draft.resumedFrom match
            case Some(original) =>
              trace("discardDraftAndRevert.restoreResumed") {
                describeVariation(original)
              }
              replaceVariation(original, original.moves, original.comment)
            case None =>
              trace("discardDraftAndRevert.removeNew") {
                s"id=$id"
              }
              removeVariation(id)
        }
        activeDraft = None
        trace("discardDraftAndRevert.end")("draft cleared")
      case None =>
        ()

  def persistDraftSnapshot(): Unit =
    activeDraft match
      case Some(draft) if draft.moves.isEmpty =>
        trace("persistDraftSnapshot.empty") {
          describeDraft(draft)
        }
        draft.currentVariationId.foreach { id =>
          draft.resumedFrom match
            case Some(original) =>
              trace("persistDraftSnapshot.restoreOriginal") {
                describeVariation(original)
              }
              replaceVariation(original, original.moves, original.comment)
              activeDraft = Some(draft.copy(currentVariationId = Some(original.id)))
            case None =>
              trace("persistDraftSnapshot.removeEmpty") {
                s"id=$id"
              }
              removeVariation(id)
              activeDraft = Some(draft.copy(currentVariationId = None))
        }
        if draft.currentVariationId.isEmpty then
          activeDraft = Some(draft.copy(currentVariationId = draft.resumedFrom.map(_.id)))
      case Some(draft) =>
        trace("persistDraftSnapshot.nonEmpty") {
          describeDraft(draft)
        }
        val commentOpt = draft.resumedFrom.flatMap(_.comment)
        draft.currentVariationId match
          case Some(existingId) =>
            val target = findVariation(variations, existingId).orElse(draft.resumedFrom)
            target.foreach { original =>
              trace("persistDraftSnapshot.updateExisting") {
                s"id=$existingId original=${describeVariation(original)} newMoves=${if draft.moves.isEmpty then "<empty>" else draft.moves.mkString(" ")}"
              }
              replaceVariation(original, draft.moves, original.comment.orElse(commentOpt))
            }
            activeDraft = Some(draft.copy(currentVariationId = Some(existingId)))
          case None =>
            val (parentIdOpt, parentPlyOpt) = draft.parentContext match
              case Some((pid, ply)) => (Some(pid), Some(ply))
              case None             => (None, None)
            trace("persistDraftSnapshot.createNew") {
              val parentDesc = parentIdOpt.map(id => s"parent=$id ply=${parentPlyOpt.getOrElse(-1)}").getOrElse("parent=<none>")
              s"anchor=${draft.anchorIndex} start=${draft.startingMoveNumber}.${draft.startingColor} moves=${if draft.moves.isEmpty then "<empty>" else draft.moves.mkString(" ")} $parentDesc"
            }
            val created = addVariation(
              anchorIndex = draft.anchorIndex,
              startingMoveNumber = draft.startingMoveNumber,
              startingColor = draft.startingColor,
              moves = draft.moves,
              comment = commentOpt,
              parentId = parentIdOpt,
              parentAnchorPly = parentPlyOpt
            )
            activeDraft = Some(draft.copy(currentVariationId = created.map(_.id)))
      case None =>
        ()

  def commitDraft(): Unit =
    trace("commitDraft.begin") {
      activeDraft.map(describeDraft).getOrElse("no active draft")
    }
    persistDraftSnapshot()
    activeDraft = None
    trace("commitDraft.end")("active draft cleared")

  def overlayMainline(notations: List[String]): List[String] =
    trace("overlayMainline") {
      val active = activeDraft.map(describeDraft).getOrElse("no active draft")
      val history = if notations.isEmpty then "<empty>" else notations.mkString(" ")
      s"notations=$history draft=$active"
    }
    activeDraft match
      case Some(draft) if draft.baseNotationSize <= notations.length =>
        val prefix = notations.take(draft.baseNotationSize)
        prefix ++ draft.originalFutureMoves
      case Some(draft) if draft.baseNotationSize == notations.length + 1 =>
        // Edge case: viewing position before the anchor removes the last half-move from history
        notations ++ draft.originalFutureMoves
      case Some(draft) =>
        draft.originalFutureMoves
      case None => notations

  def recordOriginalFutureMoves(
    anchorIndex: Int,
    moves: List[String],
    parentContext: Option[(VariationId, Int)] = None
  ): Unit =
    recordMainlineSnapshot(anchorIndex, parentContext, moves)

  def originalFutureMovesFor(
    anchorIndex: Int,
    parentContext: Option[(VariationId, Int)] = None
  ): Option[List[String]] =
    mainlineSnapshots.get(snapshotKey(anchorIndex, parentContext))

  def restoreMainline(notations: List[String]): List[String] =
    val entries = mainlineSnapshots.collect {
      case ((anchorIdx, None), moves) => anchorIdx -> moves
    }.toList.sortBy(_._1)
    val restored = entries.foldLeft(notations) { case (current, (anchorIdx, moves)) =>
      val prefixLength = math.max(anchorIdx + 1, 0)
      val prefix = if prefixLength <= current.length then current.take(prefixLength) else current
      val result = prefix ++ moves
      trace("restoreMainline.apply") {
        s"anchor=$anchorIdx prefixLength=$prefixLength replacement=${formatMoveList(moves)} outcome=${formatMoveList(result.drop(prefixLength))}"
      }
      result
    }
    trace("restoreMainline.result") {
      s"notations=${formatMoveList(notations)} restored=${formatMoveList(restored)}"
    }
    restored

  def replaceVariation(original: LiveVariation, updatedMoves: List[String], comment: Option[String] = None): Unit = {
    val mergedComment = comment.orElse(original.comment)
    trace("replaceVariation") {
      val movesDesc = if updatedMoves.isEmpty then "<empty>" else updatedMoves.mkString(" ")
      s"target=${describeVariation(original)} newMoves=$movesDesc"
    }
    variations = updateVariation(variations, original.id, _.copy(moves = updatedMoves, comment = mergedComment))
  }

  def removeVariation(variationId: VariationId): Unit =
    trace("removeVariation") {
      s"id=$variationId"
    }
    variations = deleteVariation(variations, variationId)

  def replaceWithPGNVariations(game: PGNGame): Unit =
    def buildFromPGN(pgnVariation: PGNVariation, parent: Option[LiveVariation]): Option[LiveVariation] =
      val moves = flattenPGNMoves(pgnVariation.moves)
      if moves.isEmpty then None
      else
        val originAnchor = anchorIndexFromPGN(pgnVariation.originMoveNumber, pgnVariation.originColor)
        val (parentIdOpt, parentPlyOpt, resolvedAnchorIndex) = parent match
          case Some(parentVariation) =>
            val ply = (pgnVariation.originMoveNumber, pgnVariation.originColor) match
              case (Some(moveNo), Some(color)) =>
                Some(plyOffset(parentVariation.startingMoveNumber, parentVariation.startingColor, moveNo, color))
              case _ => None
            val offset = ply.getOrElse(0)
            (Some(parentVariation.id), ply, parentVariation.anchorIndex + offset)
          case None =>
            (None, None, originAnchor)

        val base = LiveVariation(
          anchorIndex = resolvedAnchorIndex,
          startingMoveNumber = pgnVariation.startingMoveNumber,
          startingColor = pgnVariation.startingColor,
          moves = moves,
          comment = pgnVariation.comment,
          parentId = parentIdOpt,
          parentAnchorPly = parentPlyOpt
        )

        val childContext = Some(base)
        val leadingChildren = pgnVariation.leadingVariations.flatMap(buildFromPGN(_, childContext)).toList
        val moveChildren = pgnVariation.moves.flatMap(move => move.variations.flatMap(buildFromPGN(_, childContext))).toList
        Some(base.copy(children = leadingChildren ++ moveChildren))

    val rootVariations =
      game.variations.flatMap(buildFromPGN(_, None)).toList ++
        game.moves.flatMap(move => move.variations.flatMap(buildFromPGN(_, None))).toList

    variations = rootVariations
    activeDraft = None
    mainlineSnapshots = Map.empty

  def variationIndicators(maxMoves: Int = 3): Map[Int, List[String]] =
    getVariations
      .groupBy(_.anchorIndex)
      .view
      .mapValues(_.map(_.preview(maxMoves)))
      .toMap

  def exportVariations: List[PGNVariation] =
    variations.map(variation => toPGNVariation(variation, parent = None))

  private def flatten(nodes: List[LiveVariation]): List[LiveVariation] =
    nodes.flatMap(v => v :: flatten(v.children))

  private def appendVariation(nodes: List[LiveVariation], variation: LiveVariation): List[LiveVariation] =
    variation.parentId match
      case None => nodes :+ variation
      case Some(parentId) =>
        nodes.map { node =>
          if node.id == parentId then node.copy(children = node.children :+ variation)
          else node.copy(children = appendVariation(node.children, variation))
        }

  private def deleteVariation(nodes: List[LiveVariation], targetId: VariationId): List[LiveVariation] =
    nodes.flatMap { node =>
      if node.id == targetId then Nil
      else List(node.copy(children = deleteVariation(node.children, targetId)))
    }

  private def updateVariation(
    nodes: List[LiveVariation],
    targetId: VariationId,
    update: LiveVariation => LiveVariation
  ): List[LiveVariation] =
    nodes.map { node =>
      if node.id == targetId then update(node)
      else node.copy(children = updateVariation(node.children, targetId, update))
    }

  private def findVariation(nodes: List[LiveVariation], targetId: VariationId): Option[LiveVariation] =
    nodes.view.flatMap { node =>
      if node.id == targetId then Some(node)
      else findVariation(node.children, targetId)
    }.headOption

  private def plyOffset(
    startingMoveNumber: Int,
    startingColor: String,
    targetMoveNumber: Int,
    targetColor: String
  ): Int = {
    val moveDiff = targetMoveNumber - startingMoveNumber
    val base = moveDiff * 2
    (startingColor.toLowerCase, targetColor.toLowerCase) match
      case ("white", "white") => base
      case ("white", "black") => base + 1
      case ("black", "black") => base
      case ("black", "white") => base + 1
      case _                    => base
  }

  private def moveColorFromPly(
    startingMoveNumber: Int,
    startingColor: String,
    plyOffset: Int
  ): (Int, String) = {
    val moveNumber = startingMoveNumber + (plyOffset / 2)
    val sameColor = plyOffset % 2 == 0
    val color = startingColor.toLowerCase match
      case "white" => if sameColor then "white" else "black"
      case "black" => if sameColor then "black" else "white"
      case other    => other
    (moveNumber, color)
  }

  private def toPGNVariation(variation: LiveVariation, parent: Option[LiveVariation]): PGNVariation = {
    val moveList = PGNMoveBuilder.fromSanSequence(variation.startingMoveNumber, variation.startingColor, variation.moves)
    val clearedMoves = moveList.map(_.copy(variations = Nil))

    val origin = parent match
      case Some(parentVariation) =>
        variation.parentAnchorPly.flatMap { ply =>
          val (moveNumber, color) = moveColorFromPly(parentVariation.startingMoveNumber, parentVariation.startingColor, ply)
          Some(moveNumber -> color)
        }
      case None => anchorIndexToOrigin(variation.anchorIndex)

    val childConversions = variation.children.map(child => child -> toPGNVariation(child, Some(variation)))

    val (leadingChildren, anchoredChildren) = childConversions.partition { case (child, _) => child.parentAnchorPly.forall(_ == 0) }
    val leadingPGN = leadingChildren.map(_._2)

    val movesWithChildren = anchoredChildren.foldLeft(clearedMoves) {
      case (acc, (childLive, childPGN)) =>
        val ply = childLive.parentAnchorPly.getOrElse(0)
        val (moveNumber, _) = moveColorFromPly(variation.startingMoveNumber, variation.startingColor, ply)
        attachVariationToMove(acc, moveNumber, childPGN)
    }

    PGNVariation(
      startingMoveNumber = variation.startingMoveNumber,
      startingColor = variation.startingColor,
      moves = movesWithChildren,
      comment = variation.comment,
      originMoveNumber = origin.map(_._1),
      originColor = origin.map(_._2),
      leadingVariations = leadingPGN
    )
  }

  private def attachVariationToMove(moves: List[PGNMove], moveNumber: Int, variation: PGNVariation): List[PGNMove] =
    val targetIndex = moves.indexWhere(_.moveNumber == moveNumber)
    if targetIndex < 0 then moves
    else
      val targetMove = moves(targetIndex)
      val shouldShiftToNext =
        variation.originColor.contains("white") && targetMove.whiteMove.isEmpty
      if shouldShiftToNext then
        val nextIndex = moves.indexWhere(_.moveNumber == moveNumber + 1)
        if nextIndex >= 0 then
          val nextMove = moves(nextIndex)
          moves.updated(nextIndex, nextMove.copy(variations = nextMove.variations :+ variation))
        else
          moves.updated(targetIndex, targetMove.copy(variations = targetMove.variations :+ variation))
      else
        moves.updated(targetIndex, targetMove.copy(variations = targetMove.variations :+ variation))

  private def flattenPGNMoves(moves: List[PGNMove]): List[String] =
    moves.flatMap { move =>
      move.whiteMove.toList ++ move.blackMove.toList
    }.map(_.trim).filter(_.nonEmpty)

  private def anchorIndexFromPGN(originMoveNumber: Option[Int], originColor: Option[String]): Int =
    (originMoveNumber, originColor) match
      case (Some(moveNumber), Some(color)) =>
        color.toLowerCase match
          case "white" => (moveNumber - 1) * 2 - 1
          case "black" => (moveNumber - 1) * 2
          case _        => (moveNumber - 1) * 2
      case _ => -1

  private def anchorIndexToOrigin(anchorIndex: Int): Option[(Int, String)] =
    if anchorIndex < 0 then None
    else
      val moveNumber = ((anchorIndex + 1) / 2) + 1
      val color = if ((anchorIndex + 1) % 2 == 0) "white" else "black"
      Some(moveNumber -> color)

}

object LiveVariationRepository {
  type VariationId = Long

  sealed trait VariationOrigin {
    def anchorIndex: Int
  }

  object VariationOrigin {
    final case class Mainline(anchorIndex: Int) extends VariationOrigin
    final case class Nested(anchorIndex: Int, parentId: VariationId, parentAnchorPly: Int) extends VariationOrigin
  }
}
