package chess.pgn

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

/**
 * PGN Parser - Parses PGN format games into structured data
 * Supports headers, moves, variations, comments, and annotations
 */
object PGNParser {

  // Regular expressions for PGN parsing
  private val headerPattern: Regex = """\[(\w+)\s+"([^"]*)"\]""".r

  private sealed trait Token
  private case class MoveNumberToken(number: Int, isBlack: Boolean) extends Token
  private case class MoveToken(value: String) extends Token
  private case class CommentToken(text: String) extends Token
  private case class NagToken(symbol: String, code: Int) extends Token
  private case class ResultToken(value: String) extends Token
  private case object VariationStartToken extends Token
  private case object VariationEndToken extends Token

  private case class ParseResult(
    moves: List[PGNMove],
    result: Option[String],
    leadingVariations: List[PGNVariation],
    terminatedByVariationEnd: Boolean
  )

  /**
   * Parse a complete PGN string into a PGNGame
   */
  def parsePGN(pgnText: String): PGNParsingResult = {
    try {
      val lines = pgnText.split("\n").map(_.trim).filter(_.nonEmpty)
      val (headers, moveTextStart) = parseHeaders(lines)

      if (moveTextStart >= lines.length) {
        return PGNParsingResult(None, List("No move text found in PGN"))
      }

      val moveText = lines.drop(moveTextStart).mkString(" ")
      val (moves, result, errors, warnings, rootVariations) = parseMoves(moveText)

      val game = PGNGame(
        headers = headers,
        moves = moves,
        result = result,
        variations = rootVariations
      )

      PGNParsingResult(Some(game), errors, warnings)
    } catch {
      case e: Exception =>
        PGNParsingResult(None, List(s"Parsing error: ${e.getMessage}"))
    }
  }

  /**
   * Parse PGN headers from the beginning of the file
   */
  private def parseHeaders(lines: Array[String]): (Map[String, String], Int) = {
    val headers = scala.collection.mutable.Map[String, String]()
    var lineIndex = 0

    while (lineIndex < lines.length) {
      val line = lines(lineIndex)
      if (line.startsWith("[") && line.endsWith("]")) {
        headerPattern.findFirstMatchIn(line).foreach { m =>
          headers(m.group(1)) = m.group(2)
        }
        lineIndex += 1
      } else {
        // End of headers
        return (headers.toMap, lineIndex)
      }
    }

    (headers.toMap, lineIndex)
  }

  /**
   * Parse the move text section of PGN
   */
  private def parseMoves(moveText: String): (List[PGNMove], Option[String], List[String], List[String], List[PGNVariation]) = {
    val errors = ListBuffer[String]()
    val warnings = ListBuffer[String]()

    val cleanText = moveText.replaceAll("\\s+", " ").trim
    val tokens = tokenizeMoves(cleanText)
    val stream = new TokenStream(tokens)

    val result = parseMoveList(
      stream = stream,
      startingMoveNumber = 1,
      startingColor = "white",
      stopAtVariationEnd = false,
      errors = errors,
      warnings = warnings
    )

    (result.moves, result.result, errors.toList, warnings.toList, result.leadingVariations)
  }

  /**
   * Recursively parse a sequence of moves (main line or variation)
   */
  private def parseMoveList(
    stream: TokenStream,
    startingMoveNumber: Int,
    startingColor: String,
    stopAtVariationEnd: Boolean,
    errors: ListBuffer[String],
    warnings: ListBuffer[String]
  ): ParseResult = {
    val moves = ListBuffer[PGNMove]()
    val leadingVariations = ListBuffer[PGNVariation]()

    var result: Option[String] = None
    var currentMoveNumber = startingMoveNumber
    var colorToMove = startingColor
    var pendingMove: Option[MoveBuilder] = None
    var lastMoveRef: Option[(Int, String)] = None
    var terminatedByVariationEnd = false

    def finalizePending(): Unit = {
      pendingMove.foreach { builder =>
        if (builder.hasContent) {
          moves += builder.build()
        }
      }
      pendingMove = None
    }

    def attachVariation(variation: PGNVariation): Unit = {
      pendingMove match {
        case Some(builder) if builder.hasContent =>
          builder.addVariation(variation)
        case Some(builder) if !builder.hasContent && variation.originColor.isDefined =>
          builder.addVariation(variation)
        case _ =>
          if (lastMoveRef.isDefined && moves.nonEmpty) {
            val (originMove, _) = lastMoveRef.get
            val lastIndex = moves.lastIndexWhere(_.moveNumber == originMove)
            if (lastIndex >= 0) {
              val updated = moves(lastIndex).copy(variations = moves(lastIndex).variations :+ variation)
              moves(lastIndex) = updated
            } else {
              leadingVariations += variation
            }
          } else {
            leadingVariations += variation
          }
      }
    }

    while (stream.hasNext) {
      stream.peek match {
        case VariationEndToken if stopAtVariationEnd =>
          terminatedByVariationEnd = true
          stream.next()
          finalizePending()
          return ParseResult(moves.toList, result, leadingVariations.toList, terminatedByVariationEnd)

        case VariationEndToken =>
          warnings += "Unmatched variation end detected"
          stream.next()

        case ResultToken(value) =>
          result = Some(value)
          stream.next()
          finalizePending()
          return ParseResult(moves.toList, result, leadingVariations.toList, terminatedByVariationEnd)

        case MoveNumberToken(number, isBlack) =>
          if (pendingMove.exists(_.moveNumber != number)) {
            finalizePending()
          }
          currentMoveNumber = number
          colorToMove = if (isBlack) "black" else "white"
          if (isBlack && pendingMove.forall(_.moveNumber != number)) {
            pendingMove = Some(new MoveBuilder(number))
          }
          stream.next()

        case MoveToken(rawValue) =>
          val value = rawValue.trim
          if (isMoveToken(value)) {
            val builder = pendingMove match {
              case Some(existing) if existing.moveNumber == currentMoveNumber => existing
              case Some(_) =>
                finalizePending()
                val created = new MoveBuilder(currentMoveNumber)
                pendingMove = Some(created)
                created
              case None =>
                val created = new MoveBuilder(currentMoveNumber)
                pendingMove = Some(created)
                created
            }

            val added = builder.setMove(colorToMove, value)
            if (!added) {
              warnings += s"Multiple $colorToMove moves found for move number ${builder.moveNumber}"
            }

            lastMoveRef = Some((builder.moveNumber, colorToMove))

            colorToMove = if (colorToMove == "white") {
              "black"
            } else {
              currentMoveNumber = builder.moveNumber + 1
              "white"
            }
          } else if (value.nonEmpty) {
            warnings += s"Unknown token: $value"
          }
          stream.next()

        case CommentToken(textRaw) =>
          val text = textRaw.trim
          if (text.nonEmpty) {
            if (pendingMove.isDefined && lastMoveRef.isDefined) {
              val builder = pendingMove.get
              lastMoveRef.foreach {
                case (_, "white") => builder.appendComment("white", text)
                case (_, "black") => builder.appendComment("black", text)
              }
            } else if (lastMoveRef.isDefined && moves.nonEmpty) {
              val (moveNumber, color) = lastMoveRef.get
              val idx = moves.lastIndexWhere(_.moveNumber == moveNumber)
              if (idx >= 0) {
                val move = moves(idx)
                val updated = color match {
                  case "white" => move.copy(whiteComment = combineComments(move.whiteComment, Some(text)))
                  case "black" => move.copy(blackComment = combineComments(move.blackComment, Some(text)))
                }
                moves(idx) = updated
              } else {
                leadingVariations += PGNVariation(
                  startingMoveNumber = currentMoveNumber,
                  startingColor = colorToMove,
                  moves = List.empty,
                  comment = Some(text)
                )
              }
            } else {
              leadingVariations += PGNVariation(
                startingMoveNumber = currentMoveNumber,
                startingColor = colorToMove,
                moves = List.empty,
                comment = Some(text)
              )
            }
          }
          stream.next()

        case NagToken(symbol, _) =>
          if (pendingMove.isDefined && lastMoveRef.isDefined) {
            val builder = pendingMove.get
            val (_, color) = lastMoveRef.get
            builder.addAnnotation(PGNAnnotation(symbol, color))
          } else if (lastMoveRef.isDefined && moves.nonEmpty) {
            val (moveNumber, color) = lastMoveRef.get
            val idx = moves.lastIndexWhere(_.moveNumber == moveNumber)
            if (idx >= 0) {
              val move = moves(idx)
              moves(idx) = move.copy(annotations = move.annotations :+ PGNAnnotation(symbol, color))
            }
          } else {
            warnings += s"Annotation $symbol appears before any move"
          }
          stream.next()

        case VariationStartToken =>
          val savedLastMove = lastMoveRef
          val savedMoveNumber = currentMoveNumber
          val savedColorToMove = colorToMove

          stream.next()

          val variationResult = parseMoveList(
            stream = stream,
            startingMoveNumber = savedMoveNumber,
            startingColor = savedColorToMove,
            stopAtVariationEnd = true,
            errors = errors,
            warnings = warnings
          )

          if (!variationResult.terminatedByVariationEnd) {
            errors += "Unmatched variation parentheses"
          }

          val variationComment = stream.peekOption.collect {
            case CommentToken(text) =>
              stream.next()
              text.trim
          }.filter(_.nonEmpty)

          val variationMoves = variationResult.moves
          val startingMove = variationMoves.headOption.map(_.moveNumber).getOrElse(savedMoveNumber)
          val startingColor = determineStartingColor(variationMoves, savedColorToMove)

          if (variationMoves.isEmpty && variationResult.leadingVariations.isEmpty && variationComment.isEmpty) {
            warnings += "Empty variation encountered and skipped"
          } else {
            val variation = PGNVariation(
              startingMoveNumber = startingMove,
              startingColor = startingColor,
              moves = variationMoves,
              comment = variationComment,
              originMoveNumber = savedLastMove.map(_._1),
              originColor = savedLastMove.map(_._2),
              leadingVariations = variationResult.leadingVariations
            )
            attachVariation(variation)
          }
      }
    }

    finalizePending()
    ParseResult(moves.toList, result, leadingVariations.toList, terminatedByVariationEnd)
  }

  /**
   * Tokenize move text into individual components
   */
  private def tokenizeMoves(moveText: String): Vector[Token] = {
    val tokens = ListBuffer[Token]()
    val length = moveText.length
    var index = 0

    def readWhile(condition: Char => Boolean): String = {
      val start = index
      while (index < length && condition(moveText.charAt(index))) {
        index += 1
      }
      moveText.substring(start, index)
    }

    def readSanToken(): String = {
      val start = index
      while (index < length && !Character.isWhitespace(moveText.charAt(index)) && "{}()$;".indexOf(moveText.charAt(index)) == -1) {
        index += 1
      }
      moveText.substring(start, index)
    }

    while (index < length) {
      moveText.charAt(index) match {
        case c if c.isWhitespace =>
          index += 1

        case '{' =>
          val start = index + 1
          var depth = 1
          index += 1
          while (index < length && depth > 0) {
            moveText.charAt(index) match {
              case '{' => depth += 1
              case '}' => depth -= 1
              case _ =>
            }
            index += 1
          }
          val end = if (depth == 0) index - 1 else length
          val comment = moveText.substring(start, end).trim
          tokens += CommentToken(comment)

        case ';' =>
          index += 1
          val start = index
          while (index < length && moveText.charAt(index) != '\n') {
            index += 1
          }
          val comment = moveText.substring(start, index).trim
          tokens += CommentToken(comment)

        case '(' =>
          tokens += VariationStartToken
          index += 1

        case ')' =>
          tokens += VariationEndToken
          index += 1

        case '$' =>
          index += 1
          val digits = readWhile(_.isDigit)
          if (digits.nonEmpty) {
            val nagNumber = digits.toInt
            tokens += NagToken(nagToSymbol(nagNumber), nagNumber)
          }

        case '*' =>
          tokens += ResultToken("*")
          index += 1

        case _ if moveText.startsWith("1/2-1/2", index) =>
          tokens += ResultToken("1/2-1/2")
          index += 7

        case _ if moveText.startsWith("1-0", index) =>
          tokens += ResultToken("1-0")
          index += 3

        case _ if moveText.startsWith("0-1", index) =>
          tokens += ResultToken("0-1")
          index += 3

        case c if c.isDigit =>
          val digits = readWhile(_.isDigit)
          var dotCount = 0
          while (index < length && moveText.charAt(index) == '.') {
            dotCount += 1
            index += 1
          }
          if (dotCount > 0) {
            tokens += MoveNumberToken(digits.toInt, dotCount >= 3)
          } else {
            val san = readSanToken()
            if (san.nonEmpty) tokens += MoveToken(san)
          }

        case _ =>
          val san = readSanToken()
          if (san.nonEmpty) {
            tokens += MoveToken(san)
          } else {
            index += 1
          }
      }
    }

    tokens.toVector
  }

  /**
   * Helper class for sequential token access
   */
  private final class TokenStream(tokens: Vector[Token]) {
    private var index = 0

    def hasNext: Boolean = index < tokens.length

    def peek: Token = tokens(index)

    def peekOption: Option[Token] = if (hasNext) Some(tokens(index)) else None

    def next(): Token = {
      val token = tokens(index)
      index += 1
      token
    }
  }

  /**
   * Check if a token represents a chess move
   */
  private def isMoveToken(token: String): Boolean = {
    val clean = token.replaceAll("[+#!?]", "")
    clean.matches(".*[NBRQK]?[a-h]?[1-8]?x?[a-h][1-8].*") ||
      clean.matches("O-O|O-O-O|0-0|0-0-0")
  }

  /**
   * Combine comments intelligently
   */
  private def combineComments(existing: Option[String], addition: Option[String]): Option[String] = {
    (existing, addition) match {
      case (None, None) => None
      case (Some(a), None) => Some(a)
      case (None, Some(b)) => Some(b)
      case (Some(a), Some(b)) if a.isEmpty => Some(b)
      case (Some(a), Some(b)) if b.isEmpty => Some(a)
      case (Some(a), Some(b)) => Some(s"$a\n$b")
    }
  }

  /**
   * Determine starting color for a variation
   */
  private def determineStartingColor(moves: List[PGNMove], fallback: String): String = {
    moves.headOption match {
      case Some(first) if first.whiteMove.isDefined => "white"
      case Some(first) if first.blackMove.isDefined => "black"
      case _ => fallback
    }
  }

  /**
   * Convert NAG number to symbol
   */
  private def nagToSymbol(nag: Int): String = {
    nag match {
      case 1 => "!"
      case 2 => "?"
      case 3 => "!!"
      case 4 => "??"
      case 5 => "!?"
      case 6 => "?!"
      case 7 => "□"
      case 10 => "="
      case 13 => "∞"
      case 14 => "⩲"
      case 15 => "⩱"
      case 16 => "±"
      case 17 => "∓"
      case 18 => "+−"
      case 19 => "−+"
      case 22 => "⨀"
      case 32 => "⟪"
      case 33 => "⟫"
      case other => s"$$$other"
    }
  }

  /**
   * Builder for PGNMove instances during parsing
   */
  private final class MoveBuilder(val moveNumber: Int) {
    private var whiteMove: Option[String] = None
    private var blackMove: Option[String] = None
    private var whiteComment: Option[String] = None
    private var blackComment: Option[String] = None
    private val annotations = ListBuffer[PGNAnnotation]()
    private val variations = ListBuffer[PGNVariation]()

    def hasContent: Boolean = whiteMove.isDefined || blackMove.isDefined

    def setMove(color: String, notation: String): Boolean = {
      color match {
        case "white" if whiteMove.isEmpty =>
          whiteMove = Some(notation)
          true
        case "black" if blackMove.isEmpty =>
          blackMove = Some(notation)
          true
        case "white" => false
        case "black" => false
        case _ => false
      }
    }

    def appendComment(color: String, comment: String): Unit = {
      color match {
        case "white" => whiteComment = combineComments(whiteComment, Some(comment))
        case "black" => blackComment = combineComments(blackComment, Some(comment))
        case _ =>
      }
    }

    def addAnnotation(annotation: PGNAnnotation): Unit = {
      annotations += annotation
    }

    def addVariation(variation: PGNVariation): Unit = {
      variations += variation
    }

    def build(): PGNMove = PGNMove(
      moveNumber = moveNumber,
      whiteMove = whiteMove,
      blackMove = blackMove,
      whiteComment = whiteComment,
      blackComment = blackComment,
      variations = variations.toList,
      annotations = annotations.toList
    )
  }
}