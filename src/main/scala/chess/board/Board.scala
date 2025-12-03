package chess.board

import chess.pieces._
import chess.types.HighlightType._
import chess.controllers.GameController
import chess.utils.{ImageManager, NoOpImageObserver}
import chess.intelligent.hangingPieces
import java.awt.{Graphics2D, Point, Toolkit, Dimension, Color}
import scala.swing.{Component, Reactor}
import java.awt.image.BufferedImage
import java.awt.BasicStroke

class Board(val gameController: GameController) extends Component with Reactor {
  val precomputedLightImages: Array[BufferedImage] = precomputeRotatedImages("light-square.png")
  val precomputedDarkImages: Array[BufferedImage] = precomputeRotatedImages("dark-square.png")
  var flipped: Boolean = false
  var screenSize: Dimension = Toolkit.getDefaultToolkit.getScreenSize
  var Sz: Int = screenSize.width / 24
  var border_w = Sz / 3
  var boardSize: Int = 8 * Sz
  var sizeUnit2: Int = screenSize.width / 30
  val squares: Array[Array[Square]] = Array.ofDim[Square](8, 8)
  var head_border_l = boardSize + Sz/2
  var borders = Map(
    "top" -> new Border(0, 0, head_border_l, border_w),
    "bottom" -> new Border(0, boardSize + border_w, head_border_l, border_w),
    "left" -> new Border(0, border_w, border_w, boardSize),
    "right" -> new Border(boardSize + border_w, border_w, border_w, boardSize)
  )
  var draggedPiece: Option[Piece] = None
  var draggedPieceX: Int = 0
  var draggedPieceY: Int = 0
  var enPassantTarget: Option[Square] = None

  private def precomputeRotatedImages(imageName: String): Array[BufferedImage] =
    val baseImage = ImageManager.getImage(imageName).asInstanceOf[BufferedImage]
    Seq(0, 90, 180, 270).map(angle => ImageManager.rotateImage(baseImage, angle)).toArray
  end precomputeRotatedImages

  this.preferredSize = new Dimension(boardSize + 2 * border_w, boardSize + 2 * border_w)

  // Luokan sisältämä vaikutuksellinen toiminto
  for (row <- 0 until 8; col <- 0 until 8) {
    val squareName = (('a' + col).toChar.toString) + (8 - row).toString
    val position = new Point(col * Sz + border_w, row * Sz + border_w)
    val squareImage =
      if ((row + col) % 2 == 0) then
        precomputedLightImages(scala.util.Random.nextInt(precomputedLightImages.length))
      else
        precomputedDarkImages(scala.util.Random.nextInt(precomputedDarkImages.length))
    squares(row)(col) = new Square(squareName.toLowerCase, position, this, squareImage)
}
  initializePieces()
  // Luokan aktiivialue päättyy

  private def initializePieces(): Unit =
    for (col <- 0 until 8)
      new Pawn("white", squares(6)(col), this)
      new Pawn("black", squares(1)(col), this)

    new Rook("white", squares(7)(0), this)
    new Rook("white", squares(7)(7), this)
    new Rook("black", squares(0)(0), this)
    new Rook("black", squares(0)(7), this)
    new Knight("white", squares(7)(1), this)
    new Knight("white", squares(7)(6), this)
    new Knight("black", squares(0)(1), this)
    new Knight("black", squares(0)(6), this)
    new Bishop("white", squares(7)(2), this)
    new Bishop("white", squares(7)(5), this)
    new Bishop("black", squares(0)(2), this)
    new Bishop("black", squares(0)(5), this)
    new Queen("white", squares(7)(3), this)
    new Queen("black", squares(0)(3), this)
    new King("white", squares(7)(4), this)
    new King("black", squares(0)(4), this)
  end initializePieces


  def generateAttackSquaresBuffer(color: String): List[Square] =
    val attackSquares = scala.collection.mutable.Set[Square]()
    for {
      row <- squares
      square <- row
      piece <- square.occupiedBy if piece.color == color
    } {
      piece.attackSquares().foreach(attackSquares += _)
    }
    attackSquares.toList
  end generateAttackSquaresBuffer

  def selectSquare(square: Square): Unit =
    deselectAll()
    square.select()
    square.occupiedBy.foreach { piece =>
      val validMoves = gameController.filterMovesToPreventCheck(piece, piece.possibleMoves())
      validMoves.foreach { moveSquare =>
        moveSquare.occupiedBy match
          case Some(capturedPiece) if capturedPiece.color != piece.color =>
            moveSquare.highlight(Capture)
          case _ =>
            moveSquare.highlight(Move1)}}
    repaint()
  end selectSquare

  def deselectAll(): Unit =
    for (row <- squares; square <- row) {
      square.resetColor()
    }
    repaint()
  end deselectAll
  
  // Update all square positions after board size changes
  def updateSquarePositions(): Unit =
    for (row <- 0 until 8; col <- 0 until 8) {
      val position = new Point(col * Sz + border_w, row * Sz + border_w)
      squares(row)(col).updatePosition(position)
    }
  end updateSquarePositions

  override def paintComponent(g: Graphics2D): Unit =
    super.paintComponent(g)
    
    // Draw large grey background square that covers entire board area
    g.setColor(new Color(90, 90, 90))
    g.fillRect(0, 0, boardSize + 2 * border_w, boardSize + 2 * border_w)
    
    // Draw chess squares on top of grey background
    for (row <- 0 until 8; col <- 0 until 8) {
      if flipped then
        val flip_row = 7-row
        val flip_col = 7-col
        val x = (flip_col * Sz) + border_w
        val y = (flip_row * Sz) + border_w
        squares(row)(col).draw(g, x, y)
        squares(row)(col).updatePosition(new Point(x + Sz/2, y + Sz/2))
      else
        val x = (col * Sz) + border_w
        val y = (row * Sz) + border_w
        squares(row)(col).draw(g, x, y)
        squares(row)(col).updatePosition(new Point(x + Sz/2, y + Sz/2))
    }
    draggedPiece.foreach { piece =>
      val image = piece.getImage
      g.drawImage(image, draggedPieceX - sizeUnit2 / 2, draggedPieceY - sizeUnit2 / 2, sizeUnit2, sizeUnit2, NoOpImageObserver)
    }
    
    // Don't draw colored borders - only draw text labels on transparent borders
    g.setColor(Color.WHITE)
    g.setFont(g.getFont.deriveFont(14f))

    for (col <- 0 until 8) {
      val char =
        if flipped then
          ('H' - col).toChar.toString
        else ('A' + col).toChar.toString
      val textX = col * Sz + border_w + Sz / 2 - g.getFontMetrics.stringWidth(char) / 2
      val textYTop = border_w / 2 + g.getFontMetrics.getAscent / 2
      val textYBottom = boardSize + border_w + border_w / 2 + g.getFontMetrics.getAscent / 2
      g.drawString(char, textX, textYTop)    // Top border
      g.drawString(char, textX, textYBottom) // Bottom border
    }
    for (row <- 0 until 8) {
      val char =
        if flipped then
          (row + 1).toString
        else (8 - row).toString
      val textXLeft = border_w / 2 - g.getFontMetrics.stringWidth(char) / 2
      val textXRight = boardSize + border_w + border_w / 2 - g.getFontMetrics.stringWidth(char) / 2
      val textY = row * Sz + border_w + Sz / 2 + g.getFontMetrics.getAscent / 2
      g.drawString(char, textXLeft, textY)
      g.drawString(char, textXRight, textY)}
    
      g.setColor(new Color(255, 0, 140)) // Pinkki korostus väri
      g.setStroke(new BasicStroke(3))
      hangingPieces(this).foreach {square =>
        val x = square.position.x - Sz / 2
        val y = square.position.y - Sz / 2
        g.drawRect(x + 2, y + 2, Sz - 4, Sz - 4)
      }
  end paintComponent

  def resetBoard(): Unit =
    for (row <- 0 until 8; col <- 0 until 8) {
      val square = squares(row)(col)
      square.occupiedBy = None
      square.deselect()
    }
    initializePieces()
    generateAttackSquaresBuffer(gameController.currentPlayer)
    repaint()
  end resetBoard

  def isWithinBoard(col: Int, row: Int): Boolean = col >= 0 && col < 8 && row >= 0 && row < 8

  def getSquareAt(x: Int, y: Int): Option[Square] =
    val col = (x - border_w)/ Sz
    val row = (y - border_w)/ Sz
    val (finalRow, finalCol) =
      if (!flipped) then (row, col)
      else (7 - row, 7 - col)

    if (isWithinBoard(finalCol, finalRow))
      Some(squares(finalRow)(finalCol))
    else None
  end getSquareAt
  
  def getSquareByName(name: String): Option[Square] =
    if (name.length != 2) return None
    val col = name.charAt(0) - 'a'
    val row = 8 - (name.charAt(1) - '0')
    if (isWithinBoard(col, row))
      Some(squares(row)(col))
    else None
  end getSquareByName

  def isKingInCheck(color: String): (Boolean, Square) =
    val kingSquare = squares.flatten.find(sq => sq.occupiedBy.exists(piece => piece.isInstanceOf[chess.pieces.King] && piece.color == color))
    kingSquare match {
      case Some(kingSq) =>
        val opponentColor = if (color == "white") "black" else "white"
        val opponentAttackSquares = generateAttackSquaresBuffer(opponentColor)
        (opponentAttackSquares.contains(kingSq), kingSq)
      case None => (false, squares.flatten.head)
    }
  end isKingInCheck

  def clearAllHighlights(): Unit =
    squares.flatten.foreach(_.clearHighlight())

  def hasValidMoves(color: String): Boolean =
    squares.flatten.exists { square =>
      square.occupiedBy.exists { piece =>
        piece.color == color && gameController.filterMovesToPreventCheck(piece, piece.possibleMoves()).nonEmpty } }
  end hasValidMoves
}
  class Border(x: Int, y: Int, width: Int, height: Int){
    var x_public = x
    var y_public = y
    var width_public = width
    var height_public = height}