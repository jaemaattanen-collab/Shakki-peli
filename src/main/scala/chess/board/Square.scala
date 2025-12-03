package chess.board
import chess.pieces.Piece
import chess.types.HighlightType._
import chess.types.HighlightType
import chess.utils.NoOpImageObserver
import java.awt.{Color, Graphics2D, Point, BasicStroke}
import java.awt.image.BufferedImage

class Square(val name: String, var position: Point, val board: Board, val image: BufferedImage, var isOccupied: Boolean = false) {
  var occupiedBy: Option[Piece] = None
  var seenByBlack: Int = 0
  var seenByWhite: Int = 0
  private var _isSelected: Boolean = false
  private var _highlightType: Option[HighlightType] = None
  
  def isSelected: Boolean = _isSelected
  
  def highlightType: Option[HighlightType] = 
    _highlightType

  def resetColor(): Unit =
    _isSelected = false
    _highlightType = None
  end resetColor

  def updatePosition(newPosition: Point): Unit =
    val (x, y) = if (board.flipped) then
      val flippedX = board.boardSize - newPosition.x
      val flippedY = board.boardSize - newPosition.y
      (flippedX, flippedY)
    else
      (newPosition.x, newPosition.y)
    this.position.setLocation(new Point(x, y))
  end updatePosition

  def clearHighlight(): Unit =
    this._highlightType = None


  def highlight(highlightType: HighlightType): Unit =
    _highlightType = Some(highlightType)

  def select(): Unit = _isSelected = true

  def deselect(): Unit =
    _isSelected = false
    _highlightType = None
  end deselect

  def getCoordinates: (Int, Int) = (position.x / board.Sz, position.y / board.Sz)

  def drawWeakness(): Unit =
    _highlightType = Some(Weakness)


  def draw(g: Graphics2D, x: Int, y: Int): Unit = 
    g.drawImage(image, x, y, board.Sz, board.Sz, NoOpImageObserver)
    _highlightType.foreach {
      case Move1 =>
        g.setColor(new Color(0, 255, 0)) // Vihreä rajaus siirrolle
        g.setStroke(new BasicStroke(3))
        g.drawRect(x + 2, y + 2, board.Sz - 4, board.Sz - 4)
      case Capture =>
        g.setColor(new Color(0, 0, 255)) // Sininen rajaus capturelle
        g.setStroke(new BasicStroke(3))
        g.drawRect(x + 2, y + 2, board.Sz - 4, board.Sz - 4)
      case Weakness =>
        g.setColor(new Color(255, 0, 140))
        g.setStroke(new BasicStroke(3))
        g.drawRect(x + 2, y + 2, board.Sz - 4, board.Sz - 4)}

    // Piirrä valinta-rajaus jos valittu
    if (_isSelected) then
      g.setColor(new Color(255, 0, 0)) // Punainen valinta
      g.setStroke(new BasicStroke(4))   // Paksumpi rajaus
      g.drawRect(x, y, board.Sz, board.Sz)

    // Piirrä nappulan kuva, jos ruudussa on nappula
    occupiedBy.foreach { piece =>
      val image = piece.getImage
      val centerX = x + board.Sz / 2
      val centerY = y + board.Sz / 2
      val imageX = centerX - (board.sizeUnit2 / 2)
      val imageY = centerY - (board.sizeUnit2 / 2)

      g.drawImage(image, imageX, imageY, board.sizeUnit2, board.sizeUnit2, NoOpImageObserver) }
  end draw
}
