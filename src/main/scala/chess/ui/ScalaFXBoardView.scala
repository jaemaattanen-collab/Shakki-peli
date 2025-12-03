package chess.ui

import chess.board.Board

import scalafx.scene.layout.{GridPane, StackPane, Pane}
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Rectangle, Line, Polygon, StrokeLineCap}
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.text.Text
import scalafx.scene.input.{MouseEvent, DragEvent, ClipboardContent}
import scalafx.geometry.Pos
import scalafx.Includes._
import scalafx.animation.TranslateTransition
import scalafx.util.Duration

object ScalaFXBoardView {
  final case class MoveArrow(
    fromRow: Int,
    fromCol: Int,
    toRow: Int,
    toCol: Int,
    color: Color,
    opacity: Double = 0.85,
    width: Double = 4.0
  )
}

/**
 * ScalaFX view that renders the existing Swing-based Board model.
 * For now this is a read-only visualisation: it draws pieces based on the Board state
 * but does not yet handle click-to-move.
 */
class ScalaFXBoardView extends StackPane {

  import ScalaFXBoardView.MoveArrow

  // The actual grid for the chess board
  private val boardGrid = new GridPane {
    alignment = Pos.Center
    hgap = 1
    vgap = 1
    focusTraversable = false
  }
  
  // Overlay pane for animations (on top of the grid)
  private val animationLayer = new Pane {
    mouseTransparent = true
    pickOnBounds = false
  }

  // Promotion menu overlay
  private val promotionOverlay = new Pane {
    mouseTransparent = false
    pickOnBounds = false
    visible = false
  }
  
  // Stack the grid, animation layer, and promotion overlay
  children = Seq(boardGrid, animationLayer, promotionOverlay)
  alignment = Pos.Center
  focusTraversable = false  // Disable focus to prevent layout "pop" on first click

  // Track the last known cell size so we can rebuild when layout changes
  private var lastCellSize: Double = 0.0

  /** Current visual cell size in pixels (one square edge). */
  def cellSize: Double = lastCellSize

  // Board flip state
  private var isFlipped: Boolean = false

  private var modelBoard: Option[Board] = None
  // Optional callback invoked when a square is clicked: (row, col)
  var onSquareClick: Option[(Int, Int) => Unit] = None
  // Optional callback for drag-and-drop moves: (fromRow, fromCol, toRow, toCol)
  var onDragMove: Option[(Int, Int, Int, Int) => Unit] = None
  // Optional callback to get legal move targets for a piece at (row, col)
  var onGetLegalMoves: Option[(Int, Int) => Seq[(Int, Int)]] = None

  // Track drag state for visual feedback
  private var dragSourceCoord: Option[(Int, Int)] = None
  
  // Animation state
  private var isAnimating: Boolean = false
  private var animatingToSquare: Option[(Int, Int)] = None  // Hide piece at this square during animation
  
  // Promotion menu state
  private var promotionCallback: Option[String => Unit] = None
  private var promotionColor: String = "white"
  private var promotionSquare: Option[(Int, Int)] = None

   // Highlighting helpers (selection and last move). We track them client-side
   // using row/col indices because the underlying Swing Board already knows
   // how to manage its own highlights for the Swing view.
  private var selectedSquareCoord: Option[(Int, Int)] = None
  private var lastMoveFromCoord: Option[(Int, Int)] = None
  private var lastMoveToCoord: Option[(Int, Int)] = None
  private var legalMoveTargets: Set[(Int, Int)] = Set.empty
  private var moveHints: Seq[MoveArrow] = Seq.empty
  
  // Move classification highlight for destination square
  private var moveClassificationHighlight: Option[(Int, Int, String)] = None  // (row, col, symbol)

   /**
    * Set the currently selected square for visual highlighting.
    */
   def setSelectedSquare(row: Int, col: Int): Unit = {
     selectedSquareCoord = Some((row, col))
     refreshFromModel()
   }

   def clearSelectedSquare(): Unit = {
     selectedSquareCoord = None
     refreshFromModel()
   }

   /**
    * Set last move highlight (from -> to squares).
    */
   def setLastMove(fromRow: Int, fromCol: Int, toRow: Int, toCol: Int): Unit = {
     lastMoveFromCoord = Some((fromRow, fromCol))
     lastMoveToCoord = Some((toRow, toCol))
     refreshFromModel()
   }
   
   /**
    * Set last move coordinates without refreshing (for use with animation).
    */
   def setLastMoveCoords(fromRow: Int, fromCol: Int, toRow: Int, toCol: Int): Unit = {
     lastMoveFromCoord = Some((fromRow, fromCol))
     lastMoveToCoord = Some((toRow, toCol))
   }
   
   /**
    * Animate a piece moving from one square to another, then refresh the board.
    * Duration is approximately 250ms.
    * This version looks up the piece from the model - use before making the move.
    */
   def animateMove(fromRow: Int, fromCol: Int, toRow: Int, toCol: Int, onComplete: () => Unit = () => ()): Unit = {
     if (isAnimating || lastCellSize <= 0) {
       // Skip animation if already animating or board not yet sized
       onComplete()
       return
     }
     
     modelBoard.foreach { b =>
       // Get the piece at the source square (before the move is made in the model)
       val sourceSquare = b.squares(fromRow)(fromCol)
       sourceSquare.occupiedBy match {
         case Some(piece) =>
           // Load the piece image
           val pieceName = piece.pieceType.toString.toLowerCase
           animatePieceMove(fromRow, fromCol, toRow, toCol, piece.color, pieceName, onComplete)
         case None =>
           // No piece at source, just complete
           onComplete()
       }
     }
   }
   
   /**
    * Animate a piece moving from one square to another with explicit piece info.
    * Use this when the piece is already moved in the model and you have the piece info.
    * Duration is approximately 250ms.
    */
   def animatePieceMove(fromRow: Int, fromCol: Int, toRow: Int, toCol: Int, 
                        pieceColor: String, pieceType: String, onComplete: () => Unit = () => ()): Unit = {
     if (isAnimating || lastCellSize <= 0) {
       // Skip animation if already animating or board not yet sized
       refreshFromModel()
       onComplete()
       return
     }
     
     loadPieceImage(pieceColor, pieceType) match {
       case Some(img) =>
         isAnimating = true
         // Hide the piece at the destination during animation
         animatingToSquare = Some((toRow, toCol))
         
         // Calculate positions BEFORE refresh (using current cellSize)
         val sz = lastCellSize
         val (fromVisRow, fromVisCol) = modelToVisual(fromRow, fromCol)
         val (toVisRow, toVisCol) = modelToVisual(toRow, toCol)
         
         // GridPane uses gaps between cells
         val gapX = boardGrid.hgap.value
         val gapY = boardGrid.vgap.value
         val startX = fromVisCol * (sz + gapX)
         val startY = fromVisRow * (sz + gapY)
         val endX = toVisCol * (sz + gapX)
         val endY = toVisRow * (sz + gapY)
         
         // Create animated piece overlay at the START position
         val pieceSize = sz * 0.8
         val offset = (sz - pieceSize) / 2
         
         val animatedPiece = new ImageView(img) {
           fitWidth = pieceSize
           fitHeight = pieceSize
           preserveRatio = true
           mouseTransparent = true
           // Absolute position within the animation layer
           layoutX = startX + offset
           layoutY = startY + offset
         }
         
         // Refresh the board to show current state (with destination hidden)
         refreshFromModel()
         
         // Add animated piece to the animation layer (which is on top of the grid)
         animationLayer.children.add(animatedPiece)
         
         println(s"Animation started: from ($fromVisRow,$fromVisCol) to ($toVisRow,$toVisCol)")
         println(s"  startX=$startX, startY=$startY, endX=$endX, endY=$endY")
         println(s"  cellSize=$sz, pieceSize=$pieceSize")
         
         // Create animation
         val animation = new TranslateTransition {
           duration = Duration(125)  // 125ms animation (fast and smooth)
           node = animatedPiece
           toX = endX - startX
           toY = endY - startY
           onFinished = _ => {
             println("Animation finished")
             animationLayer.children.remove(animatedPiece)
             animatingToSquare = None
             isAnimating = false
             refreshFromModel()
             onComplete()
           }
         }
         
         animation.play()
         
       case None =>
         refreshFromModel()
         onComplete()
     }
   }

   def clearLastMove(): Unit = {
     lastMoveFromCoord = None
     lastMoveToCoord = None
     refreshFromModel()
   }

   /** Highlight all legal destination squares for the currently selected piece. */
   def setLegalMoveTargets(coords: Iterable[(Int, Int)]): Unit = {
     legalMoveTargets = coords.toSet
     refreshFromModel()
   }

   def clearLegalMoveTargets(): Unit = {
     if legalMoveTargets.nonEmpty then
       legalMoveTargets = Set.empty
       refreshFromModel()
   }

  def setMoveHints(hints: Seq[MoveArrow]): Unit = {
    moveHints = hints
    refreshFromModel()
  }
  
  /** Set move classification highlight on destination square with quality badge */
  def setMoveClassificationHighlight(row: Int, col: Int, symbol: String): Unit = {
    moveClassificationHighlight = Some((row, col, symbol))
    refreshFromModel()
  }
  
  def clearMoveClassificationHighlight(): Unit = {
    moveClassificationHighlight = None
    refreshFromModel()
  }
  
  /** Get the color for a move classification symbol */
  private def getClassificationColor(symbol: String): Color = symbol match {
    case "!!" => Color.web("#1baca6")  // Brilliant - teal
    case "!" => Color.web("#5c8bb0")   // Great - blue
    case "★" => Color.web("#98bc4b")   // Best - green
    case "○" => Color.web("#98bc4b")   // Excellent - same green as Best
    case "✓" => Color.web("#98bc4b")   // Good - same green as Best
    case "?!" => Color.web("#f7c631")  // Inaccuracy - yellow
    case "?" => Color.web("#e68a2e")   // Mistake - orange
    case "??" => Color.web("#ca3431")  // Blunder - red
    case "📖" => Color.web("#a88b5e")  // Book - brown
    case _ => Color.web("#808080")      // Default gray
  }

  /** Set board flip state (view from black's perspective) */
  def setFlipped(flipped: Boolean): Unit = {
    isFlipped = flipped
    refreshFromModel()
  }

  /** Check if board is currently flipped */
  def getFlipped: Boolean = isFlipped

  /** Convert visual grid coordinates to model coordinates based on flip state */
  private def visualToModel(visualRow: Int, visualCol: Int): (Int, Int) = {
    if isFlipped then (7 - visualRow, 7 - visualCol)
    else (visualRow, visualCol)
  }

  /** Convert model coordinates to visual grid coordinates based on flip state */
  private def modelToVisual(modelRow: Int, modelCol: Int): (Int, Int) = {
    if isFlipped then (7 - modelRow, 7 - modelCol)
    else (modelRow, modelCol)
  }

  // --- Piece image loading ---
  // Cache images by a simple key like "white-pawn", "black-queen", etc.
  private val pieceImageCache = scala.collection.mutable.Map[String, Image]()

  private def loadPieceImage(color: String, pieceName: String): Option[Image] = {
    val key = s"${color.toLowerCase}-${pieceName.toLowerCase}"
    pieceImageCache.get(key).orElse {
      val resourcePath = s"/${key}.png"
      val url = getClass.getResource(resourcePath)
      if (url != null) {
        val img = new Image(url.toExternalForm)
        pieceImageCache.update(key, img)
        Some(img)
      } else {
        None
      }
    }
  }

  /** Set the underlying Board model (from chess.board.Board) */
  def setModel(board: Board): Unit = {
    modelBoard = Some(board)
    refreshFromModel()
  }

  // Whenever this view gets resized, recompute cell size based on height
  // so that the 8x8 board fills most of the available vertical space.
  height.onChange { (_, _, _) =>
    refreshFromModel()
  }

  /**
   * Rebuild the JavaFX nodes from the current Board state.
   * This does not modify the model, only reflects its current squares and pieces.
   */
  def refreshFromModel(): Unit = {
    boardGrid.children.clear()
    modelBoard.foreach { b =>
      val squares = b.squares

      // Compute cell size from current height so board uses most vertical space.
      // Subtract a small margin for gaps.
      val availableHeight = math.max(height.value, 0.0)
      val sz =
        if (availableHeight > 0) then
          // 8 rows plus 7 gaps of 1px each (approx). Use a small padding factor.
          (availableHeight - 16) / 8.0
        else
          80.0 // sensible default before first layout

      lastCellSize = sz

      for {
        visualRow <- 0 until 8
        visualCol <- 0 until 8
      } {
        // Convert visual position to model position based on flip state
        val (modelRow, modelCol) = visualToModel(visualRow, visualCol)
        val square = squares(modelRow)(modelCol)
        val isLight = (modelRow + modelCol) % 2 == 0

        // Base square color
        var baseColor = if (isLight) Color.web("#f0d9b5") else Color.web("#b58863")

        // Apply simple overlay for selection / last-move highlighting
        // These coordinates are stored in model space, so compare with model coords
        val modelCoords = (modelRow, modelCol)
        val isSelected = selectedSquareCoord.contains(modelCoords)
        val isLegalTarget = legalMoveTargets.contains(modelCoords)
        val isLastFrom = lastMoveFromCoord.contains(modelCoords)
        val isLastTo = lastMoveToCoord.contains(modelCoords)

        // Check for move classification highlight (destination square)
        val classificationInfo = moveClassificationHighlight.filter { case (r, c, _) => r == modelRow && c == modelCol }
        
        if (isSelected) {
          baseColor = Color.web("#f6f669") // yellow-ish for selection
        } else if (isLegalTarget) {
          baseColor = Color.web("#6bd4a7") // teal for available moves
        } else if (classificationInfo.isDefined) {
          // Use classification color for destination square
          val symbol = classificationInfo.get._3
          val classColor = getClassificationColor(symbol)
          // Blend classification color with base square color
          val blendFactor = 0.5
          baseColor = Color.color(
            baseColor.red * (1 - blendFactor) + classColor.red * blendFactor,
            baseColor.green * (1 - blendFactor) + classColor.green * blendFactor,
            baseColor.blue * (1 - blendFactor) + classColor.blue * blendFactor
          )
        } else if (isLastFrom || isLastTo) {
          baseColor = Color.web("#add8e6") // light blue for last move
        }

        val rect = new Rectangle {
          width = sz
          height = sz
          fill = baseColor
        }

        // Check if this square should hide its piece during animation
        val hideForAnimation = animatingToSquare.contains((modelRow, modelCol))

        // Prefer PNG piece sprites from resources; fall back to simple letter if missing
        val pieceNode: Option[ImageView] = if hideForAnimation then None else square.occupiedBy.flatMap { p =>
          val name = p.pieceType.toString.toLowerCase // e.g. "pawn", "queen"
          val maybeImg = loadPieceImage(p.color, name)
          maybeImg.map { img =>
            new ImageView(img) {
              fitWidth = sz * 0.8
              fitHeight = sz * 0.8
              preserveRatio = true
            }
          }
        }
        
        // Fallback text node if no image
        val fallbackNode = if pieceNode.isEmpty && !hideForAnimation then
          square.occupiedBy.map { p =>
            val symbol = p.pieceType.toString.headOption.getOrElse('?')
            new Text(symbol.toString) {
              fill = if (p.color == "white") Color.White else Color.Black
            }
          }
        else None

        // Classification badge for destination square
        val classificationBadge: Option[StackPane] = classificationInfo.flatMap { case (_, _, symbol) =>
          if symbol.nonEmpty then
            val badgeColor = getClassificationColor(symbol)
            val badgeSize = sz * 0.28
            Some(new StackPane {
              // Position in top-right corner
              translateX = sz * 0.32
              translateY = -sz * 0.32
              children = Seq(
                new Rectangle {
                  width = badgeSize
                  height = badgeSize
                  arcWidth = badgeSize * 0.3
                  arcHeight = badgeSize * 0.3
                  fill = badgeColor
                  stroke = Color.web("#000000")
                  strokeWidth = 1
                },
                new Text(symbol) {
                  fill = Color.White
                  style = s"-fx-font-size: ${badgeSize * 0.6}px; -fx-font-weight: bold;"
                }
              )
            })
          else None
        }

        val cell = new StackPane {
          prefWidth = sz
          prefHeight = sz
          children = Seq(rect) ++ pieceNode.toSeq ++ fallbackNode.toSeq ++ classificationBadge.toSeq
          
          // Click callback uses model coordinates
          onMouseClicked = _ => onSquareClick.foreach(cb => cb(modelRow, modelCol))
          
          // Drag and Drop support
          onDragDetected = (event: MouseEvent) => {
            // Only start drag if there's a piece on this square
            if square.occupiedBy.isDefined then
              val dragboard = delegate.startDragAndDrop(javafx.scene.input.TransferMode.MOVE)
              val content = new ClipboardContent()
              content.putString(s"$modelRow,$modelCol")
              dragboard.setContent(content.delegate)
              
              // Use the original piece image at the displayed size
              pieceNode.foreach { imgView =>
                val displayWidth = imgView.fitWidth.value
                val displayHeight = imgView.fitHeight.value
                
                // Use the actual rendered image from ImageView with proper scaling
                val originalImage = imgView.image.value
                val imageView = new javafx.scene.image.ImageView(originalImage)
                imageView.setFitWidth(displayWidth)
                imageView.setFitHeight(displayHeight)
                imageView.setPreserveRatio(true)
                imageView.setSmooth(true)
                
                val params = new javafx.scene.SnapshotParameters()
                params.setFill(javafx.scene.paint.Color.TRANSPARENT)
                val snapshot = imageView.snapshot(params, null)
                
                dragboard.setDragView(snapshot, displayWidth / 2, displayHeight / 2)
                // Hide original piece immediately
                imgView.visible = false
              }
              
              // Show legal move targets
              onGetLegalMoves.foreach { getLegalMoves =>
                val targets = getLegalMoves(modelRow, modelCol)
                setLegalMoveTargets(targets)
              }
              
              dragSourceCoord = Some((modelRow, modelCol))
              event.consume()
          }
          
          onDragOver = (event: DragEvent) => {
            // Accept drops if we have valid drag content
            if event.dragboard.hasString then
              event.acceptTransferModes(javafx.scene.input.TransferMode.MOVE)
            event.consume()
          }
          
          onDragDropped = (event: DragEvent) => {
            var success = false
            if event.dragboard.hasString then
              val sourceStr = event.dragboard.getString
              val parts = sourceStr.split(",")
              if parts.length == 2 then
                val fromRow = parts(0).toIntOption.getOrElse(-1)
                val fromCol = parts(1).toIntOption.getOrElse(-1)
                if fromRow >= 0 && fromCol >= 0 then
                  // Trigger move callback
                  onDragMove.foreach(cb => cb(fromRow, fromCol, modelRow, modelCol))
                  success = true
            event.setDropCompleted(success)
            event.consume()
          }
          
          onDragDone = (event: DragEvent) => {
            // Show piece again if drag was cancelled (no move made)
            pieceNode.foreach(_.visible = true)
            // Clear legal move highlights
            clearLegalMoveTargets()
            dragSourceCoord = None
            event.consume()
          }
        }

        // Add cell at visual position
        boardGrid.add(cell, visualCol, visualRow)
      }

      buildArrowLayer(sz).foreach { layer =>
        boardGrid.add(layer, 0, 0, 8, 8)
      }
    }
  }

  private def buildArrowLayer(cellSize: Double): Option[Pane] = {
    if moveHints.isEmpty then return None

    val gapX = boardGrid.hgap.value
    val gapY = boardGrid.vgap.value
    val layer = new Pane {
      pickOnBounds = false
      mouseTransparent = true
      prefWidth = (cellSize * 8) + (gapX * 7)
      prefHeight = (cellSize * 8) + (gapY * 7)
    }

    moveHints.foreach { hint =>
      if hint.fromRow != hint.toRow || hint.fromCol != hint.toCol then
        // Convert model coordinates to visual coordinates for arrow drawing
        val (visualFromRow, visualFromCol) = modelToVisual(hint.fromRow, hint.fromCol)
        val (visualToRow, visualToCol) = modelToVisual(hint.toRow, hint.toCol)
        
        val sx = visualFromCol * (cellSize + gapX) + (cellSize / 2.0)
        val sy = visualFromRow * (cellSize + gapY) + (cellSize / 2.0)
        val ex = visualToCol * (cellSize + gapX) + (cellSize / 2.0)
        val ey = visualToRow * (cellSize + gapY) + (cellSize / 2.0)

        val dx = ex - sx
        val dy = ey - sy
        val length = math.sqrt((dx * dx) + (dy * dy))
        if length > 0.0001 then
          val line = new Line {
            startX = sx
            startY = sy
            endX = ex
            endY = ey
            stroke = hint.color
            strokeWidth = hint.width
            opacity = hint.opacity
            strokeLineCap = StrokeLineCap.Round
          }

          val headLength = math.max(cellSize * 0.35, 12.0)
          val headWidth = headLength * 0.6
          val normX = dx / length
          val normY = dy / length
          val baseX = ex - normX * headLength
          val baseY = ey - normY * headLength
          val leftX = baseX + (-normY) * headWidth
          val leftY = baseY + normX * headWidth
          val rightX = baseX - (-normY) * headWidth
          val rightY = baseY - normX * headWidth

          val head = new Polygon {
            points ++= Seq(ex, ey, leftX, leftY, rightX, rightY)
            fill = hint.color
            opacity = hint.opacity
          }

          layer.children.addAll(line, head)
    }

    Some(layer)
  }

  // -------------------------------------------------------------------------
  // Promotion menu (visual dropdown on the board)
  // -------------------------------------------------------------------------

  /**
   * Show the promotion menu at the given square.
   * @param row The row of the promotion square
   * @param col The column of the promotion square
   * @param color "white" or "black" - determines which piece images to show
   * @param onSelect Callback when a piece is selected (receives "queen", "rook", "bishop", or "knight")
   */
  def showPromotionMenu(row: Int, col: Int, color: String, onSelect: String => Unit): Unit = {
    promotionCallback = Some(onSelect)
    promotionColor = color
    promotionSquare = Some((row, col))
    
    buildPromotionMenu(row, col, color)
    promotionOverlay.visible = true
  }

  /** Hide the promotion menu */
  def hidePromotionMenu(): Unit = {
    promotionOverlay.visible = false
    promotionOverlay.children.clear()
    promotionCallback = None
    promotionSquare = None
  }

  /** Check if promotion menu is currently visible */
  def isPromotionMenuVisible: Boolean = promotionOverlay.visible.value

  private def buildPromotionMenu(row: Int, col: Int, color: String): Unit = {
    promotionOverlay.children.clear()
    
    val pieces = Seq("queen", "rook", "bishop", "knight")
    val cellSz = lastCellSize
    
    // Calculate position - menu should open towards the center of the board
    // For white promoting: normally row 0, menu goes down (towards row 7)
    // For black promoting: normally row 7, menu goes up (towards row 0)
    // When flipped, the visual positions are reversed
    val isWhitePromoting = color == "white"
    
    // Calculate visual column and row (accounting for flip)
    val visualCol = if (isFlipped) 7 - col else col
    val visualRow = if (isFlipped) 7 - row else row
    
    // Determine if menu should go down (true) or up (false)
    // Menu should go towards the center of the board
    // If visual row is 0 (top), go down; if visual row is 7 (bottom), go up
    val menuGoesDown = visualRow == 0
    
    // X position based on column
    val xPos = visualCol * cellSz
    
    // Y position: if going down, start at the promotion square; if going up, start 3 cells above
    val yPos = if (menuGoesDown) visualRow * cellSz else (visualRow - 3) * cellSz
    
    // Background container for all pieces
    val menuContainer = new scalafx.scene.layout.VBox {
      layoutX = xPos
      layoutY = yPos
      style = "-fx-background-color: #f0f0f0; -fx-border-color: #888888; -fx-border-width: 1;"
    }
    
    // Order pieces: queen first when going down, knight first when going up
    val orderedPieces = if (menuGoesDown) pieces else pieces.reverse
    
    orderedPieces.foreach { pieceName =>
      val imgOpt = loadPieceImage(color, pieceName)
      
      val piecePane = new StackPane {
        prefWidth = cellSz
        prefHeight = cellSz
        style = "-fx-background-color: #f0f0f0; -fx-cursor: hand;"
        
        imgOpt.foreach { img =>
          val iv = new ImageView(img) {
            fitWidth = cellSz * 0.85
            fitHeight = cellSz * 0.85
            preserveRatio = true
          }
          children = Seq(iv)
        }
        
        // Hover effect
        onMouseEntered = _ => {
          style = "-fx-background-color: #d0d0ff; -fx-cursor: hand;"
        }
        onMouseExited = _ => {
          style = "-fx-background-color: #f0f0f0; -fx-cursor: hand;"
        }
        
        // Click handler
        onMouseClicked = _ => {
          promotionCallback.foreach(_(pieceName))
          hidePromotionMenu()
        }
      }
      
      menuContainer.children.add(piecePane)
    }
    
    // Add cancel overlay (clicking outside closes menu)
    val cancelOverlay = new Rectangle {
      width <== promotionOverlay.width
      height <== promotionOverlay.height
      fill = Color.Transparent
      onMouseClicked = _ => {
        // Default to queen if clicked outside
        promotionCallback.foreach(_("queen"))
        hidePromotionMenu()
      }
    }
    
    promotionOverlay.children.addAll(cancelOverlay, menuContainer)
  }
}
