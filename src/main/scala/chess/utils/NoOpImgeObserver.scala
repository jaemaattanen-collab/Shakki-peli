// src/main/scala/chess/utils/NoOpImageObserver.scala

package chess.utils

import java.awt.image.ImageObserver
import java.awt.Image

/**
 * A no-operation ImageObserver that does nothing.
 */
object NoOpImageObserver extends ImageObserver {
  override def imageUpdate(img: Image, infoflags: Int, x: Int, y: Int, width: Int, height: Int): Boolean = 
    false
}
