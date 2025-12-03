// src/main/scala/chess/utils/ImageManager.scala
package chess.utils
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.awt.Image
import java.io.IOException
import java.awt.RenderingHints
import java.awt.geom.AffineTransform

/**
 * Utility object to manage loading and caching of images.
 */
object ImageManager {
  private val imageCache = scala.collection.mutable.Map[String, Image]()

  /**
   * Retrieves an image by its filename. Caches the image after the first load.
   *
   * @param imageName The filename of the image (e.g., "white-pawn.png").
   * @return The loaded Image.
   */
  def getImage(imageName: String): Image = {
    imageCache.getOrElseUpdate(imageName, loadImage(imageName))
  }

  /**
   * Loads an image from the recources directory.
   *
   * @param imageName The filename of the image.
   * @return The loaded Image.
   */
  private def loadImage(imageName: String): Image = {
    try {
      // Try to load from classpath first
      val stream = getClass.getResourceAsStream(s"/$imageName")
      if (stream != null) {
        val img = ImageIO.read(stream)
        stream.close()
        img
      } else {
        // Fallback: try to load from file system (for development)
        val file = new java.io.File(s"src/main/resources/$imageName")
        if (file.exists()) {
          ImageIO.read(file)
        } else {
          throw new IOException(s"Resource not found: $imageName")
        }
      }
    } catch {
      case e: IOException =>
        println(s"Error loading image: $imageName")
        throw e
    }
  }

  def rotateImage(image: BufferedImage, angle: Int): BufferedImage = {
    val radians = Math.toRadians(angle)
    val transform = new AffineTransform()
    transform.rotate(radians, image.getWidth / 2.0, image.getHeight / 2.0)

    val rotatedImage = new BufferedImage(image.getWidth, image.getHeight, image.getType)
    val g2d = rotatedImage.createGraphics()

    // Set rendering hints for quality
    g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
    g2d.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    g2d.drawImage(image, transform, null)
    g2d.dispose()

    rotatedImage
}
}

