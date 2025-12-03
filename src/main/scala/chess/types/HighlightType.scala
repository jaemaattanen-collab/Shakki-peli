// src/main/scala/chess/types/HighlightType.scala

package chess.types

/**
 * Enumeration of highlight types for squares.
 */
sealed trait HighlightType

object HighlightType {
  println("Minua korostustyökalua on käytetty!")
  case object Move1 extends HighlightType
  case object Capture extends HighlightType
  case object Weakness extends HighlightType
}
