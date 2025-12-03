package chess.utils

import java.util.concurrent.ConcurrentHashMap

/**
 * Simple helper to emit a debug message only once per application lifecycle.
 * Each unique numeric id is recorded after the first invocation to prevent
 * repeated logging when a function fires many times.
 */
object DebugOnce {
  private val seenIds = ConcurrentHashMap.newKeySet[Int]()

  /**
   * Emits the provided message the first time the given id is marked.
   * Subsequent calls with the same id are ignored.
   */
  def mark(id: Int, message: => String): Unit = {
    if (seenIds.add(id)) {
      println(message)
    }
  }

  /**
   * Resets the internal cache so future calls will log again.
   * Useful in tests.
   */
  def reset(): Unit = {
    seenIds.clear()
  }
}
