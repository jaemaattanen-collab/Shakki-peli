package chess.ui

import chess.pgn.PGNManager
import javafx.stage.Stage
import scalafx.stage.FileChooser
import java.io.File

/**
 * Handles file operations for the chess application:
 * - PGN save/load
 * - Analysis snapshot save/load
 */
class FileOperationsHandler(
  pgnManager: PGNManager,
  analysisManager: AnalysisManager,
  notificationCenter: NotificationCenter,
  stage: () => Stage
) {

  /**
   * Save current game as PGN file.
   */
  def saveGameAsPGN(): Unit = {
    val chooser = new FileChooser {
      title = "Save Game as PGN"
      initialFileName = s"game-${System.currentTimeMillis()}.pgn"
      extensionFilters.add(new FileChooser.ExtensionFilter("PGN Files", "*.pgn"))
    }

    Option(chooser.showSaveDialog(stage())).foreach { file =>
      ensureParentExists(file)
      pgnManager.savePGN(file) match
        case Right(_) =>
          notificationCenter.showInfo(
            key = "pgn-save",
            title = "Game saved",
            message = s"Saved PGN to ${file.getName}."
          )
        case Left(error) =>
          notificationCenter.showError(
            key = "pgn-save",
            title = "Failed to save PGN",
            message = error
          )
    }
  }

  /**
   * Load game from PGN file.
   * @return Some callback data if load was successful (for UI refresh)
   */
  def loadGameFromPGN(onSuccess: () => Unit): Unit = {
    val chooser = new FileChooser {
      title = "Load PGN"
      extensionFilters.add(new FileChooser.ExtensionFilter("PGN Files", "*.pgn"))
    }

    Option(chooser.showOpenDialog(stage())).foreach { file =>
      pgnManager.loadPGN(file) match
        case Right(_) =>
          onSuccess()
          notificationCenter.showInfo(
            key = "pgn-load",
            title = "PGN loaded",
            message = s"Loaded ${file.getName}."
          )
        case Left(error) =>
          notificationCenter.showError(
            key = "pgn-load",
            title = "Failed to load PGN",
            message = error
          )
    }
  }

  /**
   * Load analysis snapshot from file.
   */
  def loadAnalysisSnapshot(): Unit = {
    val chooser = new FileChooser {
      title = "Load Analysis Snapshot"
      extensionFilters.add(
        new FileChooser.ExtensionFilter("Analysis Snapshots", Seq("*.txt", "*.log", "*.analysis"))
      )
    }
    Option(chooser.showOpenDialog(stage())).foreach { file =>
      analysisManager.loadSnapshotFromFile(file)
      notificationCenter.showInfo(
        key = "analysis-status",
        title = "Analysis loaded",
        message = s"Loaded snapshot from ${file.getName}."
      )
    }
  }

  /**
   * Save analysis snapshot to file.
   */
  def saveAnalysisSnapshot(): Unit = {
    val chooser = new FileChooser {
      title = "Save Analysis Snapshot"
      initialFileName = s"analysis-${System.currentTimeMillis()}.txt"
      extensionFilters.add(new FileChooser.ExtensionFilter("Analysis Snapshots", "*.txt"))
    }
    Option(chooser.showSaveDialog(stage())).foreach { file =>
      ensureParentExists(file)
      analysisManager.saveSnapshotToFile(file)
    }
  }

  private def ensureParentExists(file: File): Unit = {
    Option(file.getParentFile).filterNot(_.exists()).foreach(_.mkdirs())
  }
}
