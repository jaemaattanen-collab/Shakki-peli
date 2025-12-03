package chess.ui// Legacy Swing dialog helpers removed; file intentionally left blank.


import scalafx.scene.control.Alert
import scalafx.scene.control.Alert.AlertType
import scalafx.stage.Window

/**
 * Small helper for presenting modal dialogs from anywhere in the UI layer.
 */
final class DialogManager {
  private var owner: Option[Window] = None

  def setOwner(window: Window): Unit =
    owner = Option(window)

  def showAlert(alertType: AlertType, title: String, message: String): Unit =
    val alert = new Alert(alertType)
    owner.foreach(alert.initOwner)
    alert.headerText = None
    alert.title = title
    alert.contentText = message
    alert.showAndWait()
}
