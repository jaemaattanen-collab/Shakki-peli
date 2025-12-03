package chess.ui

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.{Button, Label}
import scalafx.scene.layout.{FlowPane, HBox, Region, VBox}
import scala.collection.mutable

/**
 * Lightweight in-app notifications surface that replaces modal pop-ups.
 * Allows informational, warning, and error messages along with optional
 * action buttons and custom content. Notifications are keyed so callers can
 * update or dismiss previously rendered messages.
 */
class NotificationCenter extends VBox {

  sealed trait NotificationLevel {
    def background: String
    def borderColor: String
    def accent: String
    def iconGlyph: String
  }

  case object InfoLevel extends NotificationLevel {
    val background = "#2e3a4d"
    val borderColor = "#3f5675"
    val accent = "#64b5f6"
    val iconGlyph = "ℹ"
  }

  case object WarningLevel extends NotificationLevel {
    val background = "#4a3f28"
    val borderColor = "#705b30"
    val accent = "#ffca28"
    val iconGlyph = "⚠"
  }

  case object ErrorLevel extends NotificationLevel {
    val background = "#4a2d2d"
    val borderColor = "#703838"
    val accent = "#ff5252"
    val iconGlyph = "✖"
  }

  final case class NotificationAction(
    label: String,
    onTrigger: () => Unit,
    closeOnTrigger: Boolean = true,
    styleClass: String = "primary"
  )

  private final case class NotificationPayload(
    level: NotificationLevel,
    title: String,
    body: Option[String],
    actions: Seq[NotificationAction],
    dismissible: Boolean,
    customContent: Option[VBox]
  )

  private val notifications = mutable.LinkedHashMap.empty[String, VBox]

  spacing = 10
  padding = Insets(10)
  style = "-fx-background-color: transparent;"
  visible = false
  managed = false

  private val header = new Label("Notifications") {
    style = "-fx-text-fill: #e0e0e0; -fx-font-size: 13px; -fx-font-weight: bold;"
  }

  private val container = new VBox {
    spacing = 8
  }

  children = Seq(header, container)

  private def ensureVisibility(): Unit = {
    val hasItems = container.children.nonEmpty
    visible = hasItems
    managed = hasItems
    header.visible = hasItems
    header.managed = hasItems
  }

  def clear(key: String): Unit = Platform.runLater {
    notifications.remove(key).foreach { node =>
      container.children.remove(node)
      ensureVisibility()
    }
  }

  def clearAll(): Unit = Platform.runLater {
    notifications.clear()
    container.children.clear()
    ensureVisibility()
  }

  def showInfo(key: String, title: String, message: String, actions: Seq[NotificationAction] = Nil, dismissible: Boolean = true): Unit =
    show(key, InfoLevel, title, Some(message), actions, dismissible, customContent = None)

  def showWarning(key: String, title: String, message: String, actions: Seq[NotificationAction] = Nil, dismissible: Boolean = true): Unit =
    show(key, WarningLevel, title, Some(message), actions, dismissible, customContent = None)

  def showError(key: String, title: String, message: String, actions: Seq[NotificationAction] = Nil, dismissible: Boolean = true): Unit =
    show(key, ErrorLevel, title, Some(message), actions, dismissible, customContent = None)

  def showCustom(
      key: String,
      level: NotificationLevel,
      title: String,
      body: Option[String],
      content: VBox,
      actions: Seq[NotificationAction] = Nil,
      dismissible: Boolean = true
  ): Unit =
    show(key, level, title, body, actions, dismissible, customContent = Some(content))

  private def show(
      key: String,
      level: NotificationLevel,
      title: String,
      body: Option[String],
      actions: Seq[NotificationAction],
      dismissible: Boolean,
      customContent: Option[VBox]
  ): Unit = Platform.runLater {
    val payload = NotificationPayload(level, title, body, actions, dismissible, customContent)
    val node = buildCard(key, payload)
    notifications.get(key) match
      case Some(existing) =>
        val index = container.children.indexOf(existing)
        if index >= 0 then container.children.update(index, node) else container.children.add(node)
        notifications.update(key, node)
      case None =>
        container.children.add(0, node)
        notifications.update(key, node)
    ensureVisibility()
  }

  private def buildCard(key: String, payload: NotificationPayload): VBox = {
    val level = payload.level
    val baseStyle =
      s"-fx-background-color: ${level.background};" +
        s"-fx-border-color: ${level.borderColor};" +
        "-fx-border-radius: 8;" +
        "-fx-background-radius: 8;" +
        "-fx-padding: 12;" +
        "-fx-effect: dropshadow(gaussian, rgba(0,0,0,0.25), 6, 0, 0, 2);"

    val titleLabel = new Label(payload.title) {
      style = s"-fx-text-fill: ${level.accent}; -fx-font-size: 13px; -fx-font-weight: bold;"
    }

    val iconBox = new Label(level.iconGlyph) {
      style = s"-fx-text-fill: ${level.accent}; -fx-font-size: 16px; -fx-font-weight: bold;"
      padding = Insets(0, 8, 0, 0)
    }

    val headerBox = new HBox {
      spacing = 8
      alignment = Pos.CenterLeft
      children = Seq(iconBox, titleLabel)
    }

    val bodyNodes = new VBox {
      spacing = 6
    }

    payload.body.foreach { text =>
      bodyNodes.children.add(new Label(text) {
        wrapText = true
        style = "-fx-text-fill: #e0e0e0;"
      })
    }

    payload.customContent.foreach(node => bodyNodes.children.add(node))

    def createActionButton(action: NotificationAction): Button =
      new Button(action.label) {
        styleClass ++= Seq("notification-action", s"${action.styleClass}-action")
        mnemonicParsing = false
        minWidth = Region.USE_PREF_SIZE
        onAction = _ =>
          action.onTrigger()
          if action.closeOnTrigger then clear(key)
      }

    val dismissButtonOpt =
      if payload.dismissible then
        Some(new Button("Dismiss") {
          styleClass ++= Seq("notification-action", "dismiss-action")
          mnemonicParsing = false
          minWidth = Region.USE_PREF_SIZE
          onAction = _ => clear(key)
        })
      else None

    val actionButtons: Region =
      if payload.actions.nonEmpty || payload.dismissible then
        val flow = new FlowPane {
          hgap = 8
          vgap = 8
          alignment = Pos.CenterLeft
          prefWrapLength = 420
        }
        val buttonNodes = payload.actions.map(createActionButton) ++ dismissButtonOpt.toSeq
        flow.children ++= buttonNodes.map(_.delegate)
        flow
      else
        new Region {
          minHeight = 0
          prefHeight = 0
          maxHeight = 0
        }

    new VBox {
      style = baseStyle
      spacing = 10
      children = Seq(headerBox, bodyNodes, actionButtons)
    }
  }
}
