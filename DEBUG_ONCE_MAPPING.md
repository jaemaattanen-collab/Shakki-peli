# Debug Once Instrumentation Map (Filtered)

_Generated on 2025-11-22 23:04:37_

This list contains functions with **no obvious call-sites** based on automated search heuristics. Instrument these first.

## `src/main/scala/chess/analysis/EngineConfig.scala`

| ID | Function | Line | Signature |
| --- | --- | --- | --- |
| 2 | `depthDescription` | 23 | `def depthDescription: String = depth match {` |

## `src/main/scala/chess/analysis/FENGenerator.scala`

| ID | Function | Line | Signature |
| --- | --- | --- | --- |
| 9 | `parseFEN` | 150 | `def parseFEN(fen: String): Map[String, String] = {` |

## `src/main/scala/chess/analysis/MoveNotationConverter.scala`

| ID | Function | Line | Signature |
| --- | --- | --- | --- |
| 10 | `uciToChessNotation` | 17 | `def uciToChessNotation(uciMove: String, board: Board): String = {` |

## `src/main/scala/chess/analysis/StockfishEngine.scala`

| ID | Function | Line | Signature |
| --- | --- | --- | --- |
| 17 | `updateConfig` | 182 | `def updateConfig(newConfig: EngineConfig): Unit = {` |
| 19 | `getBestMove` | 206 | `def getBestMove(fen: String, timeMs: Int = 1000): Future[Option[String]] = {` |

## `src/main/scala/chess/board/Board.scala`

| ID | Function | Line | Signature |
| --- | --- | --- | --- |
| 31 | `updateSquarePositions` | 114 | `def updateSquarePositions(): Unit =` |
| 35 | `getSquareAt` | 196 | `def getSquareAt(x: Int, y: Int): Option[Square] =` |
| 38 | `clearAllHighlights` | 228 | `def clearAllHighlights(): Unit =` |

## `src/main/scala/chess/board/Square.scala`

| ID | Function | Line | Signature |
| --- | --- | --- | --- |
| 41 | `highlightType` | 18 | `def highlightType: Option[HighlightType] = _highlightType` |
| 49 | `drawWeakness` | 51 | `def drawWeakness(): Unit =` |

## `src/main/scala/chess/core/AlgebraicNotationParser.scala`

| ID | Function | Line | Signature |
| --- | --- | --- | --- |
| 84 | `parseAndExecute` | 19 | `def parseAndExecute(notation: String, board: Board, controller: GameController, isWhite: Boolean): Boolean = {` |

## `src/main/scala/chess/core/NotationManager.scala`

| ID | Function | Line | Signature |
| --- | --- | --- | --- |
| 93 | `getNotation` | 39 | `def getNotation(): String = moves.mkString(" ")` |
| 96 | `reset` | 55 | `def reset(): Unit = clear()` |

## `src/main/scala/chess/pgn/LiveVariationRepository.scala`

| ID | Function | Line | Signature |
| --- | --- | --- | --- |
| 107 | `collectVariation` | 64 | `def collectVariation(pgnVariation: PGNVariation): Unit = {` |
| 110 | `variationToPGN` | 98 | `private def variationToPGN(lv: LiveVariation): PGNVariation = {` |

## `src/main/scala/chess/pgn/PGNExporter.scala`

| ID | Function | Line | Signature |
| --- | --- | --- | --- |
| 113 | `exportGame` | 16 | `def exportGame(` |
| 115 | `exportGameWithHeaders` | 61 | `def exportGameWithHeaders(` |

## `src/main/scala/chess/pgn/PGNManager.scala`

| ID | Function | Line | Signature |
| --- | --- | --- | --- |
| 124 | `loadPGNFromString` | 46 | `def loadPGNFromString(pgnText: String): Either[String, PGNGame] = {` |
| 126 | `getCurrentPGNString` | 80 | `def getCurrentPGNString: String = {` |
| 127 | `getCurrentPGN` | 87 | `def getCurrentPGN: Option[PGNGame] = currentPGN` |
| 128 | `getHeaders` | 92 | `def getHeaders: Map[String, String] = {` |
| 130 | `updateHeader` | 106 | `def updateHeader(key: String, value: String): Unit = {` |
| 131 | `getResult` | 114 | `def getResult: Option[String] = {` |
| 132 | `setResult` | 121 | `def setResult(result: String): Unit = {` |
| 133 | `getPGNMoves` | 128 | `def getPGNMoves: List[PGNMove] = {` |
| 134 | `convertToPGNMoves` | 135 | `def convertToPGNMoves: List[PGNMove] = {` |
| 135 | `validatePGN` | 144 | `def validatePGN(pgnText: String): (Boolean, List[String], List[String]) = {` |
| 136 | `getPGNStats` | 155 | `def getPGNStats: Map[String, Any] = {` |
| 137 | `clearPGN` | 181 | `def clearPGN(): Unit = {` |
| 138 | `hasUnsavedChanges` | 188 | `def hasUnsavedChanges: Boolean = {` |

## `src/main/scala/chess/pieces/Piece.scala`

| ID | Function | Line | Signature |
| --- | --- | --- | --- |
| 197 | `promoteTo` | 37 | `def promoteTo(newType: String): Unit =` |

## `src/main/scala/chess/state/Position.scala`

| ID | Function | Line | Signature |
| --- | --- | --- | --- |
| 208 | `equals` | 24 | `override def equals(other: Any): Boolean = other match` |

## `src/main/scala/chess/ui/AnalysisManager.scala`

| ID | Function | Line | Signature |
| --- | --- | --- | --- |
| 213 | `snapshotLines` | 105 | `def snapshotLines: Seq[String] =` |
| 223 | `formatUciMove` | 246 | `private def formatUciMove(uci: String): String =` |

## `src/main/scala/chess/ui/ScalaFXChessApp.scala`

| ID | Function | Line | Signature |
| --- | --- | --- | --- |
| 244 | `showVariationPicker` | 147 | `private def showVariationPicker(moveIndex: Int): Unit = {` |
| 261 | `fileLabel` | 626 | `def fileLabel(text: String): Label = new Label(text) {` |
| 268 | `loadPGNFile` | 832 | `private def loadPGNFile(file: File): Unit = {` |
| 292 | `handleMoveHistoryClick` | 1270 | `private def handleMoveHistoryClick(moveIndex: Int): Unit = {` |

## `src/main/scala/chess/ui/ScalaFXMoveHistoryPanel.scala`

| ID | Function | Line | Signature |
| --- | --- | --- | --- |
| 298 | `clearOnMoveSelected` | 44 | `def clearOnMoveSelected(): Unit =` |
| 300 | `clearOnVariationSelected` | 50 | `def clearOnVariationSelected(): Unit =` |
| 303 | `render` | 59 | `def render(state: MoveHistoryPanelState): Unit = {` |

## `src/main/scala/chess/utils/NoOpImgeObserver.scala`

| ID | Function | Line | Signature |
| --- | --- | --- | --- |
| 311 | `imageUpdate` | 12 | `override def imageUpdate(img: Image, infoflags: Int, x: Int, y: Int, width: Int, height: Int): Boolean = false` |
