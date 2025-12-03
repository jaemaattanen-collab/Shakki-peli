package chess.tools

import java.io.{File, PrintWriter}
import scala.io.Source
import scala.util.{Try, Using}

/**
 * Tool for splitting multi-game PGN files into individual game files.
 * 
 * Usage: Run with path to PGN file containing multiple games.
 * Output: Individual PGN files in Peliarkisto folder, named by players and date.
 */
object PgnSplitter {
  
  case class GameInfo(
    white: String,
    black: String,
    date: String,
    result: String,
    content: String
  )
  
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("PGN Splitter - Jakaa usean pelin PGN-tiedoston erillisiksi tiedostoiksi")
      println()
      println("Käyttö: sbt \"runMain chess.tools.PgnSplitter <pgn-tiedosto>\"")
      println()
      println("Esimerkki:")
      println("  sbt \"runMain chess.tools.PgnSplitter /Users/jaakko/Downloads/chess_com_games.pgn\"")
      println()
      println("Tulostiedostot tallennetaan Peliarkisto-kansioon.")
    } else {
      val inputFile = args(0)
      splitPgnFile(inputFile)
    }
  }
  
  def splitPgnFile(inputPath: String): Unit = {
    val file = new File(inputPath)
    if (!file.exists()) {
      println(s"Virhe: Tiedostoa ei löydy: $inputPath")
    } else {
      println(s"Luetaan tiedostoa: $inputPath")
      
      Using(Source.fromFile(file, "UTF-8")) { source =>
        source.mkString
      } match {
        case scala.util.Success(content) =>
          processContent(content)
        case scala.util.Failure(_) =>
          println("Virhe: Tiedoston lukeminen epäonnistui")
      }
    }
  }
  
  private def processContent(content: String): Unit = {
    val games = parseGames(content)
    
    if (games.isEmpty) {
      println("Ei löytynyt pelejä tiedostosta.")
    } else {
      println(s"Löydettiin ${games.length} peliä.")
      
      // Determine output directory (Peliarkisto in project root)
      val projectRoot = new File(".").getCanonicalPath
      val outputDir = new File(projectRoot, "Peliarkisto")
      
      if (!outputDir.exists()) {
        outputDir.mkdirs()
        println(s"Luotiin kansio: ${outputDir.getPath}")
      }
      
      // Save each game
      var savedCount = 0
      for (game <- games) {
        val filename = generateFilename(game, outputDir)
        val outputFile = new File(outputDir, filename)
        
        Try {
          Using(new PrintWriter(outputFile, "UTF-8")) { writer =>
            writer.write(game.content)
          }
        }.fold(
          error => println(s"  Virhe tallennettaessa: ${error.getMessage}"),
          _ => {
            savedCount += 1
            println(s"  [$savedCount] ${game.white} vs ${game.black} -> $filename")
          }
        )
      }
      
      println()
      println(s"Valmis! Tallennettiin $savedCount peliä kansioon: ${outputDir.getPath}")
    }
  }
  
  /**
   * Parse PGN content into individual games
   */
  def parseGames(content: String): List[GameInfo] = {
    // Split by [Event which starts a new game
    // But keep the [Event tag with the game
    val gameTexts = content.split("(?=\\[Event )").filter(_.trim.nonEmpty)
    
    gameTexts.map(parseGame).toList
  }
  
  /**
   * Parse a single game's PGN text into GameInfo
   */
  def parseGame(gameText: String): GameInfo = {
    val lines = gameText.split("\n")
    
    var white = "Unknown"
    var black = "Unknown"
    var date = ""
    var result = "*"
    
    for (line <- lines) {
      val trimmed = line.trim
      if (trimmed.startsWith("[White \"")) {
        white = extractTagValue(trimmed)
      } else if (trimmed.startsWith("[Black \"")) {
        black = extractTagValue(trimmed)
      } else if (trimmed.startsWith("[Date \"")) {
        date = extractTagValue(trimmed)
      } else if (trimmed.startsWith("[Result \"")) {
        result = extractTagValue(trimmed)
      }
    }
    
    GameInfo(white, black, date, result, gameText.trim)
  }
  
  /**
   * Extract value from PGN tag like [White "PlayerName"]
   */
  def extractTagValue(tag: String): String = {
    val start = tag.indexOf('"')
    val end = tag.lastIndexOf('"')
    if (start >= 0 && end > start) {
      tag.substring(start + 1, end)
    } else {
      ""
    }
  }
  
  /**
   * Generate unique filename for a game
   */
  def generateFilename(game: GameInfo, outputDir: File): String = {
    // Clean player names for filename
    val whiteName = cleanForFilename(game.white)
    val blackName = cleanForFilename(game.black)
    
    // Format date (2025.12.01 -> 2025-12-01)
    val dateStr = game.date.replace(".", "-")
    
    // Base filename
    val baseName = s"${whiteName}_vs_${blackName}-$dateStr"
    
    // Check if file exists, add number if needed
    var filename = s"$baseName.pgn"
    var counter = 2
    
    while (new File(outputDir, filename).exists()) {
      filename = s"${baseName}_$counter.pgn"
      counter += 1
    }
    
    filename
  }
  
  /**
   * Clean string for use in filename
   */
  def cleanForFilename(name: String): String = {
    name
      .replaceAll("[^a-zA-Z0-9_-]", "_")
      .replaceAll("_+", "_")
      .take(30)  // Limit length
  }
}
