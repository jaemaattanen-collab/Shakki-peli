import scoverage.ScoverageSbtPlugin.autoImport.*
import scala.xml.XML
import scala.util.Try

name := "ScalaChessGame"

version := "1.0"

scalaVersion := "3.6.2"

// Specify the main class (modern SBT syntax)
Compile / mainClass := Some("chess.ui.ScalaFXChessApp")

// Library dependencies
libraryDependencies ++= Seq(
  // Keep Swing for now during transition
  "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
  // Add ScalaFX for modern UI
  "org.scalafx" %% "scalafx" % "22.0.0-R33",
  // JavaFX dependencies for macOS
  "org.openjfx" % "javafx-controls" % "22.0.1",
  "org.openjfx" % "javafx-fxml" % "22.0.1",
  "org.openjfx" % "javafx-graphics" % "22.0.1",
  "org.openjfx" % "javafx-base" % "22.0.1",
  "org.scalatest" %% "scalatest" % "3.2.18" % Test
)

// Encourage the compiler to warn about unused definitions
ThisBuild / scalacOptions ++= Seq("-deprecation", "-Wunused:all")

// Enable coverage instrumentation globally so test runs collect hit counts
ThisBuild / coverageEnabled := true
ThisBuild / coverageOutputCobertura := true
ThisBuild / coverageOutputXML := true

lazy val printUsageSummary = Def.task {
  val log = streams.value.log
  val reportFile = crossTarget.value / "scoverage-report" / "scoverage.xml"
  if (!reportFile.exists()) {
    log.warn(s"Coverage report not found at ${reportFile.getAbsolutePath}. Run `sbt coverageReport` first.")
  } else {
  val xml = XML.loadFile(reportFile)
  val statements = (xml \\ "statement").map { node =>
      val clazz = (node \ "@class").text
      val method = (node \ "@method").text
      val invocations = Try((node \ "@invocation-count").text.toLong).getOrElse(0L)
      ((clazz, method), invocations)
    }

    val methodGroups = statements.groupBy(_._1)
    val coverages = methodGroups.map { case ((clazz, method), entries) =>
      val total = entries.length
      val covered = entries.count(_._2 > 0L)
      val hits = entries.map(_._2).sum
      (clazz, method, total, covered, hits)
    }.toList

    val uncovered = coverages.filter { case (_, _, _, covered, _) => covered == 0 }.sortBy { case (clazz, method, _, _, _) => (clazz, method) }
    val partiallyCovered = coverages.filter { case (_, _, total, covered, _) => covered > 0 && covered < total }.sortBy { case (clazz, method, _, _, _) => (clazz, method) }

    log.info(s"Coverage summary captured from ${reportFile.getName}:")
    log.info(f"  Methods observed: ${coverages.size}%d")
    log.info(f"  Fully covered: ${coverages.count { case (_, _, total, covered, _) => covered == total }}%d")
    log.info(f"  Partially covered: ${partiallyCovered.size}%d")
    log.info(f"  No coverage: ${uncovered.size}%d")

    if (uncovered.nonEmpty) {
      log.warn("Methods never executed during the instrumentation run:")
      uncovered.foreach { case (clazz, method, total, _, _) =>
        log.warn(s"    ${clazz}.${method} (statements=${total})")
      }
    }

    if (partiallyCovered.nonEmpty) {
      log.info("Methods partially exercised (some statements untouched):")
      partiallyCovered.foreach { case (clazz, method, total, covered, hits) =>
        log.info(s"    ${clazz}.${method} (${covered}/${total} statements, total hits=${hits})")
      }
    }
  }
}

lazy val usageReport = taskKey[Unit]("Run coverage-instrumented tests and emit a usage summary")

usageReport := Def.sequential(
  Test / test,
  coverageReport,
  printUsageSummary
).value
