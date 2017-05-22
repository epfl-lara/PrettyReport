name := "PrettyReport"

organization := "ch.epfl.lara"

licenses := Seq("GNU General Public License, Version 3" -> url("http://www.gnu.org/licenses/gpl-3.0.html"))

version := "1.0-SNAPSHOT"

scalaVersion := "2.12.2"

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature"
)

libraryDependencies ++= Seq(
  "org.json4s" %% "json4s-native" % "3.5.2"
)

lazy val scriptName = settingKey[String]("Name of the generated 'prettyreport' script")
lazy val scriptFile = taskKey[File]("Location of the generated 'prettyreport' script (computed from 'scriptName')")
lazy val script = taskKey[Unit]("Generate the prettyreport Bash script")

scriptName := "prettyreport"
lazy val binDir = file("./bin")
scriptFile := binDir / scriptName.value

clean := {
  clean.value
  val file = scriptFile.value
  if (file.exists && file.isFile) file.delete
}

script := {
  val s = streams.value
  val jarjarbin = assembly.value
  s.log.info(s"Generating script for $jarjarbin")
  val file = scriptFile.value
  try {
    if (file.exists) {
      s.log.info("Regenerating '" + file.getName + "' script")
      file.delete
    } else {
      s.log.info("Generating '" + file.getName + "' script")
    }

    val parentDir = file.getParentFile
    if (parentDir != null && !parentDir.exists) {
      s.log.info("Creating directory/ies '" + parentDir.getName + "'")
      parentDir.mkdirs()
    }

    IO.write(file, s"""|#!/bin/bash --posix
                       |
                       |java -jar $jarjarbin $$@ 2>&1
                       |""".stripMargin)
    file.setExecutable(true)

    s.log.info(s"Script was written to ${file.getAbsolutePath}")
  } catch {
    case e: Throwable =>
      s.log.error("There was an error while generating the script file: " + e.getLocalizedMessage)
  }
}

