
lazy val stainlessCommit = "3231e0ef0352163c0fa776bddf2920bdf427c697"
lazy val stainlessModule = "stainless-core"
lazy val stainlessProject = ProjectRef(uri(s"git://github.com/epfl-lara/stainless.git#$stainlessCommit"), stainlessModule)

lazy val scriptName = settingKey[String]("Name of the generated 'prettyreport' script")
lazy val scriptFile = taskKey[File]("Location of the generated 'prettyreport' script (computed from 'scriptName')")
lazy val script = taskKey[Unit]("Generate the prettyreport Bash script")
  
lazy val binDir = file("./bin")

lazy val basicSettings: Seq[Setting[_]] = Seq(
  name := "PrettyReport",
  organization := "ch.epfl.lara",
  licenses := Seq("GNU General Public License, Version 3" -> url("http://www.gnu.org/licenses/gpl-3.0.html")),
  version := "1.1",
  scalaVersion := "2.11.8",
  scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-feature"
  )
) // end of basicSettings

lazy val scriptSettings: Seq[Setting[_]] = Seq(

  scriptName := "prettyreport",
  scriptFile := binDir / scriptName.value,

  clean := {
    clean.value
    val file = scriptFile.value
    if (file.exists && file.isFile) file.delete
  },

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

) // end of scriptSettings

lazy val `pretty-report` = (project in file("."))
  .settings(basicSettings, scriptSettings)
  .dependsOn(stainlessProject)

