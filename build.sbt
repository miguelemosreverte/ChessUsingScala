name := "ChessSBT"

version := "0.1"

scalaVersion := "2.13.3"

lazy val `domain` = project
lazy val `application` = project.dependsOn(domain)

libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.2" % "test"

unmanagedJars in Compile += file("./resources/libs/userinput.jar")
