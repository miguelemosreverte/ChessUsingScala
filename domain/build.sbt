name := "domain"

version := "0.1"

scalaVersion := "2.13.3"

unmanagedJars in Compile += file("./resources/libs/userinput.jar")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.2" % "test"
