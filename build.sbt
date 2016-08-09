name := """hello-akka"""

version := "1.0"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.3.11",
  "com.typesafe.akka" %% "akka-testkit" % "2.3.11"
)

testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")


fork in run := true

fork in run := true