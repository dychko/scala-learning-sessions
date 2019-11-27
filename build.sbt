name := "learning-sessions"

version := "1.0"

scalaVersion := "2.12.8"

scalacOptions += "-Ypartial-unification"

val monixVersion = "3.0.0-RC3"
val catsVersion = "2.0.0-M4"
val circeVersion = "0.11.1"

val circe = Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "io.monix" %% "monix" % monixVersion,
  "io.monix" %% "monix-cats" % "2.3.3"
) ++ circe