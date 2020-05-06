name := "poker-event-sourcing"

version := "0.1"

scalaVersion := "2.12.9"

libraryDependencies ++= Seq(
"org.scalatest" %% "scalatest" % "3.0.8" % Test,
"org.scalamock" %% "scalamock" % "4.1.0" % Test
)

//Dependency Resolvers
resolvers ++= Seq(
  Resolver.bintrayRepo("cakesolutions", "maven"),
  Resolver.bintrayRepo("hseeberger", "maven"),
  Resolver.sonatypeRepo("releases")
)