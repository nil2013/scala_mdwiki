name := "scala_mdwiki"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "com.tristanhunt" %% "knockoff" % "0.8.2",
  "com.github.rjeschke" % "txtmark" % "0.10"
)     

play.Project.playScalaSettings
