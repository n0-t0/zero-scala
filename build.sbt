val scala3Version = "3.2.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "optimize",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies  ++= Seq(
      "org.scalanlp" %% "breeze" % "2.1.0",
      "org.scalanlp" %% "breeze-viz" % "2.1.0"
    )
  )
