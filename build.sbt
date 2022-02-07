ThisBuild / tlBaseVersion       := "0.2"
ThisBuild / tlCiReleaseBranches := Seq("master")

Global / onChangedBuildSource  := ReloadOnSourceChanges
ThisBuild / crossScalaVersions := Seq("3.1.1", "2.13.8")
ThisBuild / githubWorkflowBuildPreamble += WorkflowStep.Sbt(List("show coverageEnabled"))

lazy val root = tlCrossRootProject.aggregate(jts, jts_awt, tests)

lazy val jts = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/jts"))
  .settings(
    name                    := "lucuma-jts",
    Compile / doc / sources := Seq(),
    Compile / packageDoc / scalacOptions ~= (_.filterNot(
      Set(
        "-Werror",
        "-Xlint:doc-detached",
        "-Ywarn-unused:params",
        "-Xfatal-warnings"
      )
    )),
    scalacOptions ~= (_.filterNot(
      Set(
        // Legacy code needs to disable these
        "-Wdead-code",
        "-Wunused:params",
        "-Wunused:explicits",
        "-Ywarn-dead-code",
        "-Ywarn-unused:params",
        "-Xlint:doc-detached"
      )
    ))
  )

lazy val jts_awt = project
  .in(file("modules/jts-awt"))
  .settings(
    name                    := "lucuma-jts-awt",
    Compile / doc / sources := Seq(),
    Compile / packageDoc / scalacOptions ~= (_.filterNot(
      Set(
        "-Werror"
      )
    )),
    scalacOptions ~= (_.filterNot(
      Set(
        // By necessity facades will have unused params
        "-Wdead-code",
        "-Wunused:params",
        "-Ywarn-dead-code",
        "-Ywarn-unused:params",
        "-Xlint:doc-detached"
      )
    ))
  )
  .dependsOn(jts.jvm)

lazy val tests = project
  .in(file("modules/tests"))
  .enablePlugins(NoPublishPlugin)
  .settings(
    name                                   := "lucuma-jts-tests",
    Compile / doc / sources                := Seq(),
    libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % "test"
  )
  .dependsOn(jts.jvm)
