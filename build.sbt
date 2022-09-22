ThisBuild / tlBaseVersion       := "0.3"
ThisBuild / tlCiReleaseBranches := Seq("master")

Global / onChangedBuildSource  := ReloadOnSourceChanges
ThisBuild / crossScalaVersions := Seq("3.2.0", "2.13.9")
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
    tlFatalWarnings         := false, // Legacy code needs to disable these
    mimaPreviousArtifacts ~= { _.filterNot(_.revision == "0.2.1") } // not released
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
    tlFatalWarnings         := false,
    tlVersionIntroduced     := Map("3" -> "0.2.2"),
    mimaPreviousArtifacts ~= { _.filterNot(_.revision == "0.2.1") }, // not released
    headerLicense := Some(
      HeaderLicense.Custom(
        """|Copyright (c) 2016 Vivid Solutions.
           |
           |All rights reserved. This program and the accompanying materials
           |are made available under the terms of the Eclipse Public License v1.0
           |and Eclipse Distribution License v. 1.0 which accompanies this distribution.
           |The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
           |and the Eclipse Distribution License is available at
           |
           |http://www.eclipse.org/org/documents/edl-v10.php.""".stripMargin
      )
    )
  )
  .dependsOn(jts.jvm)

lazy val tests = project
  .in(file("modules/tests"))
  .enablePlugins(NoPublishPlugin)
  .settings(
    name                                   := "lucuma-jts-tests",
    Compile / doc / sources                := Seq(),
    libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % "test",
    tlFatalWarnings                        := false
  )
  .dependsOn(jts.jvm)
