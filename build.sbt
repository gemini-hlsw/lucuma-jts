
inThisBuild(Seq(
  homepage := Some(url("https://github.com/gemini-hlsw/gpp-jts")),
  Global / onChangedBuildSource := ReloadOnSourceChanges
) ++ gspPublishSettings)

skip in publish := true

lazy val jts = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/jts"))
  .settings(
    name := "gpp-jts",
    publishArtifact in (Compile, packageDoc) := false,
    scalacOptions ~= (_.filterNot(
      Set(
        // By necessity facades will have unused params
        "-Wdead-code",
        "-Wunused:params",
        "-Ywarn-dead-code",
        "-Ywarn-unused:params"
      )))
  )

lazy val jts_awt = project
  .in(file("modules/jts-awt"))
  .settings(
    name := "gpp-jts-awt",
    publishArtifact in (Compile, packageDoc) := false,
    scalacOptions ~= (_.filterNot(
      Set(
        // By necessity facades will have unused params
        "-Wdead-code",
        "-Wunused:params",
        "-Ywarn-dead-code",
        "-Ywarn-unused:params"
      )))
  )
  .dependsOn(jts.jvm)
