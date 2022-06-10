lazy val root = Project(id = "lsp-demo", base = file("."))
  .settings(moduleName := "root")
  .settings(
    publish         := {},
    publishLocal    := {},
    publishArtifact := false
  )
  .aggregate(server)

lazy val server = project
  .in(file("server"))
  .settings(
    scalaVersion   := "3.1.3",
    scalacOptions ++= Seq("-source", "future", "-Ykind-projector:underscores", "-deprecation", "-unchecked"),
    libraryDependencies ++= Seq(
      "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.14.0"
    ),
    assembly / assemblyJarName := "lsp-demo-server.jar",
    assembly / mainClass       := Some("lsp.main"),
    buildInfoKeys              := Seq[BuildInfoKey](name, version, scalaVersion),
    buildInfoPackage           := "lsp"
  )
  .enablePlugins(BuildInfoPlugin)
