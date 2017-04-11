lazy val commonSettings: Seq[Def.Setting[_]] = Seq(
  scalaVersion := "2.12.1",
  scalaOrganization := "org.typelevel",

  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.bintrayIvyRepo("scalameta", "maven")
  ),

  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M7" cross CrossVersion.full),


  scalacOptions in (Compile, console) := Seq("-Yliteral-types"), // macroparadise plugin doesn't work in repl yet.
  sources in (Compile, doc) := Nil, // macroparadise doesn't work with scaladoc yet.

  scalacOptions ++= Seq(
    "-Yliteral-types",
    "-Xplugin-require:macroparadise",
    "-language:experimental.macros"
  )
)

lazy val troy = project.settings(
  commonSettings,

  libraryDependencies ++= Seq(
    "com.chuusai" %% "shapeless" % "2.3.2",
    "org.scala-lang" % "scala-reflect" % "2.12.1",
    "com.datastax.cassandra" % "cassandra-driver-core" % "3.0.0",
    "com.lihaoyi" %% "fastparse" % "0.4.2",
    "org.scalameta" %% "scalameta" % "1.6.0"
  )
)

lazy val demo = project.settings(commonSettings).dependsOn(troy)
