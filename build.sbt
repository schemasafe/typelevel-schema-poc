scalaOrganization in ThisBuild := "org.typelevel"

scalaVersion := "2.12.1"

scalacOptions := Seq("-Yliteral-types")

libraryDependencies ++= Seq(
   "com.chuusai" %% "shapeless" % "2.3.2",
   "org.scala-lang" % "scala-reflect" % "2.12.1"
)
