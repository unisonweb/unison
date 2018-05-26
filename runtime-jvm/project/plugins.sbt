scalacOptions ++= Seq(
  "-feature",
  "-deprecation"
)

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")
addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.3")
