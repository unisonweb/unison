scalacOptions ++= Seq(
  "-feature",
  "-deprecation"
)

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")
addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.0.0")
