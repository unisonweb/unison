scalacOptions ++= Seq(
  "-feature",
  "-deprecation"
)

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")
addSbtPlugin("org.scala-sbt.plugins" % "sbt-onejar" % "0.8")
