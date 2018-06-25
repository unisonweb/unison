lazy val commonSettings = Seq(
  fork := true,
  javaOptions in run ++= Seq(
    // https://docs.oracle.com/javase/8/embedded/develop-apps-platforms/codecache.htm
//    "-XX:+UnlockDiagnosticVMOptions",
//    "-XX:+LogCompilation",
    "-XX:InlineSmallCode=9001",
    "-XX:MaxInlineLevel=35"
    //"-XX:MaxInlineSize=9001"
    //"-XX:CompileThreshold=10"
    //"-XX:MinInliningThreshold=10"
    //"-XX:FreqInlineSize"
    //"-XX:MaxTrivialSize"
    //"-XX:LiveNodeCountInliningCutoff"
  ),
  organization := "org.unisonweb",
  scalaVersion := "2.12.6",
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
//    "-g:notailcalls",
    "-opt:inline",
    "-opt-inline-from:**",
    "-Xdisable-assertions",
    "-opt-warnings",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials",
    "-language:postfixOps",
    "-Xfatal-warnings",
    "-Yno-adapted-args",
    // "-Ywarn-dead-code", // Too buggy to be useful, for instance https://issues.scala-lang.org/browse/SI-9521
    "-Ywarn-value-discard",
    "-Ywarn-unused-import"
  ),
  scalacOptions in (Compile, console) ~= {_.filterNot("-Ywarn-unused-import" == _)},
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,
  scmInfo := Some(ScmInfo(url("https://github.com/unisonweb/unison"), "git@github.com:unisonweb/unison.git")),
  homepage := Some(url("https://unisonweb.org")),
  licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
) ++ testSettings

lazy val contributors = Seq("pchiusano" -> "Paul Chiusano")

lazy val testSettings = Seq(
  parallelExecution in Test := false,
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
  // fork in Test := true, Causes issues on Travis
  publishArtifact in Test := true
)

lazy val root = project.in(file(".")).
  settings(name := "unison-runtime-root").
  settings(commonSettings).
  aggregate(main, benchmark)

lazy val main = project.in(file("main"))
  .settings(commonSettings)
  .settings(name := "unison-runtime")

lazy val benchmark = project.in(file("benchmark"))
  .settings(commonSettings)
  .settings(name := "unison-runtime-benchmark")
  .settings(scalacOptions += "-Xdisable-assertions")
  .settings(libraryDependencies +=
              scalaOrganization.value % "scala-reflect" % scalaVersion.value)
  .dependsOn(main % "compile->test")

