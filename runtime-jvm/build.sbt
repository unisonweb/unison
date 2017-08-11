import com.typesafe.sbt.pgp.PgpKeys.publishSigned
import sbtrelease.Version

val ReleaseTag = """^release/([\d\.]+a?)$""".r

scalaVersion := "2.12.2"

lazy val commonSettings = Seq(
  organization := "org.unisonweb",
  isScalaJSProject := false,
  scalaVersion := "2.12.2",
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-opt:l:classpath",
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
  libraryDependencies ++= Seq(
    compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
    "org.scalatest" %%% "scalatest" % "3.0.0" % "test",
    "org.scalacheck" %%% "scalacheck" % "1.13.4" % "test"
  ),
  scmInfo := Some(ScmInfo(url("https://github.com/unisonweb/unison"), "git@github.com:unisonweb/unison.git")),
  homepage := Some(url("https://unisonweb.org")),
  licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
) ++ testSettings ++ publishingSettings

lazy val contributors = Seq("pchiusano" -> "Paul Chiusano")

lazy val testSettings = Seq(
  parallelExecution in Test := false,
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
  // fork in Test := true, Causes issues on Travis
  publishArtifact in Test := true
)

def scmBranch(v: String): String = {
  val Some(ver) = Version(v)
  if(ver.qualifier.exists(_ == "-SNAPSHOT"))
    // support branch (0.9.0-SNAPSHOT -> series/0.9)
    s"series/${ver.copy(subversions = ver.subversions.take(1), qualifier = None).string}"
  else
    // release tag (0.9.0-M2 -> v0.9.0-M2)
    s"v${ver.string}"
}

lazy val publishingSettings = Seq(
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (version.value.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  credentials ++= (for {
    username <- Option(System.getenv().get("SONATYPE_USERNAME"))
    password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
  } yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq,
  publishMavenStyle := true,
  pomIncludeRepository := { _ => false },
  pomExtra := {
    <developers>
      {for ((username, name) <- contributors) yield
      <developer>
        <id>{username}</id>
        <name>{name}</name>
        <url>http://github.com/{username}</url>
      </developer>
      }
    </developers>
  },
  pomPostProcess := { node =>
    import scala.xml._
    import scala.xml.transform._
    def stripIf(f: Node => Boolean) = new RewriteRule {
      override def transform(n: Node) =
        if (f(n)) NodeSeq.Empty else n
    }
    val stripTestScope = stripIf { n => n.label == "dependency" && (n \ "scope").text == "test" }
    new RuleTransformer(stripTestScope).transform(node)(0)
  }
)

lazy val noPublish = Seq(
  publish := (),
  publishLocal := (),
  publishSigned := (),
  publishArtifact := false
)

lazy val releaseSettings = Seq(
  releasePublishArtifactsAction := PgpKeys.publishSigned.value
)

lazy val root = project.in(file(".")).
  settings(name := "unison-runtime-root").
  settings(commonSettings).
  settings(noPublish).
  aggregate(main, benchmark)
  
lazy val main = project.in(file("main")).
  settings(commonSettings).
  settings(name := "unison-runtime").
  settings(sourceGenerators in Compile += Def.task {
    import org.unisonweb.codegeneration._
    val outPath = (sourceManaged in Compile).value / "org" / "unisonweb" / "compilation"
    List(
      ArityGenerator(outPath),
      LambdaGenerator(outPath),
      CompileVarGenerator(outPath),
      LookupVarGenerator(outPath),
      TailCallGenerator(outPath),
      RuntimeGenerator(outPath),
      StaticCallGenerator(outPath),
      DynamicCallGenerator(outPath),
      If0Generator(outPath)
    ).map { case (file, content) => IO.write(file, content); file }
  }.taskValue)

lazy val benchmark = project.in(file("benchmark")).
  settings(commonSettings).
  settings(noPublish).
  settings(
    name := "unison-runtime-benchmark"
  )
  .settings(
    libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value
  )
  .enablePlugins(JmhPlugin)
  .dependsOn(main)
