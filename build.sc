import mill._
import mill.scalalib._
import ammonite.ops._
import coursier.maven.MavenRepository
import mill.modules.Jvm
import mill.eval.Result

object adrift extends ScalaModule {

  val lwjglVersion = "3.1.2"
  override def forkArgs = Seq("-Xmx12g") ++ (
    if (System.getProperty("os.name") startsWith "Mac")
      Seq("-XstartOnFirstThread")
    else Seq.empty)
  override def scalaVersion = "2.13.1"

  override def mainClass = Some("adrift.Main")

  override def repositories = super.repositories ++ Seq(
    MavenRepository("https://oss.sonatype.org/content/repositories/snapshots/"),
    MavenRepository("https://oss.sonatype.org/content/repositories/releases/")
  )

  def runAlt(mainClass: String, args: String*) = T.command{
    try Result.Success(Jvm.runSubprocess(
      mainClass,
      runClasspath().map(_.path),
      jvmArgs = Seq.empty,
      envArgs = forkEnv(),
      mainArgs = args,
      workingDir = forkWorkingDir()
    )) catch { case e: Exception =>
      Result.Failure("subprocess failed")
    }
  }


  override def ivyDeps = Agg(
    ivy"org.choco-solver:choco-solver:4.0.6",
    ivy"org.choco-solver:choco-graph:4.2.2",

    ivy"org.scalanlp::breeze:1.0",
    ivy"org.scalanlp::breeze-natives:1.0",
    ivy"org.scalanlp::breeze-viz:1.0",

    ivy"io.circe::circe-core:0.13.0",
    ivy"io.circe::circe-generic:0.13.0",
    ivy"io.circe::circe-generic-extras:0.13.0",
    ivy"io.circe::circe-parser:0.13.0",
    ivy"io.circe::circe-optics:0.13.0",
    ivy"io.circe::circe-yaml:0.13.0",
  ) ++ Agg.from(
    Seq(
      "lwjgl",
      "lwjgl-glfw",
      "lwjgl-opengl",
      "lwjgl-stb",
    ).flatMap { s =>
      Seq(
        ivy"org.lwjgl:$s:$lwjglVersion",
        ivy"org.lwjgl:$s:$lwjglVersion;classifier=natives-linux",
        ivy"org.lwjgl:$s:$lwjglVersion;classifier=natives-macos",
      )
    }
  )

  override def unmanagedClasspath = Agg(
    mill.modules.Util.download(
      "http://www.softsynth.com/jsyn/developers/archives/jsyn-20171016.jar",
      "jsyn-20171016.jar"
    )
  )

  object test extends Tests {
    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.7.1")
    def testFrameworks = Seq("utest.runner.Framework")
  }
}
