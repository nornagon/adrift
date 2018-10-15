import mill._
import mill.scalalib._
import ammonite.ops._

object adrift extends SbtModule {
  val lwjglVersion = "3.1.2"
  override def forkArgs = if (System.getProperty("os.name") startsWith "Mac")
    Seq("-XstartOnFirstThread")
  else Seq.empty[String]
  override def millSourcePath = pwd
  override def scalaVersion = "2.12.4"
  override def ivyDeps = Agg(
    ivy"org.choco-solver:choco-solver:4.0.6",
    ivy"org.choco-solver:choco-graph:4.2.2",

    ivy"io.circe::circe-core:0.9.1",
    ivy"io.circe::circe-generic:0.9.1",
    ivy"io.circe::circe-generic-extras:0.9.1",
    ivy"io.circe::circe-parser:0.9.1",
    ivy"io.circe::circe-optics:0.9.1",
    ivy"io.circe::circe-yaml:0.7.0",
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
}
