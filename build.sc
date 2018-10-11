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
