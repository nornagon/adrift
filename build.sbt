lazy val osName =
  System.getProperty("os.name").split(" ")(0).toLowerCase()

lazy val startOnFirst =
  if (osName == "mac")
    Some("-XstartOnFirstThread")
  else
    None

lazy val lwjglVersion = "3.1.2"

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Adrift",
    libraryDependencies ++= Seq(
      "org.lwjgl" % "lwjgl" % lwjglVersion,
      "org.lwjgl" % "lwjgl" % lwjglVersion % "runtime" classifier "natives-windows" classifier "natives-linux" classifier "natives-macos",
      "org.lwjgl" % "lwjgl-glfw" % lwjglVersion,
      "org.lwjgl" % "lwjgl-glfw" % lwjglVersion % "runtime" classifier "natives-windows" classifier "natives-linux" classifier "natives-macos",
      "org.lwjgl" % "lwjgl-opengl" % lwjglVersion,
      "org.lwjgl" % "lwjgl-opengl" % lwjglVersion % "runtime" classifier "natives-windows" classifier "natives-linux" classifier "natives-macos",
      "org.lwjgl" % "lwjgl-stb" % lwjglVersion,
      "org.lwjgl" % "lwjgl-stb" % lwjglVersion % "runtime" classifier "natives-windows" classifier "natives-linux" classifier "natives-macos"
    ),
    fork in run := true, // todo: only when startOnFirst
    javaOptions in run ++= startOnFirst.toSeq
  )
