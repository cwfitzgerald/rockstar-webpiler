import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val sharedSettings = Seq(
	name := "rockstar",
	version := "0.1",
	scalaVersion := "2.12.6",
	libraryDependencies ++= Seq(
		"com.lihaoyi" %%% "fastparse" % "1.0.0",
		"com.lihaoyi" %%% "scalatags" % "0.6.7",
		"com.lihaoyi" %%% "utest" % "0.6.3" % "test",
		"io.github.cquiroz" %%% "scala-java-time" % "2.0.0-M13",
//		"org.typelevel" %%% "cats-core" % "1.2.0"
//		"org.typelevel" %%% "spire" % "0.16.0"
	),
	testFrameworks += new TestFramework("utest.runner.Framework")
)

lazy val root = project.in(file("."))
	.aggregate(rockstarJVM, rockstarJS)
	.settings(
		publish := {},
		publishLocal := {}
	)

enablePlugins(ScalaJSPlugin)

lazy val rockstar = crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
	.in(file("."))
	.settings(sharedSettings)
	.jsSettings(
		Seq(
			libraryDependencies ++= Seq(
				"org.querki" %%% "jquery-facade" % "1.2",
				"org.scala-js" %%% "scalajs-dom" % "0.9.6",
				"io.lemonlabs" %%% "scala-uri" % "1.1.4",
			),
			jsDependencies += "org.webjars" % "jquery" % "2.2.1" / "jquery.js" minified "jquery.min.js"
		)
	)
    .jvmSettings(
	    aggregate in fastOptJS := false,
	    aggregate in fullOptJS := false,
    )

lazy val rockstarJVM = rockstar.jvm
lazy val rockstarJS = rockstar.js
