import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val commonSettings = Seq(
	version := "0.1",
	scalaVersion := "2.12.6",
	scalacOptions ++= Seq(
		"-feature",
		"-deprecation",
		"-unchecked"
	)
)

lazy val rockstar = crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
	.in(file("rockstar"))
	.settings(commonSettings ++ Seq(
		name := "rockstar",
		libraryDependencies ++= Seq(
			"com.lihaoyi" %%% "fastparse" % "2.1.0",
			"com.lihaoyi" %%% "scalatags" % "0.6.7",
			"com.lihaoyi" %%% "utest" % "0.6.3" % "test",
			"io.github.cquiroz" %%% "scala-java-time" % "2.0.0-M13",
		),
		testFrameworks += new TestFramework("utest.runner.Framework")
	))
    .jsSettings(Seq(
	    test := {}
    ))

lazy val rockstarJS = rockstar.js
lazy val rockstarJVM = rockstar.jvm

lazy val frontend = crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("frontend"))
	.settings(commonSettings ++ Seq(
		name := "frontend",
		test := {},
		libraryDependencies ++= Seq(
			"com.lihaoyi" %%% "scalatags" % "0.6.7",
			"io.github.cquiroz" %%% "scala-java-time" % "2.0.0-M13",
		)
	))
	.jsSettings(
		Seq(
			libraryDependencies ++= Seq(
				"org.querki" %%% "jquery-facade" % "1.2",
				"org.scala-js" %%% "scalajs-dom" % "0.9.6",
				"io.lemonlabs" %%% "scala-uri" % "1.1.4",
				"com.lihaoyi" %%% "ujson" % "0.6.6",
				"com.github.karasiq" %%% "scalajs-bootstrap-v4" % "2.3.2"
			),
			jsDependencies ++= Seq(
				"org.webjars" % "jquery" % "3.3.1" / "jquery.js" minified "jquery.min.js",
				"org.webjars" % "bootstrap" % "4.1.2" / "js/bootstrap.bundle.js" minified "js/bootstrap.bundle.min.js"
			)
		)
	)
    .dependsOn(rockstar)

lazy val frontendJS: Project = frontend.js
lazy val frontendJVM: Project = frontend.jvm

def combineFiles(inFiles: File*)(outfile: File): Unit = {
	IO.write(outfile, inFiles.map(IO.read(_)).reduce(_ + _))
	println(s"Copying ${inFiles.length} files into ${outfile.getCanonicalPath}")
}

frontendJS / Compile / fastOptJS := {
	val result = (frontendJS / Compile / fastOptJS).value
	val base = frontendJS.base / "target" / "scala-2.12"
	combineFiles(base / "frontend-jsdeps.js", base / "frontend-fastopt.js")(baseDirectory.value / "bundle" / "static" / "js" / "rs.js")
	result
}

frontendJS / Compile / fullOptJS := {
	val result = (frontendJS / Compile / fullOptJS).value
	val base = frontendJS.base / "target" / "scala-2.12"
	combineFiles(base / "frontend-jsdeps.js", base / "frontend-opt.js")(baseDirectory.value / "bundle" / "static" / "js" / "rs.js")
	result
}
