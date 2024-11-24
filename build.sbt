import complete.DefaultParsers.*
import Dependencies.*
import java.nio.file.Paths

lazy val installPurescript = TaskKey[Unit]("installPurescript", "Install purescript")
installPurescript := {
  import sys.process.*
  "npm install" !
}

lazy val buildPurescript = TaskKey[Unit]("buildPurescript", "Build frontend")
buildPurescript := {
  import sys.process.*
  "npm run build" !
}

lazy val checksumHTML = TaskKey[Unit]("checksum", "Compute index.html checksum")
checksumHTML := {
    import sys.process.*
    "npm run checksum" !
}

lazy val packagePurescript = TaskKey[Unit]("packagePurescript", "Package frontend")
packagePurescript := {
  import sys.process.*
  Process(Seq("bash", "-c", "npm run package"), None, "CURRENT_COMMIT" -> "development").!
}

lazy val keepPackagingPurescript = TaskKey[Unit]("keepPackagingPurescript", "Keep packaging frontend")
keepPackagingPurescript := {
  import sys.process.*
  "npm run keep-packaging" !
}

lazy val servePurescript = TaskKey[Unit]("servePurescript", "Serve frontend")
servePurescript := {
  import sys.process.*
  "npm run go" !
}

lazy val cleanDependenciesPurescript =
  TaskKey[Unit]("cleanDependenciesPurescript", "Clean dependencies frontend")
cleanDependenciesPurescript := {
  import sys.process.*
  "npm run nuke" !
}

lazy val runTestPurescript = TaskKey[Unit]("runTestPurescript", "Run test frontend")
runTestPurescript := {
  import sys.process.*
  "npm run test-browser" !
}

lazy val cleanTargetSubdir = inputKey[Unit]("Clean the given subdirectory of the target directory")
cleanTargetSubdir := {
  import sys.process.*
  val directory: String = spaceDelimited("<arg>").parsed(0)
  val path = Paths.get(baseDirectory.value.toString(), "target", directory)
  s"rm -rf ${path}" !
}

//=====================================================================

ThisBuild / organization := "is.clipperz"
ThisBuild / scalaVersion := "3.5.1"

ThisBuild / scalacOptions ++=
  Seq(
    "-deprecation",
    "-feature",
    "-language:implicitConversions",
    "-unchecked",
    "-Xfatal-warnings",
    "-Yexplicit-nulls", // experimental (I've seen it cause issues with circe)
    "-Xkind-projector",
    "-Wsafe-init", // experimental (I've seen it cause issues with circe)
  ) ++ Seq("-rewrite", "-indent") ++ Seq("-source", "future")

lazy val `clipperz-backend` =
  project
    .in(file("."))
    .settings(name := "clipperz backend")
    .settings(commonSettings)
    .settings(dependencies)
    .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))

lazy val commonSettings = commonScalacOptions ++ Seq(
  update / evictionWarningOptions := EvictionWarningOptions.empty
)

lazy val commonScalacOptions = Seq(
  Compile / console / scalacOptions --= Seq(
    "-Wunused:_",
    "-Xfatal-warnings",
  ),
  Test / console / scalacOptions :=
    (Compile / console / scalacOptions).value,
)

val zio_version =         "2.1.11"
val zio_http_version =    "3.0.1"
val zio_logging_version = "2.3.1"
val zio_json_version =    "0.7.3"
val zio_cache_version =   "0.2.3"
val nscala_time_version = "2.34.0"
val zio_metrics_version = "2.3.1"
val zio_nio_version =     "2.0.2"

lazy val dependencies = Seq(
  libraryDependencies ++= Seq(
    "dev.zio" %% "zio"                            % zio_version,
    "dev.zio" %% "zio-streams"                    % zio_version,
    "dev.zio" %% "zio-json"                       % zio_json_version,
    "dev.zio" %% "zio-cache"                      % zio_cache_version,
    "dev.zio" %% "zio-http"                       % zio_http_version,
    "dev.zio" %% "zio-logging"                    % zio_logging_version,
    "dev.zio" %% "zio-logging-slf4j"              % zio_logging_version,
    "dev.zio" %% "zio-metrics-connectors"         % zio_metrics_version,
    "dev.zio" %% "zio-metrics-connectors-datadog" % zio_metrics_version,
    "dev.zio" %% "zio-nio"                        % zio_nio_version,

    "org.slf4j" % "slf4j-simple" % "1.7.36",
    "com.github.nscala-time" %% "nscala-time" % nscala_time_version,
  ),
  libraryDependencies ++= Seq(
    "dev.zio" %% "zio-test"     % zio_version,
    "dev.zio" %% "zio-test-sbt" % zio_version,
  ).map(_ % Test),
)

cancelable in Global := true
fork in Global := true

Compile / mainClass := Some("is.clipperz.backend.Main")

assembly / assemblyJarName := "clipperz.jar"
assembly / assemblyMergeStrategy := {
 case PathList("META-INF", _*) => MergeStrategy.discard
 case _                        => MergeStrategy.first
}
