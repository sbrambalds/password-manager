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

lazy val createDbDirectories = TaskKey[Unit]("createDbDirectories", "Create database directories")
createDbDirectories := {
  val dir = baseDirectory.value / "target" / "db"
  if (!dir.exists()) {
    IO.createDirectory(dir)
  }
}

lazy val runCollector = TaskKey[Unit]("runCollector", "Start otel collector")
runCollector := {
  import sys.process.*
  "npm run run-collector" !
}

lazy val createFSDirectories = TaskKey[Unit]("createFSDirectories", "Create blobs directories")
createFSDirectories := {
  val blob = baseDirectory.value / "target" / "archive" / "blobs"
  val oneTimeShare = baseDirectory.value / "target" / "archive" / "one_time_share"
  val user = baseDirectory.value / "target" / "archive" / "users"
  if (!blob.exists()) {
    IO.createDirectory(blob)
  }
  if (!oneTimeShare.exists()) {
    IO.createDirectory(oneTimeShare)
  }
  if (!user.exists()) {
    IO.createDirectory(user)
  }
}

//=====================================================================

ThisBuild / organization := "is.clipperz"
ThisBuild / scalaVersion := "3.5.1"
ThisBuild / version      := "0.0.1"

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

val zio_version =                "2.1.12"
val zio_http_version =           "3.4.0"
val zio_logging_version =        "2.3.1"
val zio_json_version =           "0.7.3"
val zio_cache_version =          "0.2.3"
val zio_metrics_version =        "2.3.1"
val zio_nio_version =            "2.0.2"
val zio_opentelemetry_version =  "3.1.7"
val zio_S3_version =             "0.4.4"

val nscala_time_version =        "2.34.0"

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
    "dev.zio" %% "zio-opentelemetry"              % zio_opentelemetry_version,
    "dev.zio" %% "zio-opentelemetry-zio-logging"  % zio_opentelemetry_version,
    "dev.zio" %% "zio-s3"                         % zio_S3_version,
    
    "com.github.nscala-time"   %% "nscala-time"                 % nscala_time_version,

    "io.opentelemetry"         %  "opentelemetry-sdk"           % "1.44.1",
    "io.opentelemetry"         %  "opentelemetry-exporter-otlp" % "1.44.1",
    "io.opentelemetry.semconv" %  "opentelemetry-semconv"       % "1.28.0-alpha",
    "org.slf4j"                %  "slf4j-simple"                % "1.7.36",

    "com.augustnagro" %% "magnumzio"  % "2.0.0-M1",
    "org.xerial"      % "sqlite-jdbc" % "3.49.1.0",
    "com.zaxxer"      % "HikariCP"    % "6.3.0",
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
