import Util.styled

addCommandAlias(
  "buildAll", 
  "installPurescript; buildPurescript; compile"
)
addCommandAlias(
  "runAllFileSystem", 
  "installPurescript; buildPurescript; packagePurescript; createFSDirectories; run \"fileSystem\" \"8090\" \"./target/archive/blobs\" \"./target/archive/users\" \"./target/archive/one_time_share\""
)
addCommandAlias(
  "runAllS3", 
  "installPurescript; buildPurescript; packagePurescript; createFSDirectories; run \"s3\" \"8090\""
)
addCommandAlias(
  "runAllFileSystem",
  "installPurescript; buildPurescript; packagePurescript; createFSDirectories; run \"fileSystem\" \"8090\" \"./target/archive/blobs\" \"./target/archive/users\" \"./target/archive/one_time_share\"",
)
addCommandAlias(
  "runAllDb",
  "installPurescript; buildPurescript; packagePurescript; createDbDirectories; run \"db\" \"8090\" \"target/db/\"",
)
addCommandAlias(
  "cleanAll",
  "clean; cleanDependenciesPurescript",
)
addCommandAlias(
  "cleanArchive",
  "cleanTargetSubdir archive",
)
addCommandAlias(
  "testPurescript",
  "installPurescript; buildPurescript; runTestPurescript",
)
addCommandAlias("testAll", "test; testPurescript")

addCommandAlias("b", "buildPurescript")
addCommandAlias("c", "compile")
addCommandAlias("t", "testAll")
addCommandAlias("r", "runAllFileSystem")
addCommandAlias("rfs", "runAllFileSystem")
addCommandAlias("rdb", "runAllDb")
addCommandAlias("rS3", "runAllS3")

addCommandAlias(
  "styleCheck",
  "scalafmtSbtCheck; scalafmtCheckAll",
)
addCommandAlias(
  "styleFix",
  "scalafmtSbt; scalafmtAll",
)
addCommandAlias(
  "up2date",
  "reload plugins; dependencyUpdates; reload return; dependencyUpdates",
)

onLoadMessage +=
  s"""|
      |╭───────────────────────────────────────╮
      |│     List of defined ${styled("aliases")}           │
      |├────────────────┬──────────────────────┤
      |│ ${styled("buildAll")}       │ build all            │
      |│ ${styled("cleanAll")}       │ clean all            │
      |│ ${styled("cleanArchive")}   │ clean target/archive │
      |│ ${styled("b")}              │ build purescript     │
      |│ ${styled("c")}              │ compile scala        │
      |│ ${styled("r")}              │ run all              │
      |│ ${styled("t")}              │ test all             │
      |│ ${styled("testPurescript")} │ test purescript      │
      |│ ${styled("styleCheck")}     │ fmt check            │
      |│ ${styled("styleFix")}       │ fmt                  │
      |│ ${styled("up2date")}        │ dependencyUpdates    │
      |╰────────────────┴──────────────────────╯""".stripMargin
