package is.clipperz.backend.storage

import zio.nio.file.Path

/** Canonical archive identifiers, shared by every storage backend so that the
  * `archive` label on the `files.count` / `files.size` metrics is the same
  * regardless of the backend in use (file system, sqlite, s3/minio).
  *
  * The values must be valid S3 bucket names (lowercase, no underscores).
  */
object ArchiveName:
    val blobs: String         = "blobs"
    val users: String         = "users"
    val oneTimeShares: String  = "one-time-shares"

    /** Maps the base folder name of a file-system archive (which is defined by
      * the CLI arguments / sbt aliases and may use legacy spellings) to its
      * canonical name. Unknown names are returned unchanged.
      */
    private val fsAliases: Map[String, String] = Map(
        "blobs"           -> blobs,
        "users"           -> users,
        "one_time_share"  -> oneTimeShares,
        "one_time_shares" -> oneTimeShares,
        "one-time-shares" -> oneTimeShares,
        "shares"          -> oneTimeShares,
    )

    def fromFsPath(path: Path): String =
        val name = path.filename.toFile.toString
        fsAliases.getOrElse(name, name)
