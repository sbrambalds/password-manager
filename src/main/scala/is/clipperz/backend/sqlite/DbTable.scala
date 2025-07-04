package is.clipperz.backend.sqlite

import com.augustnagro.magnum.magzio.* 

type Key = String

trait DbTable:
   def hash: Key
   def content: String
   def blob: Array[Byte]

@Table(SqliteDbType)
case class UserDb(
   @Id hash: Key,
   content: String = "",
   blob: Array[Byte]
) extends DbTable derives DbCodec

@Table(SqliteDbType)
case class OneTimeShareDb(
   @Id hash: Key,
   content: String = "",
   blob: Array[Byte]
) extends DbTable derives DbCodec

@Table(SqliteDbType)
case class BlobDb(
   @Id hash: Key,
   content: String = "",
   blob: Array[Byte]
) extends DbTable derives DbCodec

class BlobRepo extends Repo[BlobDb, BlobDb, Key]

class UserRepo extends Repo[UserDb, UserDb, Key]

class OneTimeShareRepo extends Repo[OneTimeShareDb, OneTimeShareDb, Key]

