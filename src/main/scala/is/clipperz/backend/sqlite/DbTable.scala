package is.clipperz.backend.sqlite

import com.augustnagro.magnum.magzio.* 
import zio.ZIO
import is.clipperz.backend.Exceptions.ResourceNotFoundException

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
   blob: Array[Byte],
) extends DbTable derives DbCodec

extension [T <: DbTable](repo: Repo[T, T, Key])
   def createTable(transactor: Transactor) =
      repo match
         case _: BlobRepo =>       
            transactor.transact:
               sql"create table if not exists BlobDb (hash text primary key, content text, blob blob);".update.run()
         case _: UserRepo =>       
            transactor.transact:
               sql"create table if not exists UserDb (hash text primary key, content text, blob blob);".update.run()
         case _: OneTimeShareRepo =>       
            transactor.transact:
               sql"create table if not exists OneTimeShareDb (hash text primary key, content text, blob blob);".update.run()

class UserRepo extends Repo[UserDb, UserDb, Key]

class OneTimeShareRepo extends Repo[OneTimeShareDb, OneTimeShareDb, Key]

class BlobRepo extends Repo[BlobDb, BlobDb, Key]
