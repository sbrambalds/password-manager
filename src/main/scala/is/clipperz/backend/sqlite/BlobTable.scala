package is.clipperz.backend.sqlite

import com.augustnagro.magnum.magzio.* 
import zio.stream.ZStream
import org.sqlite.SQLiteDataSource
import java.sql.Blob

@Table(SqliteDbType)
case class BlobDb(
   @Id hash: Key,
   metadata: String,
   blob: Blob
) derives DbCodec
