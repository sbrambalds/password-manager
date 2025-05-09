package is.clipperz.backend.sqlite

import com.augustnagro.magnum.magzio.* 

type Key = String

class BlobRepo extends Repo[BlobDb, BlobDb, Key]

