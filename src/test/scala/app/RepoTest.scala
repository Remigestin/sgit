package app

import java.io.File
import org.scalatest._

class RepoTest extends FlatSpec {

  it should "create .sgit dir with other dirs and file inside after the init" in {
    Repo.init()
    assert(new File(".sgit").exists())

  }

}
