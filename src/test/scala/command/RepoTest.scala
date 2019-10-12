package command

import java.io.File

import org.scalatest._
import util.FileUtil

class RepoTest extends FlatSpec {


  it should "create .sgit dir with other dirs and file inside after the init" in {
    Repo.init(System.getProperty("user.dir"))
    assert(new File(".sgit").exists())

  }

/*
  it should "escape meta character"
    assert(FileUtil.escapeMetaCharacters("*") == "/*")
*/

 */
}
