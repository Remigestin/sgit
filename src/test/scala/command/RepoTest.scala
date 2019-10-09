package command

import java.io.File

import org.scalatest._
import util.FileUtil

class RepoTest extends FlatSpec {

  it should "create .sgit dir with other dirs and file inside after the init" in {
    Repo.init()
    assert(new File(".sgit").exists())

  }

  it should "return .sgit directory path"
    Repo.init()
    assert(Repo.getSgitPath(System.getProperty("user.dir")).get == ((System.getProperty("user.dir") + File.separator + ".sgit")))
/*
  it should "escape meta character"
    assert(FileUtil.escapeMetaCharacters("*") == "/*")
*/

 */
}
