package util

import java.io.{File, FileWriter}

import command.Repo
import util.FileUtil.sha1Hash

object SgitObjectUtil {


  /**
   *
   * @param content the content of the sgitObject that will be created
   * @return the sha of the sgit sgitObject created.
   */
  def createSgitObject(content: String): String = {
    val sha = sha1Hash(content)
    val sgitPath = Repo.getSgitPath(System.getProperty("user.dir")).get + File.separator + "objects" + File.separator + sha
    FileUtil.editFile(sgitPath, content)
    sha
  }

}
