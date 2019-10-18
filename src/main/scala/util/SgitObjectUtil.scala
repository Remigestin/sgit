package util

import java.io.{File, FileWriter}

import command.Repo
import util.FileUtil._

object SgitObjectUtil {


  /**
   *
   * @param content the content of the sgitObject that will be created
   * @return the sha of the sgit sgitObject created.
   */
  def createSgitObject(repoPath: String, content: String): String = {
    val sha = sha1Hash(content)
    val sgitPath = getPathSgitObject(repoPath, sha)
    editFile(sgitPath, content, append = false)
    sha
  }

  def readSgitObjectToList(repoPath: String, sha: String): List[String] = {
    readFileToList(getPathSgitObject(repoPath, sha))
  }

  def getPathSgitObject(repoPath: String, sha:String): String = {
    repoPath + File.separator + ".sgit" + File.separator + "objects" + File.separator + sha
  }

}
