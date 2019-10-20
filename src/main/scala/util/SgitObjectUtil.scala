package util

import java.io.File

import util.FileUtil._

object SgitObjectUtil {


  /**
   *
   * @param repoPath : the path of the sgit repo
   * @param content  : the content of the sgitObject that will be created
   * @return the sha of the sgit sgitObject created.
   */
  def createSgitObject(repoPath: String, content: String): String = {
    val sha = sha1Hash(content)
    val sgitPath = getPathSgitObject(repoPath, sha)
    editFile(sgitPath, content, append = false)
    sha
  }

  /**
   *
   * @param repoPath : the path of the sgit repo
   * @param sha      : the sha of the file asked to read
   * @return the content of the sgit object in param in a list of string
   */
  def readSgitObjectToList(repoPath: String, sha: String): List[String] = {
    readFileToList(getPathSgitObject(repoPath, sha))
  }

  /**
   *
   * @param repoPath : the path of the sgit repo
   * @param sha      : the sha of the sgit object
   * @return the path of the sgit object asked
   */
  def getPathSgitObject(repoPath: String, sha: String): String = {
    repoPath + File.separator + ".sgit" + File.separator + "objects" + File.separator + sha
  }

}
