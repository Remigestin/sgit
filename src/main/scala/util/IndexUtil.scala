package util

import java.io.File

import util.FileUtil.editFile

object IndexUtil {


  /**
   *
   * @param repoPath : the path of the sgit repo
   * @return the path of the index file. If the repo is initialized but without index, the index file is created
   */
  def getIndexPath(repoPath: String): String = {
    val indexPath = repoPath + File.separator + ".sgit" + File.separator + "index"
    if (!new File(indexPath).exists()) {
      editFile(indexPath, "", append = false)
    }
    indexPath
  }

  /**
   *
   * @param repoPath : the path of the sgit repo
   * @return the content of the index file in a list of string
   */
  def readIndexToList(repoPath: String): List[String] = {
    val indexPath = getIndexPath(repoPath)
    FileUtil.readFileToList(indexPath)
  }

  /**
   *
   * @param repoPath : the path of the sgit repo
   * @return the content of the index file in a map (src -> sha)
   */
  def readIndexToMap(repoPath: String): Map[String, String] = {
    val lines = readIndexToList(repoPath)
    val paths = lines.map(s => s.split(" ")(1))
    val listSha = lines.map(s => s.split(" ")(0))
    (paths zip listSha).toMap
  }

  /**
   *
   * @param repoPath : the sgit repo
   * @return if the index file is created
   */
  def isIndexCreated(repoPath: String): Boolean = {
    new File(repoPath + File.separator + ".sgit" + File.separator + "index").exists()
  }

}

