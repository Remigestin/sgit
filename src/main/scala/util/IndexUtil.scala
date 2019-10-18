package util

import java.io.File

import command.Repo
import util.FileUtil.editFile

import scala.annotation.tailrec

object IndexUtil {

  //get the index file path. If the repo is initialized but without index, the index file is created

  def getIndexPath(repoPath: String): String = {
    val indexPath = repoPath + File.separator + ".sgit" + File.separator + "index"
    if (!new File(indexPath).exists()) {
      editFile(indexPath, "", append=false)
    }
    indexPath
  }

  def readIndexToList(repoPath: String): List[String] = {
    val indexPath = getIndexPath(repoPath)
    FileUtil.readFileToList(indexPath)
  }

  def readIndexToMap(repoPath: String): Map[String, String] = {
    val lines = readIndexToList(repoPath)
    val paths = lines.map(s => s.split(" ")(1))
    val listSha =  lines.map(s => s.split(" ")(0))
    (paths zip listSha).toMap
  }

  def isIndexCreated(repoPath: String): Boolean = {
    new File(repoPath + File.separator + ".sgit" + File.separator + "index").exists()
  }

}

