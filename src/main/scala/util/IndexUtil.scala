package util

import java.io.File

import command.Repo
import util.FileUtil.editFile

import scala.annotation.tailrec

object IndexUtil {

  //get the index file path. If the repo is initialized but without index, the index file is created
  @tailrec
  def getIndexPath(path: String): Option[String] = {
    if (path.isEmpty)  None
    else if (new File(path + File.separator + ".sgit").exists()) {
      val indexPath = path + File.separator + ".sgit" + File.separator + "index"
      editFile(indexPath, "")
      Some(new File(indexPath).getAbsolutePath)

    } else {
      val parent = new File(path).getParentFile
      if (!parent.getName.isEmpty) getIndexPath(parent.getAbsolutePath)
      else getIndexPath("")
    }
  }

  def readIndexToList(): List[String] = {
    val indexPath = getIndexPath(System.getProperty("user.dir")).get
    val source = scala.io.Source.fromFile(indexPath)
    try source.getLines.toList finally source.close()
  }

  def readIndexToMap(): Map[String, String] = {
    val lines = readIndexToList()
    val paths = lines.map(s => s.split(" ")(1))
    val listSha =  lines.map(s => s.split(" ")(0))
    (paths zip listSha).toMap
  }

  def isIndexCreated(currentDir: String): Boolean = {
    new File(Repo.getSgitPath(currentDir) + File.separator + "index").exists()
  }

}

