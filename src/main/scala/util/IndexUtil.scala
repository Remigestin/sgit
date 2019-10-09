package util

import command.Repo

object IndexUtil {

  def readIndexToList(): List[String] = {
    val indexPath = Repo.getIndexPath(System.getProperty("user.dir")).get
    val source = scala.io.Source.fromFile(indexPath)
    try source.getLines.toList finally source.close()
  }

  def readIndexToMap(): Map[String, String] = {
    val lines = readIndexToList()
    val paths = lines.map(s => s.split(" ")(1))
    val listSha =  lines.map(s => s.split(" ")(0))
    (paths zip listSha).toMap
  }

}

