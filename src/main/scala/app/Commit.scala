package app

import java.io.File
import java.util.regex.Pattern



object Commit {

  def commit(): String = {

    //get all the lines of the index files
    val listIndex = readIndex()

    //keep just the list of the paths
    val listPath = listIndex.map(l => l.split(" ")(1))

    //sort the list by the greatest number of directories in each path
    val separator = Pattern.quote(System.getProperty("file.separator"))
    val listSorted = listPath.sortBy(f => f.split(separator).length).reverse






    ""

  }

  def readIndex(): List[String] = {
    val sgitPath = Repo.getSgitPath(System.getProperty("user.dir")).get
    val indexPath = sgitPath + File.separator + "index"
    val source = scala.io.Source.fromFile(indexPath)
    try source.getLines.toList finally source.close()
  }
}
