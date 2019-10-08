package app

import java.io.File
import java.util.regex.Pattern

import util.FileUtil



object Commit {

  def commit(): String = {

    //get all the lines of the index files
    val listIndex = FileUtil.readIndex()

    //keep just the list of the paths
    val listPath = listIndex.map(l => l.split(" ")(1))

    //sort the list by the greatest number of directories in each path
    val separator = Pattern.quote(System.getProperty("file.separator"))
    val listSorted = listPath.sortBy(f => f.split(separator).length).reverse






    ""

  }


}
