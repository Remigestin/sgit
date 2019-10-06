package app

import java.io.File

import util.FileUtil

import scala.annotation.tailrec

object add {

  val curDir: String = System.getProperty("user.dir")

  //TODO
  def add(files: Seq[File]): String = {

    if (!new File(".sgit/index").exists()) {
      new File(".sgit/index").createNewFile()
    }

    //For each pattern after the sgit add, we retrieve the path of all the corresponding files
    val pathList = getListPaths(files)
    pathList.foreach(p => println(p))

    //Create Blob
    val ha = FileUtil.sha1Hash(System.getProperty("user.dir"))
    println(ha)


    //Edit index file

    "return"
  }

  //TODO : regex
  //For each pattern after the sgit add, we get the path of all the corresponding files
  def getListPaths(files: Seq[File]): List[String] = {
    @tailrec
    def loop(listPath: List[String], f: Seq[File]): List[String] = {
      f match {
        case Nil => listPath
        case head :: tail =>
          val list = FileUtil.recursiveListFiles(new File(curDir), head.getName.r).filter(a => a.isFile).map(a => a.getAbsolutePath)
          loop(listPath ++ list, tail)
      }
    }
    loop(List[String](), files)
  }

}
