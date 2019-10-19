package command

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Path, Paths}

import util.FileUtil._

import scala.annotation.tailrec
import scala.util.matching.Regex

object Repo {

  def init(currentPath : String): String = {

    val pathSgit = currentPath + File.separator + ".sgit"

    if (!new File(pathSgit).exists()) {

      new File(pathSgit).mkdir()

      val dirs = List("branches", "tags", "objects")
      // create the dirs ans the file HEAD
      dirs.foreach(folder => new File(pathSgit + File.separator + folder).mkdir())
      new File(pathSgit + File.separator + "HEAD").createNewFile()

      //init the file HEAD with the future branch master
      val pw = new PrintWriter(new File(pathSgit + File.separator + "HEAD"))
      pw.write("branches" + File.separator + "master")
      pw.close()

      //return success message
      "Initialized empty Sgit repository in " + pathSgit + File.separator

    } else {
      "This directory was already initialized with Sgit"
    }

  }

  @tailrec
  def getRepoPath(path: String): Option[String] = {
    if (path.isEmpty) None
    else if (new File(path + File.separator + ".sgit").exists()) {
      Some(path)
    } else {
      val parent = new File(path).getParentFile
      if (!parent.getName.isEmpty) getRepoPath(parent.getAbsolutePath)
      else getRepoPath("")
    }
  }

  def isInASgitRepo(curDir: String): Boolean = {
    getRepoPath(curDir).isDefined
  }

}
