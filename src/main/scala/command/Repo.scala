package command

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Path, Paths}

import util.FileUtil._

import scala.annotation.tailrec
import scala.util.matching.Regex

object Repo {

  def init(): String = {

    if (!new File(".sgit/").exists()) {
      val dirs = List(".sgit", ".sgit/branches", ".sgit/tags", ".sgit/objects")
      // create the dirs ans the file HEAD
      dirs.foreach(folder => new File(folder).mkdir())
      new File(".sgit/HEAD").createNewFile()

      //init the file HEAD with the future branch master
      val pw = new PrintWriter(new File(".sgit/HEAD"))
      pw.write("branches/master")
      pw.close()

      //return success message
      "Initialized empty Sgit repository in " + System.getProperty("user.dir") + File.separator + ".sgit" + File.separator

    } else {
      "This directory was already initialized with Sgit"
    }

  }


  @tailrec
  def getSgitPath(path: String): Option[String] = {
    if (path.isEmpty)  None
    else if (new File(path + File.separator + ".sgit").exists()) {
      Some(new File(path + File.separator + ".sgit").getAbsolutePath)
    } else {
      val parent = new File(path).getParentFile
      if (!parent.getName.isEmpty) getSgitPath(parent.getAbsolutePath)
      else getSgitPath("")
    }
  }

  @tailrec
  def getRepoPath(path: String): Option[String] = {
    if (path.isEmpty) None
    else if (new File(path + File.separator + ".sgit").exists()) {
      Some(new File(path).getAbsolutePath)
    } else {
      val parent = new File(path).getParentFile
      if (!parent.getName.isEmpty) getRepoPath(parent.getAbsolutePath)
      else getRepoPath("")
    }
  }


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



  def isInASgitRepo: Boolean = {
    Repo.getRepoPath(System.getProperty("user.dir")).isDefined
  }

}
