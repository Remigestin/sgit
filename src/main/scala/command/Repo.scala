package command

import java.io.{File, PrintWriter}

import scala.annotation.tailrec

object Repo {

  /**
   *
   * @param path : the path were to init the sgit repo
   * @return the success or fail message of the init command
   */
  def init(path: String): String = {

    val pathSgit = path + File.separator + ".sgit"

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

  /**
   *
   * @param path : the path were the sgit repo is asked
   * @return the path of the sgit repo of the path in param
   */
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

  /**
   *
   * @param curDir : the dir in question
   * @return if the curDir param is in a sgit repo
   */
  def isInASgitRepo(curDir: String): Boolean = {
    getRepoPath(curDir).isDefined
  }

}
