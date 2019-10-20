package command

import java.io.{File, PrintWriter}

import util.RepoUtil

import scala.annotation.tailrec

object Init {

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

}
