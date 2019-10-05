package app

import java.io.{File, PrintWriter}
import scala.annotation.tailrec

object Repo {

  def init(): String = {

    if (!new File(".sgit/").exists()) {
      val dirs = List(".sgit", ".sgit/branches", ".sgit/tags", ".sgit/commits", ".sgit/trees", ".sgit/blobs")
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
      "This directory was already initialized"
    }

  }

  //TODO
  def add(regex: String): String = {

    if (!new File(".sgit/index").exists()) {
      new File(".sgit/index").createNewFile()
    }

    "return"
  }

  @tailrec
  def getSgitPath(path: String): Option[String] = {
    if (path == null) {
      None
    }
    else if (new File(path + File.separator + ".sgit/").exists()) {
      Some(new File(path + File.separator + ".sgit/").getAbsolutePath)
    } else {
      getSgitPath(new File(".").getParentFile.getAbsolutePath)
    }
  }


}
