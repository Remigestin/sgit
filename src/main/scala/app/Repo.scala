package app

import java.io.{File, PrintWriter}

object Repo {

  def init(): String = {
    val dirs = List(".sgit", ".sgit/branches", ".sgit/tags", ".sgit/commits", ".sgit/trees", ".sgit/blobs")
    // create the dirs ans the file HEAD
    dirs.foreach(folder => new File(folder).mkdir())
    new File(".sgit/HEAD").createNewFile()

    //init the file HEAD with the future branch master
    val pw = new PrintWriter(new File(".sgit/HEAD" ))
    pw.write("branches/master")
    pw.close()

    //return success message
    "Initialized empty Sgit repository in " + System.getProperty("user.dir") + File.separator + ".sgit" + File.separator
  }



}
