package app

import java.io.File

object Repo {

  def init(): String = {
    val dirs = List(".sgit", ".sgit/branches", ".sgit/tags", ".sgit/commits", ".sgit/trees", ".sgit/blobs")
    dirs.foreach(folder => new File(folder).mkdir())
    new File(".sgit/HEAD").createNewFile()
    "Initialized empty Sgit repository in " + System.getProperty("user.dir") + File.separator + ".sgit" + File.separator
  }



}
