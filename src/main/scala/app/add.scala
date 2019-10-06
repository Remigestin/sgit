package app

import java.io.File

import util.FileUtil

object add {

  //TODO
  def add(files: Seq[File]): String = {

    if (!new File(".sgit/index").exists()) {
      new File(".sgit/index").createNewFile()
    }

    val pathList = List[String]()

    //For each regex after the sgit add, we get the path of all the corresponding files
    //TODO : regex
    files.foreach(file => FileUtil.recursiveListFiles(new File(System.getProperty("user.dir")),FileUtil.escapeMetaCharacters(file.getName).r)
      .filter(f => f.isFile)
      .foreach(fi => pathList ++ fi.getAbsolutePath))

    println(pathList.foreach(p => p))

    //Create Blob
    val md = java.security.MessageDigest.getInstance("SHA-1")
    val ha = new sun.misc.BASE64Encoder().encode(md.digest("sdfqsdfqsdfqsfd".getBytes))
    println(ha)


    //Edit index file






    "return"
  }

}
