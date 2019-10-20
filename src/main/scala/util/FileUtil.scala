package util

import java.io.FileWriter
import java.math.BigInteger
import java.security.MessageDigest

object FileUtil {


  import java.io.File

  /**
   *
   * @param f : the directory asked
   * @return recursively all the files of the directory in param
   */
  def recursiveListFiles(f: File): Array[File] = {
    f.getName match {
      case "." =>
        val these = f.listFiles
        val theseClean = these.map(file => new File(file.getPath.slice(2, file.getPath.length)))
        theseClean ++ theseClean.filter(_.isDirectory).flatMap(recursiveListFiles _)
      case _ =>
        val these = f.listFiles
        these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles _)
    }
  }

  /**
   *
   * @param s : the string to hash
   * @return : the sha1 of the string in param
   */
  def sha1Hash(s: String): String = {

    val md = MessageDigest.getInstance("SHA1")
    val digest = md.digest(s.getBytes)
    val bigInt = new BigInteger(1, digest)
    val hashedString = bigInt.toString(16)
    hashedString
  }

  /**
   *
   * @param path    : the path of the file
   * @param content : the content to edited in the file
   * @param append  : append the string in the file
   *                edit the file passed in the path param with the content param. Create the file if it's not.
   */
  def editFile(path: String, content: String, append: Boolean): Unit = {

    if (!new File(path).exists()) {
      //create the file
      new File(path).createNewFile()
    }

    //fill the file
    val fw = new FileWriter(path, append)
    fw.write(content)
    fw.close()

  }


  /**
   *
   * @param path : the path of the file
   * @return the content of the file in param in a list of string. return an empty list if the file does not exist
   */
  def readFileToList(path: String): List[String] = {

    if (new File(path).exists && new File(path).isFile) {
      val source = scala.io.Source.fromFile(path)
      try source.getLines.toList finally source.close()
    }
    else {
      List()
    }
  }

}