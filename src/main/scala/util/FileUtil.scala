package util

import scala.util.matching.Regex
import java.io.{ByteArrayOutputStream, File, FileInputStream, FileWriter}
import java.security.MessageDigest
import java.math.BigInteger

import command.Repo

import scala.annotation.tailrec

object FileUtil {


  import java.io.File

  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles _)
  }

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
   *                edit the object passed in the path param with the content param. Create the file if it's not.
   */
  def editFile(path: String, content: String, append: Boolean): Unit = {

    if (!new File(path).exists()) {
      //create the file
      new File(path).createNewFile()
    }
    if (!content.isEmpty) {
      //fill the file
      val fw = new FileWriter(path, append)
      fw.write(content)
      fw.close()
    }
  }


  def readFileToList(path: String): List[String] = {
    val source = scala.io.Source.fromFile(path)
    try source.getLines.toList finally source.close()
  }

}