package util

import scala.util.matching.Regex
import java.io.{ByteArrayOutputStream, File, FileInputStream}
import java.security.MessageDigest
import java.math.BigInteger

import app.Repo

import scala.annotation.tailrec

object FileUtil {


  def recursiveListFiles(f: File, r: Regex): Array[File] = {
    val these = f.listFiles
    val good = these.filter(f => r.findFirstIn(f.getName).isDefined)
    good ++ these.filter(_.isDirectory).flatMap(recursiveListFiles(_,r))
  }

  def escapeMetaCharacters(inputString: String): String = {
    val rep = inputString
    val metaCharacters = Array( "^", "$", "{", "}", "[", "]", "(", ")", ".", "*", "+", "?", "|", "<", ">", "-", "&", "%")
    metaCharacters.foreach(c =>  if (rep.contains(c)) rep.replace(c, "/" + c))
    rep
  }

  def sha1Hash(s: String): String = {

    val md = MessageDigest.getInstance("SHA1")
    val digest = md.digest(s.getBytes)
    val bigInt = new BigInteger(1,digest)
    val hashedString = bigInt.toString(16)
    hashedString
  }

  def readIndex(): List[String] = {
    val indexPath = Repo.getIndexPath(System.getProperty("user.dir")).get
    val source = scala.io.Source.fromFile(indexPath)
    try source.getLines.toList finally source.close()
  }
}