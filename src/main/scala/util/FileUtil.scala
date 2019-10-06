package util

import scala.util.matching.Regex
import java.io.{ByteArrayOutputStream, File, FileInputStream}
import java.security.MessageDigest
import java.math.BigInteger

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



}