package util

import scala.util.matching.Regex
import java.io.{ByteArrayOutputStream, File, FileInputStream}

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



}