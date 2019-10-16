package util

import java.io.File

import util.FileUtil.readFileToList
import java.io.File._
import java.util.regex.Pattern

object BranchUtil {

  def getCurrentBranchName(repoPath: String): String = {
    val separatorSplit = Pattern.quote(System.getProperty("file.separator"))

    val pathBranch = getCurrentBranchPath(repoPath)
    if (new File(pathBranch).exists()) {
      pathBranch.split(separatorSplit).last
    }
    else {
      "master"
    }
  }

  def getCurrentBranchPath(repoPath: String): String = {
    val pathHead = repoPath + separator + ".sgit"  + separator + "HEAD"
    repoPath + separator + ".sgit" + separator + readFileToList(pathHead).head

  }



}
