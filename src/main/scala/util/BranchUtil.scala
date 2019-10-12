package util

import util.FileUtil.readFileToList
import java.io.File._
import java.util.regex.Pattern

object BranchUtil {

  def getCurrentBranchName(repoPath: String): String = {
    val separatorSplit = Pattern.quote(System.getProperty("file.separator"))
    val pathBranch = getCurrentBranchPath(repoPath)
    pathBranch.split(separatorSplit).last
  }

  def getCurrentBranchPath(repoPath: String): String = {
    val pathHead = repoPath + separator + ".sgit"  + separator + "HEAD"
    repoPath + separator + ".sgit" + separator + readFileToList(pathHead).head
  }



}
