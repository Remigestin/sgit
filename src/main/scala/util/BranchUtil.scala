package util

import java.io.File

import util.FileUtil.readFileToList
import java.io.File._
import java.util.regex.Pattern

import scala.annotation.tailrec

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

  /**
   *
   * @param repoPath : path of the sgit repo
   * @return the list of all the branches with this pattern for each branch : (name, shaCommit, contentCommit)
   */
  def getAllBranches(repoPath: String): List[(String, String, String)] = {

    val pathBranches = repoPath + separator + ".sgit" + separator + "branches"

    @tailrec
    def loop(result: List[(String, String, String)], listBranchesCurrent: List[File]): List[(String, String, String)] = {

      listBranchesCurrent match {
        case Nil => result
        case head :: tail =>

          val name = head.getName
          val shaCommit = FileUtil.readFileToList(pathBranches + separator + name).head
          val contentCommit = SgitObjectUtil.readSgitObjectToList(repoPath, shaCommit)  mkString "\n"

          val resultUpdated = (name, shaCommit, contentCommit) :: result

          loop(resultUpdated, tail)
      }

    }


    loop(List(), new File(pathBranches).listFiles().toList)









  }



}
