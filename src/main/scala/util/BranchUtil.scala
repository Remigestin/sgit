package util

import java.io.File

import util.FileUtil.readFileToList
import java.io.File._
import java.util.regex.Pattern

import scala.annotation.tailrec

class BranchItem(val name: String, val shaCommit: String, val commitMsg: String)

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
   * @return the list of all the branches with this pattern for each branch : (name, shaCommit, commitMsg)
   */
  def getAllBranchesOrTagItem(repoPath: String, nameToGet: String): List[BranchItem] = {

    val pathItems = repoPath + separator + ".sgit" + separator + nameToGet

    @tailrec
    def loop(result: List[BranchItem], listBranchesCurrent: List[File]): List[BranchItem] = {

      listBranchesCurrent match {
        case Nil => result
        case head :: tail =>

          val name = head.getName
          val shaCommit = FileUtil.readFileToList(pathItems + separator + name).head
          val contentCommit = SgitObjectUtil.readSgitObjectToList(repoPath, shaCommit)  mkString "\n"
          val messageCommit = contentCommit.split("\n\n").tail mkString "\n"

          val resultUpdated = new BranchItem(name,shaCommit,messageCommit) :: result

          loop(resultUpdated, tail)
      }

    }


    loop(List(), new File(pathItems).listFiles().toList)









  }



}
