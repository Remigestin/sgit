package util

import java.io.File

import util.FileUtil.readFileToList
import java.io.File._
import java.util.regex.Pattern

import scala.annotation.tailrec

class BranchItem(val name: String, val shaCommit: String, val commitMsg: String) {
  override def toString: String = {
    name + " "+ shaCommit.slice(0,7) + " " + commitMsg +"\n"
  }
}

object BranchUtil {

  /**
   *
   * @param repoPath : the path of the sgit repo
   * @return the name of the current branch, the default value is master
   */
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

  /**
   *
   * @param repoPath : the path of the sgit repo
   * @return the path of the current branch file
   */
  def getCurrentBranchPath(repoPath: String): String = {
    val pathHead = repoPath + separator + ".sgit"  + separator + "HEAD"
    repoPath + separator + ".sgit" + separator + readFileToList(pathHead).head

  }


  /**
   *
   * @param repoPath : the path of the sgit repo
   * @param nameToGet : the type of items that we want (tags or branches)
   * @return the list of items which represent all the branches or all the tags
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
