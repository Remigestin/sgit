package command

import java.io.File

import command.Log.{getAllCommits, getLogResult}
import util.{BranchUtil, CommitUtil, SgitObjectUtil}

import scala.annotation.tailrec
import util.SgitObjectUtil._

object Log {

  def log(repoPath: String): String = {


    //read IO -> list of all the commits
    if (CommitUtil.isThereACommit(repoPath)) {
      val branchName = BranchUtil.getCurrentBranchName(repoPath)
      val shaLastCommit = CommitUtil.getLastCommitObject(repoPath, branchName)
      val listAllCommits = getAllCommits(repoPath, shaLastCommit)


      //recover the result of a log
      "branch " + branchName + "\n\n" + getLogResult(repoPath, listAllCommits)
    } else {
      "there is no commit"
    }
  }

  def logOption(repoPath: String, option: String): String = {


    //read IO -> list of all the commits
    if (CommitUtil.isThereACommit(repoPath)) {
      val branchName = BranchUtil.getCurrentBranchName(repoPath)
      val shaLastCommit = CommitUtil.getLastCommitObject(repoPath, branchName)
      val listAllCommits = getAllCommits(repoPath, shaLastCommit)

      if (option == "patch") {
        "branch " + branchName + "\n\n" + getLogOption(repoPath, listAllCommits, Diff.getDiffAllFiles)
      } else {
        "branch " + branchName + "\n\n" + getLogOption(repoPath, listAllCommits, Diff.getDiffStatAllFiles)
      }

      //recover the result of a log

    } else {
      "there is no commit"
    }
  }


  /**
   *
   * @param repoPath   : path of the Repo
   * @param listCommit : List with all the commits, the pattern for each commit is is (shaCommit, contentCommit). The list has the more recent commit in first.
   * @return the result of sgit log in String : for each commit his sha and his message
   */
  def getLogResult(repoPath: String, listCommit: List[(String, String)]): String = {

    @tailrec
    def loop(listCurrent: List[(String, String)], result: String): String = {

      listCurrent match {
        case Nil => result
        case head :: tail =>

          //recover the message of the commit
          val messageCommit = head._2.split("\n\n")(1)

          //create the string for a commit
          val stringCommit = Console.YELLOW + "commit " + head._1 + Console.RESET + "\n\n      " + messageCommit + "\n\n"

          //update the result of the sgit diff
          val resultUpdated = stringCommit + result

          //recursion
          loop(tail, resultUpdated)
      }
    }

    loop(listCommit, "")
  }

  /**
   *
   * @param repoPath   : the path of the repo
   * @param listCommit : list of all the tuples representing the commits with the pattern (sha, content)
   * @param option     : function of diff to apply to the commits
   * @return the string to print in the terminal of the diff of the listCommit for the option asked.
   */
  def getLogOption(repoPath: String, listCommit: List[(String, String)], option: (List[(String, String, String)], String) => String): String = {

    @tailrec
    def loop(listCurrent: List[(String, String)], result: String): String = {

      listCurrent match {
        case Nil => result
        case head :: tail =>

          //recover the message of the commit
          val messageCommit = head._2.split("\n\n")(1)

          val mapCommitCurrent = CommitUtil.getMapOfCommit(repoPath, head._1)

          val contentCommitList = head._2.split("\n")

          if (contentCommitList(1).split(" ")(0) == "Parent") {
            val shaCommitParent = contentCommitList(1).split(" ")(1)

            val mapCommitParent = CommitUtil.getMapOfCommit(repoPath, shaCommitParent)

            val listTuplesToCompare = getListTuples(mapCommitCurrent, mapCommitParent)
              .map(tuple => (getPathSgitObject(repoPath, tuple._1), getPathSgitObject(repoPath, tuple._2), tuple._3))


            val stringAllDiff = option(listTuplesToCompare, repoPath)

            //create the string for a commit
            val stringCommit = Console.YELLOW + "commit " + head._1 + Console.RESET + "\n\n      " + messageCommit + "\n\nDiffs : " + "\n" + stringAllDiff + "\n\n"

            //update the result of the sgit diff
            val resultUpdated = stringCommit + result

            loop(tail, resultUpdated)

          }

          else {


            val mapEmpty = Map(("", "")).withDefaultValue("")

            val listTuplesToCompare = getListTuples(mapCommitCurrent, mapEmpty)
              .map(tuple => (getPathSgitObject(repoPath, tuple._1), getPathSgitObject(repoPath, tuple._2), tuple._3))

            val stringAllDiff = option(listTuplesToCompare, repoPath)

            //create the string for a commit
            val stringCommit = Console.YELLOW + "commit " + head._1 + Console.RESET + "\n\n      " + messageCommit + "\n\nDiffs : " + "\n" + stringAllDiff + "\n"

            //update the result of the sgit diff
            val resultUpdated = stringCommit + result

            //recursion
            loop(tail, resultUpdated)
          }


      }
    }

    loop(listCommit, "")
  }

  /**
   *
   * @param repoPath   : the path of the repo
   * @param lastCommit : the sha of the last commit to check
   * @return a list of tuples with the pattern (sha, content)
   */
  def getAllCommits(repoPath: String, lastCommit: String): List[(String, String)] = {

    @tailrec
    def loop(result: List[(String, String)], shaCurrentCommit: String): List[(String, String)] = {

      val contentCommitList = SgitObjectUtil.readSgitObjectToList(repoPath, shaCurrentCommit)
      val content = contentCommitList mkString "\n"

      val resultUpdated = (shaCurrentCommit, content) :: result

      if (contentCommitList(1).split(" ")(0) != "Parent") {
        resultUpdated
      }
      else {
        val shaParent = contentCommitList(1).split(" ")(1)
        loop(resultUpdated, shaParent)
      }
    }

    loop(List(), lastCommit)
  }

  /**
   *
   * @param mapNew : map of the new commits to diff with the pattern (path -> sha)
   * @param mapOld : map of the old commits to diff with the pattern (path -> sha)
   * @return a list of tuples representing the diff to effectuate wit the pattern (newSha, oldSha, pathFile)
   */
  def getListTuples(mapNew: Map[String, String], mapOld: Map[String, String]): List[(String, String, String)] = {

    @tailrec
    def loop(listCurrent: List[(String, String, String)], mapNewCurrent: Map[String, String]): List[(String, String, String)] = {
      if (mapNewCurrent.isEmpty) {

        val deletedFiles = mapOld.keys.toList diff mapNew.keys.toList
        val deletedTuples = deletedFiles.map(src => ("", mapOld(src), src))
        listCurrent ++ deletedTuples

      } else {

        val tuple = mapNewCurrent.head
        val newBlob = tuple._2
        val oldBlob = mapOld(tuple._1)

        val listUpdated = (newBlob, oldBlob, tuple._1) :: listCurrent
        loop(listUpdated, mapNewCurrent.tail)
      }
    }

    loop(List(), mapNew)
  }

}
