package command

import util.{BranchUtil, CommitUtil, SgitObjectUtil}

import scala.annotation.tailrec

class CommitToDiff(val sha: String, val message: String, val listFilesToDiff: List[FilesToDiff])

object Log {


  /**
   *
   * @param repoPath : the path of the sgit repo
   * @param option   : the option of the log command ("" or "patch" or "stat")
   * @return the message of the log command
   */
  def log(repoPath: String, option: String): String = {


    //---------------------- IO READING STEP
    if (CommitUtil.isThereACommit(repoPath)) {
      val branchName = BranchUtil.getCurrentBranchName(repoPath)
      val shaLastCommit = CommitUtil.getLastCommitObject(repoPath, branchName).get
      val listAllCommits = getAllCommits(repoPath, shaLastCommit)

      //------------------ PURE FUNCTIONAL STEP

      val listAllCommitsToDiff = getListFilesToDiffAllCommits(repoPath, listAllCommits)

      option match {
        //log -p
        case "patch" => "branch " + branchName + "\n\n" + getLogResult(repoPath, listAllCommitsToDiff, Some(Diff.getDiffAllFiles))

        //log --stat
        case "stat" => "branch " + branchName + "\n\n" + getLogResult(repoPath, listAllCommitsToDiff, Some(Diff.getDiffStatAllFiles))

        //log
        case _ => "branch " + branchName + "\n\n" + getLogResult(repoPath, listAllCommitsToDiff, None)
      }

    } else {
      "there is no commit"
    }
  }


  /**
   *
   * @param repoPath         : the path of the repo
   * @param listCommitToDiff : list of all the commit to diff
   * @param option           : function of diff to apply to the commitToDiff
   * @return the string to print in the terminal of the diff of the listCommit for the option asked.
   */
  def getLogResult(repoPath: String, listCommitToDiff: List[CommitToDiff], option: Option[(List[FilesToDiff], String) => String]): String = {

    @tailrec
    def loop(listCurrent: List[CommitToDiff], result: String): String = {

      listCurrent match {
        case Nil => result
        case head :: tail =>
          //create the string for a commit
          val stringCommit = Console.YELLOW + "commit " + head.sha + Console.RESET + "\n\n      " + head.message + "\n\n"

          if (option.isEmpty) {
            //recursion
            loop(tail, stringCommit + result)

          } else {
            val stringAllDiff = option.get(head.listFilesToDiff, repoPath)

            //create the string for a commit
            val stringCommitOption = stringCommit + "Diffs : " + "\n" + stringAllDiff + "\n\n"

            //update the result of the sgit diff
            val resultUpdated = stringCommitOption + result

            //recursion
            loop(tail, resultUpdated)
          }
      }
    }

    loop(listCommitToDiff, "")
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
   * @param repoPath : the path of the sgit repo
   * @param mapNew   : map of the new commits to diff with the pattern (path -> content)
   * @param mapOld   : map of the old commits to diff with the pattern (path -> content)
   * @return a list of FilesToDiff representing the diff to effectuate for one commit
   */
  def getListFilesToDiffForOneCommit(repoPath: String, mapNew: Map[String, List[String]], mapOld: Map[String, List[String]]): List[FilesToDiff] = {

    @tailrec
    def loop(listCurrent: List[FilesToDiff], mapNewCurrent: Map[String, List[String]]): List[FilesToDiff] = {
      if (mapNewCurrent.isEmpty) {

        val deletedFiles = mapOld.keys.toList diff mapNew.keys.toList
        val deletedTuples = deletedFiles.map(src => new FilesToDiff(List(), mapOld(src), src))
        listCurrent ++ deletedTuples

      } else {

        val tuple = mapNewCurrent.head
        val newBlob = tuple._2
        val oldBlob = mapOld(tuple._1)

        val listUpdated = new FilesToDiff(newBlob, oldBlob, tuple._1) :: listCurrent
        loop(listUpdated, mapNewCurrent.tail)
      }
    }

    loop(List(), mapNew)
  }

  /**
   *
   * @param repoPath   : the path of the sgit repo
   * @param listCommit : the list of all the commits objects asked to diff (sha -> content)
   * @return the list of all the CommitToDiff which each contains a list of FilesToDiff which represent the files in their commit
   */
  def getListFilesToDiffAllCommits(repoPath: String, listCommit: List[(String, String)]): List[CommitToDiff] = {

    @tailrec
    def loop(listCommitCurrent: List[(String, String)], result: List[CommitToDiff]): List[CommitToDiff] = {

      listCommitCurrent match {
        case Nil => result
        case head :: tail =>

          //recover the message of the commit
          val messageCommit = head._2.split("\n\n")(1)

          val mapCommitCurrent = CommitUtil.getCommitMap(repoPath, Some(head._1)).get

          val contentCommitList = head._2.split("\n")

          if (contentCommitList(1).split(" ")(0) == "Parent") {

            val shaCommitParent = contentCommitList(1).split(" ")(1)

            val mapCommitParent = CommitUtil.getCommitMap(repoPath, Some(shaCommitParent)).get

            val listFilesToDiff = getListFilesToDiffForOneCommit(repoPath, mapCommitCurrent, mapCommitParent)

            val commitToDiff = new CommitToDiff(head._1, messageCommit, listFilesToDiff)

            //update the result of the sgit diff
            val resultUpdated = commitToDiff :: result

            loop(tail, resultUpdated)

          }

          else {

            val mapEmpty = Map(("", List())).withDefaultValue(List())

            val listFilesToDiff = getListFilesToDiffForOneCommit(repoPath, mapCommitCurrent, mapEmpty)

            val commitToDiff = new CommitToDiff(head._1, messageCommit, listFilesToDiff)

            //update the result of the sgit diff
            val resultUpdated = commitToDiff :: result

            loop(tail, resultUpdated)
          }
      }
    }

    loop(listCommit, List())
  }
}
