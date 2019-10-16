package command

import util.{BranchUtil, CommitUtil}

import scala.annotation.tailrec

object Log {

  def log(repoPath: String): String = {


    //read IO -> list of all the commits
    if (CommitUtil.isThereACommit(repoPath)) {
      val shaLastCommit = CommitUtil.getLastCommitObject(repoPath, BranchUtil.getCurrentBranchName(repoPath))
      val listAllCommits = CommitUtil.getAllCommits(repoPath, shaLastCommit)


      //recover the result of a log
      getLogResult(repoPath, listAllCommits)
    } else {
      ""
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
          val stringCommit = "commit " + head._1 + "\n\n      " + messageCommit + "\n\n"

          //update the result of the sgit diff
          val resultUpdated = result + stringCommit

          //recursion
          loop(tail, resultUpdated)
      }
    }

    loop(listCommit, "")
  }

}
