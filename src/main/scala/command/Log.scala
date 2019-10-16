package command

import util.{BranchUtil, CommitUtil}

object Log {

  def log(repoPath: String): String = {

    val shaLastCommit = CommitUtil.getLastCommitObject(repoPath, BranchUtil.getCurrentBranchName(repoPath))
    val listAllCommits = CommitUtil.getAllCommits(repoPath, shaLastCommit)
    getLogResult(repoPath, listAllCommits)

  }

  def getLogResult(repoPath: String, listCommit: List[(String, String)]): String = {

    def loop(listCurrent: List[(String, String)], result: String): String = {

      listCurrent match {
        case Nil => result
        case head :: tail =>{
          val contentList = result.split("\n")









          ""
        }


      }


    }

    loop(listCommit, "")
  }

}
