package command

import java.io.File

import command.Log.{getAllCommits, getLogResult}
import util.{BranchUtil, CommitUtil, SgitObjectUtil}

import scala.annotation.tailrec

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

  def logOptionP(repoPath:String): String = {

    //read IO -> list of all the commits
    if (CommitUtil.isThereACommit(repoPath)) {
      val branchName = BranchUtil.getCurrentBranchName(repoPath)
      val shaLastCommit = CommitUtil.getLastCommitObject(repoPath, branchName)
      val listAllCommits = getAllCommits(repoPath, shaLastCommit)


      //recover the result of a log
      "branch " + branchName + "\n\n" + getLogOptionP(repoPath, listAllCommits)
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
          val stringCommit = "commit " + head._1 + "\n\n      " + messageCommit + "\n\n"

          //update the result of the sgit diff
          val resultUpdated =  stringCommit + result

          //recursion
          loop(tail, resultUpdated)
      }
    }

    loop(listCommit, "")
  }

  def getLogOptionP(repoPath: String, listCommit: List[(String, String)]): String = {

    @tailrec
    def loop(listCurrent: List[(String, String)], result: String): String = {

      listCurrent match {
        case Nil => result
        case head :: tail =>

          //recover the message of the commit
          val messageCommit = head._2.split("\n\n")(1)

          val mapCommitCurrent = CommitUtil.getMapOfCommit(repoPath,head._1)

          val contentCommitList = head._2.split("\n")

          if (contentCommitList(1).split(" ")(0) == "Parent") {
            val shaCommitParent = contentCommitList(1).split(" ")(1)

            val mapCommitParent = CommitUtil.getMapOfCommit(repoPath, shaCommitParent)

            val listTuplesToCompare = getListTuples(mapCommitCurrent,mapCommitParent)
              .map(tuple => (repoPath +File.separator + ".sgit" + File.separator +"objects" + File.separator + tuple._1,repoPath +File.separator + ".sgit" + File.separator +"objects" + File.separator + tuple._2, tuple._3 ))


            val stringAllDiff = Diff.getDiffAllFiles(listTuplesToCompare, repoPath)


            //create the string for a commit
            val stringCommit = "commit " + head._1 + "\n\n      " + messageCommit + "\n\nDiffs : " + "\n" + stringAllDiff + "\n\n"

            //update the result of the sgit diff
            val resultUpdated =  stringCommit + result

            loop(tail, resultUpdated)
          }

          else {


            val mapEmpty = Map(("","")).withDefaultValue("")

            val listTuplesToCompare = getListTuples(mapCommitCurrent,mapEmpty)
              .map(tuple => (repoPath +File.separator + ".sgit" + File.separator +"objects" + File.separator + tuple._1,repoPath +File.separator + ".sgit" + File.separator +"objects" + File.separator + tuple._2 , tuple._3))

            val stringAllDiff = Diff.getDiffAllFiles(listTuplesToCompare, repoPath)

            //create the string for a commit
            val stringCommit = "commit " + head._1 + "\n\n      " + messageCommit + "\n\nDiffs : " + "\n" + stringAllDiff + "\n\n"

            //update the result of the sgit diff
            val resultUpdated =  stringCommit + result

            //recursion
            loop(tail, resultUpdated)
          }


      }
    }

    loop(listCommit, "")
  }

  def getAllCommits(repoPath: String, lastCommit: String): List[(String, String)] = {

    @tailrec
    def loop(result:List[(String, String)], shaCurrentCommit: String): List[(String, String)] = {

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

  def getListTuples(mapNew: Map[String,String], mapOld: Map[String, String]) : List[(String, String, String)] = {

    @tailrec
    def loop(listCurrent: List[(String, String, String)], mapNewCurrent: Map[String, String]) : List[(String, String, String)] = {
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
