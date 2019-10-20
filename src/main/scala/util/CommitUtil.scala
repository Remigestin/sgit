package util

import java.io.File

import util.FileUtil.readFileToList
import util.SgitObjectUtil._

class CommitMap(val map: Map[String, List[String]])

object CommitUtil {

  /**
   *
   * @param repoPath : the path of the sgit repo
   * @return if there is a commit in the repo
   */
  def isThereACommit(repoPath: String): Boolean = {
    val pathBranch = BranchUtil.getCurrentBranchPath(repoPath)
    new File(pathBranch).exists()
  }

  /**
   *
   * @param repoPath  : the path of the sgit repo
   * @param shaCommit : the sha1 of the commit asked
   * @return the sha1 of the tree of the commit asked
   */
  def getTreeFromCommit(repoPath: String, shaCommit: String): String = {
    val commit = readSgitObjectToList(repoPath, shaCommit)
    commit.head.split(" ")(1)
  }

  /**
   *
   * @param repoPath   : the path of the sgit repo
   * @param branchName : the name of the branch asked
   * @return an option of the sha1 of the last commit of the branch asked. Return None if there was 0 commit.
   */
  def getLastCommitObject(repoPath: String, branchName: String): Option[String] = {
    val pathBranch = repoPath + File.separator + ".sgit" + File.separator + "branches" + File.separator + branchName
    if (new File(pathBranch).exists()) {
      Some(readFileToList(pathBranch).head)
    } else {
      None
    }


  }

  /**
   *
   * @param repoPath   : the path of the sgit repo
   * @param branchName : the name of the branch asked
   * @return the sha1 of the tree of the last commit of the branch in param
   */
  def getLastCommitTree(repoPath: String, branchName: String): String = {
    getTreeFromCommit(repoPath, getLastCommitObject(repoPath, branchName).get)
  }

  /**
   *
   * @param repoPath  : the path of the sgit repo
   * @param shaCommit : the hash of the commit object (option)
   * @return a option of a map which represent all content of the commit with this pattern map(path -> content). Return none if the shacommit is empty
   */
  def getCommitMap(repoPath: String, shaCommit: Option[String]): Option[Map[String, List[String]]] = {

    def loop(listContentTree: List[String], pathParent: String, mapCommit: Map[String, List[String]]): Map[String, List[String]] = {

      //if the tree is completely read, return the mapCommit
      if (listContentTree.isEmpty) {
        mapCommit
      }

      else {
        val lineCurrent = listContentTree.head

        //if a blob is inspected, it's added to the map then it calls the next line
        if (lineCurrent.split(" ")(0) == "blob") {
          val name = lineCurrent.split(" ")(2)
          val sha = lineCurrent.split(" ")(1)
          val content = readSgitObjectToList(repoPath, sha)


          //if the file is in the racine, don't put a slash
          if (pathParent == "") {
            val mapCommitUpdated = mapCommit + (name -> content)
            loop(listContentTree.tail, pathParent, mapCommitUpdated)
          } else {
            val mapCommitUpdated = mapCommit + (pathParent + File.separator + name -> content)
            loop(listContentTree.tail, pathParent, mapCommitUpdated)
          }
        }

        //if a tree is inspected, it first read the tree and update the map then it calls the next line
        else {
          val nameTree = lineCurrent.split(" ")(2)
          val shaTree = lineCurrent.split(" ")(1)
          val contentTree = readSgitObjectToList(repoPath, shaTree)
          //if the file is in the racine, don't put a slash
          if (pathParent == "") {
            val mapCommitUpdated = loop(contentTree, nameTree, mapCommit)
            loop(listContentTree.tail, pathParent, mapCommitUpdated)
          } else {
            val mapCommitUpdated = loop(contentTree, pathParent + File.separator + nameTree, mapCommit)
            loop(listContentTree.tail, pathParent, mapCommitUpdated)
          }
        }
      }
    }

    if (shaCommit.isDefined) {
      val shaTreeCommit = getTreeFromCommit(repoPath, shaCommit.get)
      val treeLists = readSgitObjectToList(repoPath, shaTreeCommit)
      Some(loop(treeLists, "", Map().withDefaultValue(List())))
    } else {
      None
    }


  }


}
