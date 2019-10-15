package util

import util.FileUtil.readFileToList
import util.SgitObjectUtil._
import java.io.File
import java.util.regex.Pattern

import javafx.scene.DepthTest

import scala.annotation.tailrec

object CommitUtil {

  def isThereACommit(repoPath: String): Boolean = {
    val pathBranch = BranchUtil.getCurrentBranchPath(repoPath)
    new File(pathBranch).exists()
  }

  def getTreeFromCommit(repoPath: String, shaCommit: String): String = {
    val commit = readSgitObjectToList(repoPath, shaCommit)
    commit.head.split(" ")(1)
  }

  def getLastCommitObject(repoPath: String): String = {
    val pathBranch = BranchUtil.getCurrentBranchPath(repoPath)
    readFileToList(pathBranch).head
  }

  def getLastCommitTree(repoPath: String): String = {
    getTreeFromCommit(repoPath, getLastCommitObject(repoPath))
  }

  @tailrec
  def getHashOfPathInTheCommit(repoPath: String, pathToFind: String, shaTree: String): Option[String] = {
    val separatorSplit = Pattern.quote(System.getProperty("file.separator"))
    val pathToFindTab = pathToFind.split(separatorSplit).toList

    if (pathToFindTab.length == 1) {

      val nameBlobToFind = pathToFindTab.head
      val treeContent = readSgitObjectToList(repoPath, shaTree)
      val treeContentWithOnlyBlob = treeContent.filter(_.split(" ")(0) == "blob")

      val lineBlobToFind = treeContentWithOnlyBlob.filter(_.split(" ")(2) == nameBlobToFind)

      if (lineBlobToFind.isEmpty) {
        None
      } else {
        val shaBlobToFind = lineBlobToFind.head.split(" ")(1)
        Some(shaBlobToFind)
      }

    } else {
      val nameTreeToFind = pathToFindTab.head
      val treeContent = readSgitObjectToList(repoPath, shaTree)
      val treeContentWithOnlyTree = treeContent.filter(_.split(" ")(0) == "tree")

      val lineTreeToFind = treeContentWithOnlyTree.filter(_.split(" ")(2) == nameTreeToFind)

      if (lineTreeToFind.isEmpty) {
        None
      } else {
        val shaTreeToFind = lineTreeToFind.head.split(" ")(1)
        val pathToFindDeeper = pathToFindTab.tail mkString File.separator
        getHashOfPathInTheCommit(repoPath, pathToFindDeeper, shaTreeToFind)
      }
    }
  }

  def getMapOfCommit(repoPath: String, shaCommit: String): Map[String, String] = {

    def loop(listContentTree: List[String], pathParent: String, mapCommit: Map[String, String]): Map[String, String] = {

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
          //if the file is in the racine, don't put a slash
          if (pathParent == "") {
            val mapCommitUpdated = mapCommit + (name -> sha)
            loop(listContentTree.tail, pathParent, mapCommitUpdated)
          } else {
            val mapCommitUpdated = mapCommit + (pathParent + File.separator + name -> sha)
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

    val shaTreeCommit = getTreeFromCommit(repoPath, shaCommit)
    val treeLists = readSgitObjectToList(repoPath, shaTreeCommit)

    loop(treeLists, "", Map())
  }


}
