package util

import util.FileUtil.readFileToList
import util.SgitObjectUtil._
import java.io.File
import java.util.regex.Pattern

import javafx.scene.DepthTest

import scala.annotation.tailrec

object CommitUtil {

  def getLastCommitObject(repoPath: String): String = {
    val pathBranch = BranchUtil.getCurrentBranchPath(repoPath)
    readFileToList(pathBranch).head
  }

  def getLastCommitTree(repoPath: String): String = {

    val pathBranch = BranchUtil.getCurrentBranchPath(repoPath)
    val commit = readFileToList(pathBranch).head

    val tree = readFileToList(repoPath + File.separator + ".sgit" + File.separator + "objects" + File.separator + commit).head.split(" ")(1)
    tree
  }

  def isThereACommit(repoPath: String): Boolean = {
    val pathBranch = BranchUtil.getCurrentBranchPath(repoPath)
    new File(pathBranch).exists()
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


}
