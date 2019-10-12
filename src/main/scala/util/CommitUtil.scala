package util

import util.FileUtil.readFileToList
import util.SgitObjectUtil._
import java.io.File
import java.util.regex.Pattern

import javafx.scene.DepthTest

import scala.annotation.tailrec

object CommitUtil {

  def getLastCommitTree(repoPath: String): String = {

    val pathBranch = BranchUtil.getCurrentBranchPath(repoPath)
    val commitParent = readFileToList(pathBranch).head

    val tree = readFileToList(repoPath + File.separator + ".sgit" + File.separator + "objects" + File.separator + commitParent).head.split(" ")(1)
    tree
  }

  @tailrec
  def hashOfBlobInTheCommit(repoPath: String, pathToFind: String, shaTree: String, depth: Int = 0): Option[String] = {
    val separatorSplit = Pattern.quote(System.getProperty("file.separator"))
    if (depth == pathToFind.split(separatorSplit).length -1) {

      val nameBlobToFind = pathToFind.split(separatorSplit)(depth)
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
      val nameTreeToFind = pathToFind.split(separatorSplit)(depth)
      val treeContent = readSgitObjectToList(repoPath, shaTree)
      val treeContentWithOnlyTree = treeContent.filter(_.split(" ")(0) == "tree")

      val lineTreeToFind = treeContentWithOnlyTree.filter(_.split(" ")(2) == nameTreeToFind)

      if (lineTreeToFind.isEmpty) {
        None
      } else {
        val shaTreeToFind = lineTreeToFind.head.split(" ")(1)
        hashOfBlobInTheCommit(repoPath, pathToFind, shaTreeToFind, depth + 1)
      }
    }
  }


}
