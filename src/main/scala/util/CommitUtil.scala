package util

import util.FileUtil.readFileToList
import java.io.File

object CommitUtil {

  def getLastCommitTree(repoPath: String): String = {

    val pathBranch = BranchUtil.getCurrentBranchPath(repoPath)
    val commitParent = readFileToList(pathBranch).head

    val tree = readFileToList(repoPath + File.separator + ".sgit" + File.separator + "objects" + File.separator + commitParent).head.split(" ")(1)
    tree
  }


}
