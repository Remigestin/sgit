package command

import java.io.File

import util.{BranchUtil, CommitUtil, FileUtil}

object Branch {

  def branch(repoPath: String, name:String): String = {

    if (CommitUtil.isThereACommit(repoPath)) {
      val shaCommit = CommitUtil.getLastCommitObject(repoPath, BranchUtil.getCurrentBranchName(repoPath))
      val pathBranch = repoPath + File.separator + ".sgit" + File.separator + "branches" + File.separator + name
      if (!new File(pathBranch).exists()) {
        FileUtil.editFile(pathBranch, shaCommit, append = false)
      } else {
        return "branch " + name + " already exists"
      }
      return "branch " + name + " successfully created"
    }
    "Failed to resolve 'Head' as a valid ref"

  }

  def branchAV(repoPath: String): Unit = {

  }

}
