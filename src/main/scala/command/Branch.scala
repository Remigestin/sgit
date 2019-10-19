package command

import java.io.File

import util.{BranchUtil, CommitUtil, FileUtil}

object Branch {

  def branch(repoPath: String, name: String): String = {

    if (CommitUtil.isThereACommit(repoPath)) {
      val shaCommit = CommitUtil.getLastCommitObject(repoPath, BranchUtil.getCurrentBranchName(repoPath)).get
      val pathBranch = repoPath + File.separator + ".sgit" + File.separator + "branches" + File.separator + name
      if (!new File(pathBranch).exists()) {
        FileUtil.editFile(pathBranch, shaCommit, append = false)
      } else {
        return "branch " + name + " already exists"
      }
      return "branch " + name + " successfully created"
    }
    "Fatal: Not a valid object name: 'master'."
  }

  def branchAV(repoPath: String): String = {

    if (CommitUtil.isThereACommit(repoPath)) {

      //--IO
      val listBranches = BranchUtil.getAllBranchesOrTagItem(repoPath, "branches")
      val listTags = BranchUtil.getAllBranchesOrTagItem(repoPath, "tags")

      //--FP
      val branchCurrentItem = listBranches.filter(BranchUtil.getCurrentBranchName(repoPath) == _.name).map(b => Console.GREEN +b.name + " " + b.shaCommit + " " + b.commitMsg + Console.RESET +"\n")
      val listBranchWithoutCurrentBranch = listBranches.filterNot(BranchUtil.getCurrentBranchName(repoPath) == _.name)


      val repWithBranches =  "Branches : \n" + branchCurrentItem.head + (listBranchWithoutCurrentBranch.map(b => b.name + " " + b.shaCommit + " " + b.commitMsg) mkString "\n")
      val repWithAll = repWithBranches + "\n\n Tags : \n" + (listTags.map(b => b.name + " " + b.shaCommit + " " + b.commitMsg) mkString "\n")

      repWithAll

    } else {
      ""
    }
  }
}
