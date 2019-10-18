package command

import util.CommitUtil

object Checkout {

  def checkout(repoPath: String): String = {

    //read IO
    if (CommitUtil.isThereACommit(repoPath)) {





      ""

    } else {
      "there is no commit"
    }
  }
}


