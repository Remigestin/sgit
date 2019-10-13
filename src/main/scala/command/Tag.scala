package command

import java.io.File

import parser.ErrorMessage
import util.{CommitUtil, FileUtil}

object Tag {

  def tag(repoPath: String, name: String): String = {
    if (CommitUtil.isThereACommit(repoPath)) {
      val shaCommit = CommitUtil.getLastCommitObject(repoPath)
      val pathTag = repoPath + File.separator + ".sgit" + File.separator + "tags" + File.separator + name
      if (!new File(pathTag).exists()) {
        FileUtil.editFile(pathTag, shaCommit, append = false)
      } else {
        return "tag " + name + " already exists"
      }
      return "tag " + name + " successfully created"
    }
    "Failed to resolve 'Head' as a valid ref"
  }

}
