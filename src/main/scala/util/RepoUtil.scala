package util

import java.io.File

import RepoUtil.getRepoPath

import scala.annotation.tailrec

object RepoUtil {
  /**
   *
   * @param curDir : the dir in question
   * @return if the curDir param is in a sgit repo
   */
  def isInASgitRepo(curDir: String): Boolean = {
    getRepoPath(curDir).isDefined
  }

  /**
   *
   * @param path : the path were the sgit repo is asked
   * @return the path of the sgit repo of the path in param
   */
  @tailrec
  def getRepoPath(path: String): Option[String] = {
    if (path.isEmpty) None
    else if (new File(path + File.separator + ".sgit").exists()) {
      Some(path)
    } else {
      val parent = new File(path).getParentFile
      if (!parent.getName.isEmpty) RepoUtil.getRepoPath(parent.getAbsolutePath)
      else RepoUtil.getRepoPath("")
    }
  }
}
