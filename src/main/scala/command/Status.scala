package command

import java.io.File
import java.nio.file.Paths
import java.util.regex.Pattern

import util.{CommitUtil, FileUtil, IndexUtil}
import util.FileUtil.recursiveListFiles
import util.IndexUtil._

object Status {

  def status(curDir: String): String = {

    val repoPath = Repo.getRepoPath(curDir).get


    val untracked = "Untracked files:\n " + "(use \"sgit add <file>...\" to include in what will be committed)\n\n" + (getAllPathsUntracked(repoPath, curDir) mkString "\n")
    val trackedModifiedNotAdd = "Changes not staged for commit:\n  (use \"sgit add <file>...\" to update what will be committed)\n\n" + (getAllPathsTrackedModifiedNotAdd(repoPath, curDir).map("modified:   " + _) mkString "\n")


    val toBeCommitted = "Changes to be committed:\n\n" + (getAllPathTrackedNeverCommitted(repoPath, curDir).map("new file:   " + _) mkString "\n") + "\n" + (getAllPathTrackedAndCommittedModified(repoPath, curDir).map("modified:   " + _) mkString "\n")

    toBeCommitted + "\n\n" + trackedModifiedNotAdd + "\n\n" + untracked


  }

  /**
   *
   * @param repoPath : the path of the current sgit repo
   * @return the list of the path of all the files (in the sgit repo in parm) which are not tracked by sgit
   */
  def getAllPathsUntracked(repoPath: String, curDir: String): List[String] = {

    //recover the content of the index file
    val indexContent = readIndexToList(repoPath) mkString "\n"

    //recover the path of all the files in the repoPath
    val pathsAllFilesRepoList = FileUtil
      .recursiveListFiles(new File(repoPath)).toList
      .filter(_.isFile)
      .filter(!_.getAbsolutePath.contains(".sgit"))
      .map(_.getAbsolutePath)
      .map(_.replace(repoPath + File.separator, ""))

    //filter all the path files which are not in the index
    val pathsUntracked = pathsAllFilesRepoList.filter(!indexContent.contains(_))

    relativizeAListOfPath(repoPath, curDir, pathsUntracked)
  }

  def getAllPathsTrackedModifiedNotAdd(repoPath: String, curDir: String): List[String] = {
    val indexList = readIndexToList(repoPath)
    val indexMap = readIndexToMap(repoPath)

    val srcIndex = indexList.map(_.split(" ")(1))

    val newShas = srcIndex
      .map(repoPath + File.separator + _)
      .map(FileUtil.readFileToList(_) mkString "\n")
      .map(FileUtil.sha1Hash)

    val mapNewShas = (srcIndex zip newShas).toMap

    val paths = mapNewShas.filter(m => indexMap(m._1) != m._2).keys.toList

    relativizeAListOfPath(repoPath, curDir, paths)

  }

  def getAllPathTrackedNeverCommitted(repoPath: String, curDir: String): List[String] = {

    val indexList = readIndexToList(repoPath)

    val srcIndex = indexList.map(_.split(" ")(1))

    if (CommitUtil.isThereACommit(repoPath)) {
      val lastTreeCommit = CommitUtil.getLastCommitTree(repoPath)
      val paths = srcIndex.filterNot(CommitUtil.getHashOfPathInTheCommit(repoPath, _, lastTreeCommit).isDefined)
      relativizeAListOfPath(repoPath, curDir, paths)
    } else {
      relativizeAListOfPath(repoPath, curDir, srcIndex)
    }


  }

  def getAllPathTrackedAndCommittedModified(repoPath: String, curDir: String): List[String] = {

    val indexList = readIndexToList(repoPath)
    val indexMap = readIndexToMap(repoPath)

    val srcIndex = indexList.map(_.split(" ")(1))

    if (CommitUtil.isThereACommit(repoPath)) {

      val lastTreeCommit = CommitUtil.getLastCommitTree(repoPath)

      val listHashCommit = srcIndex.map(CommitUtil.getHashOfPathInTheCommit(repoPath, _, lastTreeCommit).getOrElse("not in commit"))

      val mapCommit = (srcIndex zip listHashCommit).toMap.filterNot(m => m._2 == "not in commit")
      val paths = mapCommit.filter(m => indexMap(m._1) != m._2).keys.toList

      relativizeAListOfPath(repoPath, curDir, paths)
    } else {
      List()
    }
  }

  def relativizeAListOfPath(repoPath: String, curDir: String, list: List[String]): List[String] = {

    list.map(s => Paths.get(curDir).relativize(Paths.get(repoPath + File.separator + s)).toString)
  }

}
