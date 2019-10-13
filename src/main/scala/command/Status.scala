package command

import java.io.File
import java.util.regex.Pattern

import util.{CommitUtil, FileUtil, IndexUtil}
import util.FileUtil.recursiveListFiles
import util.IndexUtil._

object Status {

  def status(curDir: String): List[String] = {

    val repoPath = Repo.getRepoPath(curDir).get

    getAllPathTrackedAndCommittedModified(repoPath)

  }

  /**
   *
   * @param repoPath : the path of the current sgit repo
   * @return the list of the path of all the files (in the sgit repo in parm) which are not tracked by sgit
   */
  def getAllPathsUntracked(repoPath: String): List[String] = {

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

    pathsUntracked
  }

  def getAllPathsTrackedModifiedNotAdd(repoPath: String) : List[String] = {
    val indexList = readIndexToList(repoPath)
    val indexMap = readIndexToMap(repoPath)

    val srcIndex = indexList.map(_.split(" ")(1))

    val newShas = srcIndex
      .map(repoPath + File.separator + _)
      .map(FileUtil.readFileToList(_) mkString "\n")
      .map(FileUtil.sha1Hash)

    val mapNewShas = (srcIndex zip newShas).toMap

    mapNewShas.filter(m =>  indexMap(m._1) != m._2).keys.toList

  }

  def getAllPathTrackedNeverCommitted(repoPath: String): List[String] = {

    val indexList = readIndexToList(repoPath)

    val srcIndex = indexList.map(_.split(" ")(1))

    val lastTreeCommit = CommitUtil.getLastCommitTree(repoPath)

    srcIndex.filterNot(CommitUtil.getHashOfPathInTheCommit(repoPath, _, lastTreeCommit).isDefined)


  }

  def getAllPathTrackedAndCommittedModified(repoPath:String) : List[String] = {

    val indexList = readIndexToList(repoPath)
    val indexMap = readIndexToMap(repoPath)

    val srcIndex = indexList.map(_.split(" ")(1))

    val lastTreeCommit = CommitUtil.getLastCommitTree(repoPath)

    val listHashCommit = srcIndex.map(CommitUtil.getHashOfPathInTheCommit(repoPath,_, lastTreeCommit).getOrElse("not in commit") )

    val mapCommit = (srcIndex zip listHashCommit).toMap.filterNot(m => m._2 == "not in commit")

    mapCommit.filter(m =>  indexMap(m._1) != m._2).keys.toList
  }

}
