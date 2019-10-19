package command

import java.io.File
import java.nio.file.Paths
import java.util.regex.Pattern

import util.{BranchUtil, CommitUtil, FileUtil, IndexUtil}
import util.FileUtil.recursiveListFiles
import util.IndexUtil._

class StatusLists(val untrackedFiles: List[String], val trackedModifiedNotAdd: List[String], val trackedNeverCommitted: List[String], val trackedAndCommittedModified: List[String], val deletionsNotStaged: List[String], val deletionsNotCommitted: List[String])

object Status {

  def status(curDir: String): String = {

    //--------------------------- IO READING STEP
    val repoPath = Repo.getRepoPath(curDir).get
    val mapIndex = readIndexToMap(repoPath)
    val isThereACommit = CommitUtil.isThereACommit(repoPath)


    //recover the path of all the files in the repoPath
    val allFilesRepoList = FileUtil
      .recursiveListFiles(new File(repoPath)).toList
      .filter(_.isFile)
      .filter(!_.getAbsolutePath.contains(".sgit"))
      .map(_.getAbsolutePath)
      .map(_.replace(repoPath + File.separator, "")).toList

    val contentFilesRepoList = allFilesRepoList.map(p => FileUtil.readFileToList(repoPath + File.separator + p))

    val allFilesRepoMap = (allFilesRepoList zip contentFilesRepoList).toMap


    val nameBranch = BranchUtil.getCurrentBranchName(repoPath)
    val shaCommit = CommitUtil.getLastCommitObject(repoPath, nameBranch)
    val mapCommit = CommitUtil.getCommitMap(repoPath, shaCommit)


    //---------------------------- PURE FUNCTIONAL STEP

    val untrackedFiles = getAllPathsUntracked(repoPath, mapIndex, allFilesRepoMap, curDir)
    val trackedModifiedNotAdd = getAllPathsTrackedModifiedNotAdd(repoPath, mapIndex, allFilesRepoMap, curDir)
    val trackedNeverCommitted = getAllPathTrackedNeverCommitted(repoPath, mapIndex, mapCommit, curDir)
    val trackedAndCommittedModified = getAllPathTrackedAndCommittedModified(repoPath,mapIndex,mapCommit,curDir)
    val deletionsNotStaged = getAllDeletionsNotStaged(repoPath,mapIndex,allFilesRepoMap,curDir)
    val deletionsNotCommitted = getAllDeletionsNotCommitted(repoPath,mapIndex,mapCommit,curDir)



    val status = new StatusLists(untrackedFiles, trackedModifiedNotAdd, trackedNeverCommitted, trackedAndCommittedModified,deletionsNotStaged,deletionsNotCommitted)


    //Return the string of the status command
    getStatusRep(status)

  }


    def getStatusRep(statusLists: StatusLists): String = {

      val untracked = "Untracked files:\n " + "(use \"sgit add <file>...\" to include in what will be committed)\n\n" +  Console.RED + (statusLists.untrackedFiles mkString "\n") + Console.RESET
      val trackedModifiedNotAdd = "Changes not staged for commit:\n  (use \"sgit add <file>...\" to update what will be committed)\n\n" +  Console.RED + (statusLists.trackedModifiedNotAdd.map("modified:   " + _) mkString "\n") + "\n" + (statusLists.deletionsNotStaged.map("deleted:   " + _) mkString "\n") + Console.RESET


      val toBeCommitted = "Changes to be committed:\n\n" + Console.GREEN + (statusLists.trackedNeverCommitted.map("new file:   " + _) mkString "\n") + "\n" + (statusLists.trackedAndCommittedModified.map("modified:   " + _) mkString "\n") + "\n" + (statusLists.deletionsNotCommitted.map("deleted:   " + _) mkString "\n") + Console.RESET

      toBeCommitted + "\n\n" + trackedModifiedNotAdd + "\n\n" + untracked


    }

  /**
   *
   * @param repoPath : the path of the current sgit repo
   * @return the list of the path of all the files (in the sgit repo in parm) which are not tracked by sgit
   */
  def getAllPathsUntracked(repoPath: String, mapIndex: Map[String, String], allFilesRepoList: Map[String, List[String]], curDir: String): List[String] = {

    //recover the list of the paths in the index
    val pathsIndex = mapIndex.keys.toList

    //filter all the path files which are not in the index
    val pathsRepo = allFilesRepoList.keys.toList
    val pathsUntracked = pathsRepo diff pathsIndex

    pathsUntracked
  }

  def getAllPathsTrackedModifiedNotAdd(repoPath: String, mapIndex: Map[String, String], allFilesRepoList: Map[String, List[String]], curDir: String): List[String] = {

    val srcIndexAll = mapIndex.keys.toList
    val untracked = getAllPathsUntracked(repoPath, mapIndex, allFilesRepoList, curDir)

    val srcIndexExists = srcIndexAll diff untracked

    val newShas = srcIndexExists
      .map(allFilesRepoList(_))
      .map(_ mkString "\n")
      .map(FileUtil.sha1Hash)

    val mapNewShas = (srcIndexExists zip newShas).toMap

    mapNewShas.filterNot(m => mapIndex(m._1) == m._2).keys.toList
  }

  def getAllPathTrackedNeverCommitted(repoPath: String, mapIndex: Map[String, String], mapCommit: Option[Map[String, List[String]]], curDir: String): List[String] = {

    val srcIndex = mapIndex.keys.toList
    if (mapCommit.isDefined) {

      val srcCommit = mapCommit.get.keys.toList

      srcIndex diff srcCommit
    }
    else {
      srcIndex
    }
  }

  def getAllPathTrackedAndCommittedModified(repoPath: String, mapIndex: Map[String, String], mapCommit: Option[Map[String, List[String]]], curDir: String): List[String] = {

    val srcIndex = mapIndex.keys.toList

    if (mapCommit.isDefined) {
      val srcIndexNotInLastCommit = getAllPathTrackedNeverCommitted(repoPath, mapIndex, mapCommit, curDir)

      val srcIndexInLastCommit = srcIndex diff srcIndexNotInLastCommit

      srcIndexInLastCommit.filterNot(p => mapIndex(p) == FileUtil.sha1Hash(mapCommit.get(p) mkString "\n"))
    } else {
      List()
    }

  }

  def getAllDeletionsNotStaged(repoPath: String, mapIndex: Map[String, String], allFilesRepoList: Map[String, List[String]], curDir: String): List[String] = {

    val srcIndex = mapIndex.keys.toList
    srcIndex diff allFilesRepoList.keys.toList

  }

  def getAllDeletionsNotCommitted(repoPath: String,  mapIndex: Map[String, String], mapCommit: Option[Map[String, List[String]]],  curDir: String): List[String] = {

    val srcIndex = mapIndex.keys.toList

    if (mapCommit.isDefined) {
      mapCommit.get.keys.toList diff srcIndex
    }
    else {
      List()
    }
  }

  def relativizeAListOfPath(repoPath: String, curDir: String, list: List[String]): List[String] = {

    list.map(s => Paths.get(curDir).relativize(Paths.get(repoPath + File.separator + s)).toString)
  }

}
