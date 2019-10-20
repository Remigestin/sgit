package command

import java.io.File

import util.IndexUtil._
import util.{BranchUtil, CommitUtil, FileUtil}

class StatusLists(val untrackedFiles: List[String], val trackedModifiedNotAdd: List[String], val trackedNotCommitted: List[String],
                  val trackedAndCommittedModified: List[String], val deletionsNotStaged: List[String], val deletionsNotCommitted: List[String])

object Status {

  def status(repoPath: String): String = {

    //--------------------------- IO READING STEP

    //recover the map of the index files (src -> sha)
    val mapIndex = readIndexToMap(repoPath)


    //recover the path of all the files in the repoPath in a map (src -> content)
    val allFilesRepoList = FileUtil
      .recursiveListFiles(new File(repoPath))
      .filter(_.isFile)
      .filter(!_.getAbsolutePath.contains(".sgit"))
      .map(_.getAbsolutePath)
      .map(_.replace(repoPath + File.separator, "")).toList

    val contentFilesRepoList = allFilesRepoList.map(p => FileUtil.readFileToList(repoPath + File.separator + p))

    val allFilesRepoMap = (allFilesRepoList zip contentFilesRepoList).toMap


    //recover the map of the last commit. It's an option[map]
    val nameBranch = BranchUtil.getCurrentBranchName(repoPath)
    val shaCommit = CommitUtil.getLastCommitObject(repoPath, nameBranch)
    val mapCommit = CommitUtil.getCommitMap(repoPath, shaCommit)


    //---------------------------- PURE FUNCTIONAL STEP

    //recover all the different paths needed for the status

    val untrackedFiles = getAllPathsUntracked(mapIndex, allFilesRepoMap)
    val trackedModifiedNotAdd = getAllPathsTrackedModifiedNotAdd(mapIndex, allFilesRepoMap)
    val trackedNeverCommitted = getAllPathTrackedNotCommitted(mapIndex, mapCommit)
    val trackedAndCommittedModified = getAllPathTrackedAndCommittedModified(mapIndex, mapCommit)
    val deletionsNotStaged = getAllDeletionsNotStaged(mapIndex, allFilesRepoMap)
    val deletionsNotCommitted = getAllDeletionsNotCommitted(mapIndex, mapCommit)


    val status = new StatusLists(untrackedFiles, trackedModifiedNotAdd, trackedNeverCommitted, trackedAndCommittedModified, deletionsNotStaged, deletionsNotCommitted)


    //Return the string of the status command
    getStatusRep(status)

  }


  /**
   *
   * @param statusLists : the object with all the different types of files to print
   * @return the string of the status command
   */
  def getStatusRep(statusLists: StatusLists): String = {

    val untracked = "Untracked files:\n " + "(use \"sgit add <file>...\" to include in what will be committed)\n\n" + Console.RED + (statusLists.untrackedFiles mkString "\n") + Console.RESET
    val trackedModifiedNotAdd = "Changes not staged for commit:\n  (use \"sgit add <file>...\" to update what will be committed)\n\n" + Console.RED + (statusLists.trackedModifiedNotAdd.map("modified:   " + _) mkString "\n") + "\n" + (statusLists.deletionsNotStaged.map("deleted:   " + _) mkString "\n") + Console.RESET


    val toBeCommitted = "Changes to be committed:\n\n" + Console.GREEN + (statusLists.trackedNotCommitted.map("new file:   " + _) mkString "\n") + "\n" + (statusLists.trackedAndCommittedModified.map("modified:   " + _) mkString "\n") + "\n" + (statusLists.deletionsNotCommitted.map("deleted:   " + _) mkString "\n") + Console.RESET

    toBeCommitted + "\n\n" + trackedModifiedNotAdd + "\n\n" + untracked


  }


  /**
   *
   * @param mapIndex         : the index content in a map (path -> hash)
   * @param allFilesRepoList : all the files in the repo in a map (path -> content)
   * @return the paths of all the untracked files
   */
  def getAllPathsUntracked(mapIndex: Map[String, String], allFilesRepoList: Map[String, List[String]]): List[String] = {

    //recover the list of the paths in the index
    val pathsIndex = mapIndex.keys.toList

    //filter all the path files which are not in the index
    val pathsRepo = allFilesRepoList.keys.toList
    val pathsUntracked = pathsRepo diff pathsIndex

    pathsUntracked
  }

  /**
   *
   * @param mapIndex         : the index content in a map (path -> hash)
   * @param allFilesRepoList : all the files in the repo in a map (path -> content)
   * @return the path of all the files updated but not up-to-date with the index
   */
  def getAllPathsTrackedModifiedNotAdd(mapIndex: Map[String, String], allFilesRepoList: Map[String, List[String]]): List[String] = {

    //recover the list of the paths in the index and filter the files untracked
    val srcIndexAll = mapIndex.keys.toList
    val untracked = getAllPathsUntracked(mapIndex, allFilesRepoList)
    val srcIndexExists = srcIndexAll diff untracked

    //recover the current sha of these files
    val newShas = srcIndexExists
      .map(allFilesRepoList(_))
      .map(_ mkString "\n")
      .map(FileUtil.sha1Hash)

    val mapNewShas = (srcIndexExists zip newShas).toMap

    //filter the files which have their current shas different of the sha in the index
    mapNewShas.filterNot(m => mapIndex(m._1) == m._2).keys.toList
  }

  /**
   *
   * @param mapIndex  : the index content in a map (path -> hash)
   * @param mapCommit : the content of the last commit in a map (path -> content)
   * @return the paths of the files in the index but not in the last commit
   */
  def getAllPathTrackedNotCommitted(mapIndex: Map[String, String], mapCommit: Option[Map[String, List[String]]]): List[String] = {

    //recover the list of the paths in the index
    val srcIndex = mapIndex.keys.toList

    //if there was a commit, return the files in the index diff the files in the commit
    if (mapCommit.isDefined) {

      val srcCommit = mapCommit.get.keys.toList

      srcIndex diff srcCommit
    }
    else {
      srcIndex
    }
  }

  /**
   *
   * @param mapIndex  : the index content in a map (path -> hash)
   * @param mapCommit : the content of the last commit in a map (path -> content)
   * @return the files which do not have the same content between the last commit and the index
   */
  def getAllPathTrackedAndCommittedModified(mapIndex: Map[String, String], mapCommit: Option[Map[String, List[String]]]): List[String] = {

    //recover the list of the paths in the index
    val srcIndex = mapIndex.keys.toList

    //if there was a commit, return the files with a different sha1 between the index and the last commit
    if (mapCommit.isDefined) {
      val srcIndexNotInLastCommit = getAllPathTrackedNotCommitted(mapIndex, mapCommit)

      val srcIndexInLastCommit = srcIndex diff srcIndexNotInLastCommit

      srcIndexInLastCommit.filterNot(p => mapIndex(p) == FileUtil.sha1Hash(mapCommit.get(p) mkString "\n"))
    } else {
      List()
    }

  }

  /**
   *
   * @param mapIndex : the index content in a map (path -> hash)
   * @param allFilesRepoList : all the files in the repo in a map (path -> content)
   * @return all the path which are in the index but no anymore in the repo
   */
  def getAllDeletionsNotStaged( mapIndex: Map[String, String], allFilesRepoList: Map[String, List[String]]): List[String] = {

    val srcIndex = mapIndex.keys.toList
    srcIndex diff allFilesRepoList.keys.toList

  }

  /**
   *
   * @param mapIndex  : the index content in a map (path -> hash)
   * @param mapCommit : the content of the last commit in a map (path -> content)
   * @return all the paths in the last commit but not in the index
   */
  def getAllDeletionsNotCommitted(mapIndex: Map[String, String], mapCommit: Option[Map[String, List[String]]]): List[String] = {

    val srcIndex = mapIndex.keys.toList

    if (mapCommit.isDefined) {
      mapCommit.get.keys.toList diff srcIndex
    }
    else {
      List()
    }
  }

}
