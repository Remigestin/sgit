package command


import java.io.File
import java.io.File._
import java.util.regex.Pattern

import util.IndexUtil._
import util.FileUtil._
import util.SgitObjectUtil._
import util.{BranchUtil, CommitUtil, IndexUtil, SgitObjectUtil}

import scala.annotation.tailrec


object Commit {

  /**
   *
   * @param repoPath : path of the sgit Repo
   * @param message  : message of the commit
   * @return the message of the sgit commit command
   */
  def commit(repoPath: String, message: String): String = {

    //----------------- IO READING STEP

    //get the content of the index in a map : (src -> blob)
    val mapIndexIO = readIndexToMap(repoPath)
    val pathBranch = BranchUtil.getCurrentBranchPath(repoPath)
    val branchName = BranchUtil.getCurrentBranchName(repoPath)

    //------------------ PURE FUNCTIONAL STEP

    //recover the sha of the treeCommit
    val mapTreesToWrite = getContentTrees(mapIndexIO, repoPath)

    //recover the sha of the mainTree
    val contentTreeCommit = mapTreesToWrite("") mkString "\n"
    val shaTreeCommit = sha1Hash(contentTreeCommit)

    //------------------- IO WRITING STEP

    IOWriteTreesObjects(repoPath, mapTreesToWrite.values.toList)
    val notSameCommit = IOWriteCommitObject(repoPath, pathBranch, shaTreeCommit, message)

    //------------------- MESSAGE RETURN STEP

    if (notSameCommit) {
      "Commit is ok"
    }
    else {
      "On branch " + branchName + "\nNothing to commit, working tree clean"
    }
  }

  /**
   * @param mapIndex : the content of the index in a map : (src -> blob)
   * @param repoPath : path of the sgit repo
   * @return map with all the contents of each trees (pure func)
   */

  def getContentTrees(mapIndex: Map[String, String], repoPath: String): Map[String, List[String]] = {

    @tailrec
    def loop(mapParent: Map[String, List[String]], depth: Int, pathsIndex: List[List[String]]): Map[String, List[String]] = {

      val separatorSplit = Pattern.quote(System.getProperty("file.separator"))

      if (depth == 0) {
        mapParent
      }

      else {

        //filter only the paths which match with the current val of size param and remove the duplicate ones, and string it

        val pathsCurrentSize = pathsIndex
          .filter(l => l.length == depth)
          .map(l => l mkString separator)
          .distinct

        //---- STEP OF THE CREATION OF BLOB LINES

        //filter only the blob (the files)
        val blobs = pathsCurrentSize.filter(f => new File(f).isFile)

        //recover the paths of the parents directory for each blob
        val pathsBlobParent = blobs.map(blob => blob.split(separatorSplit).slice(0, blob.split(separatorSplit).length - 1) mkString separator)

        //creation of the line of the blob in the future tree file with this pattern :  "blob sha1 name"
        val linesBlobChildren = blobs.map(blob => "blob " + mapIndex(blob) + " " + blob.split(separatorSplit).last)

        //update the map of the parents with the lines children created
        val mapParentPostBlobsStep = updateMapParent(mapParent, pathsBlobParent, linesBlobChildren)

        //----  STEP OF THE CREATION OF TREE LINES

        //filter only the directories
        val pathsDir = pathsCurrentSize.filter(f => new File(f).isDirectory)

        //recover the paths of the parents directory for each directory
        val pathsDirParent = pathsDir.map(tree => tree.split(separatorSplit).slice(0, tree.split(separatorSplit).length - 1) mkString separator)

        //creation of the line of the tree in the future parent tree file with this pattern :  "tree sha1 name"
        val linesTreeChildren = pathsDir.map(dir => "tree " + sha1Hash(mapParentPostBlobsStep(dir) mkString "\n") + " " + dir.split(separatorSplit).last)

        //update the map of the parents with the lines trees created
        val mapParentPostTreesStep = updateMapParent(mapParentPostBlobsStep, pathsDirParent, linesTreeChildren)


        //---  PREPARE THE NEXT RECURSION

        //remove the elements created in this step, ie the element at the depth param position in each list.
        val pathsSliced = pathsIndex.map(l => removeLastMax(l, depth))

        //go to the depth -1 position
        loop(mapParent = mapParentPostTreesStep, depth - 1, pathsSliced)
      }
    }

    //get all the paths of the index files thanks to the keys of the mapIndex
    val listIndex = mapIndex.keys.toList

    //keep just the list of the paths cut in list ont the file separator
    val separatorSplit = Pattern.quote(System.getProperty("file.separator"))
    val listPathsIndex = listIndex.map(_.split(separatorSplit).toList)

    //sort the list by the greatest number of directories in each path
    val listSorted = listPathsIndex.sortBy(f => f.length).reverse

    //retrieve the max depth with the list sorted
    val depthMax = listSorted.head.length

    //recursion
    loop(Map(), depthMax, listSorted)
  }

  /**
   *
   * @param mapParent         : the mapPent which will be updated
   * @param listPathsParents  : list of the path of the parents directory of the linesChildren
   * @param listLinesChildren : lines which will be added in the value each path parent of the listPathsParents in the mapParent
   * @return the mapParent updated for a list of a path parent corresponding to a list of children
   */
  @tailrec
  def updateMapParent(mapParent: Map[String, List[String]], listPathsParents: List[String], listLinesChildren: List[String]): Map[String, List[String]] = {
    if (listPathsParents == Nil) {
      mapParent
    } else {
      val mapParentUpdated = updateMapParentElement(mapParent, listPathsParents.head, listLinesChildren.head)
      updateMapParent(mapParentUpdated, listPathsParents.tail, listLinesChildren.tail)
    }
  }


  /**
   *
   * @param mapParent  : the map with the content of the trees
   * @param pathParent : path of the parent of the element
   * @param lineChild  : the element to add in the map parent
   * @return update the mapParent for one parent and one element
   */
  def updateMapParentElement(mapParent: Map[String, List[String]], pathParent: String, lineChild: String): Map[String, List[String]] = {
    if (mapParent.contains(pathParent)) {
      val oldListElements = mapParent(pathParent)
      val newListElements = oldListElements.patch(0, List(lineChild), 0)
      mapParent + (pathParent -> newListElements)
    } else {
      mapParent + (pathParent -> List(lineChild))
    }
  }

  /**
   *
   * @param path  : the list to reduce
   * @param depth : the current depth from the tree method
   * @return the list without the elements at the current depth
   */
  def removeLastMax(path: List[String], depth: Int): List[String] = {
    if (path.length == depth) {
      path.slice(0, path.length - 1)
    } else {
      path
    }
  }

  /**
   *
   * @param repoPath     : the path of the sgit repo
   * @param treesToWrite : the content of all the trees to write
   *                     this is an IO function, it will write the sgit objects
   */
  def IOWriteTreesObjects(repoPath: String, treesToWrite: List[List[String]]): Unit = {

    @tailrec
    def loop(treesToWriteUpdated: List[List[String]]): Unit = {

      treesToWriteUpdated match {

        case Nil =>
        case head :: tail =>

          //recovery the content of all the trees of the current step.
          val contentTree = head mkString "\n"
          SgitObjectUtil.createSgitObject(repoPath, contentTree)

          loop(tail)
      }
    }

    loop(treesToWrite)
  }

  /**
   *
   * @param repoPath      : the path of the sgit repo
   * @param pathBranch    : the path of the current branch file
   * @param shaTreeCommit : the sha of the main tree of this commit
   * @param message       : the message of the commit
   * @return if a commit object has been created, ie is there something new to commit (IO Function)
   */
  def IOWriteCommitObject(repoPath: String, pathBranch: String, shaTreeCommit: String, message: String): Boolean = {
    //check if it is the first commit or not and create the commit object
    if (new File(pathBranch).exists()) {
      if (shaTreeCommit == CommitUtil.getLastCommitTree(repoPath, BranchUtil.getCurrentBranchName(repoPath))) {
        false
      }
      else {
        val commitParent = readFileToList(pathBranch).head
        val contentCommit = "Tree " + shaTreeCommit + "\nParent " + commitParent + "\n\n" + message
        val shaCommit = createSgitObject(repoPath, contentCommit)
        editFile(pathBranch, shaCommit, append = false)
        true
      }
    }
    else {
      new File(pathBranch).createNewFile()
      val contentCommit = "Tree " + shaTreeCommit + "\n\n" + message
      val shaCommit = createSgitObject(repoPath, contentCommit)
      editFile(pathBranch, shaCommit, append = false)
      true
    }
  }
}
