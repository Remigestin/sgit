package command

import java.io.File._
import java.util.regex.Pattern

import util.IndexUtil._
import util.FileUtil._
import util.IndexUtil

import scala.annotation.tailrec


object Commit {

  def commit(): String = {

    //get all the lines of the index files
    val listIndex = readIndexToList()

    //keep just the list of the paths cut in array.
    val separator = Pattern.quote(System.getProperty("file.separator"))
    val listPathsIndex = listIndex.map(l => l.split(" ")(1).split(separator))

    //sort the list by the greatest number of directories in each path
    val listSorted = listPathsIndex.sortBy(f => f.length).reverse

    val mapIndex = readIndexToMap()

    println(readIndexToMap())


    ""

  }

  /**
   *
   *
   * @param pathsIndex : List of all paths in the index files sorted by number of dir.
   * @param depth      : Size of the largest path in number of directories
   * @param mapParent  : map which represent each path of directories referencing their future content whi is updated
   * @param mapIndex   : map of the index file
   * @param repo       : path of the sgit repo
   * @return the sha1 main tree
   */
  @tailrec
  def tree(pathsIndex: List[Array[String]], depth: Int, mapParent: Map[String, Array[String]] = Map(), mapIndex: Map[String, String], repo: String): String = {
    if (depth == 0) {
      //---- here we can create the main tree

      //retrieve his content and we sha1 it
      val contentTreeCommit = mapParent("") mkString "\n"
      val sha = sha1Hash(contentTreeCommit)

      //create the file
      val treeFilePath = repo + separator + "objects" + separator + sha
      createFile(path = treeFilePath, content = contentTreeCommit)

      //return the sha of the main tree
      sha
    } else {

      //filter only the paths which match with the current val of size param and remove the duplicate ones, and string it

      val pathsCurrentSize = pathsIndex
        .filter(arr => arr.length == depth)
        .map(arr => arr mkString separator)
        .distinct

      //---- STEP OF THE CREATION OF BLOB LINES

      //filter only the blob (if it is in the index file)
      val blobs = pathsCurrentSize.filter(f => mapIndex.contains(f))

      //recover the paths of the parents directory for each blob
      val pathsBlobParent = blobs.map(blob => blob.split(separator).slice(0, blob.split(separator).length - 1) mkString separator)

      //creation of the line of the blob in the future tree file with this pattern :  "blob sha1 name"
      val linesBlobChildren = blobs.map(blob => "blob " + mapIndex(blob) + " " + blob.split(separator).last)

      //update the map of the parents with the lines children created
      val mapParentPostBlobsStep = updateMapParent(mapParent, pathsBlobParent, linesBlobChildren)

      //----  STEP OF THE CREATION OF TREE LINES

      //filter only the directories (if it is in the index file)
      val pathsDir = pathsCurrentSize.filter(f => !mapIndex.contains(f))

      //recover the paths of the parents directory for each directory
      val pathsDirParent = pathsDir.map(tree => tree.split(separator).slice(0, tree.split(separator).length - 1) mkString separator)

      //creation of the line of the tree in the future parent tree file with this pattern :  "tree sha1 name"
      val linesTreeChildren = pathsDir.map(dir => "tree " + sha1Hash(mapParentPostBlobsStep(dir) mkString "\n") + " " + dir.split(separator).last)

      //update the map of the parents with the lines trees created
      val mapParentPostTreesStep = updateMapParent(mapParentPostBlobsStep, pathsDirParent, linesTreeChildren)


      //--- STEP OF WRITING SGIT OBJECTS

      //recovery the content of all the trees of the current step.
      val contentTreeList = pathsDir.map(d => mapParentPostTreesStep(d) mkString "\n")

      //sha1 and write the tree file
      contentTreeList.map(content => createFile(repo + separator + "objects" + separator + sha1Hash(content), content))

      //remove the elements created in this step, ie the element at the size param position in each array.
      val pathsSliced = pathsIndex.map(arr => removeLastMax(arr, depth))

      //go to the depth -1 position
      tree(pathsIndex = pathsSliced, depth = depth - 1, mapIndex = mapIndex, repo = repo, mapParent = mapParentPostTreesStep)
    }

  }

  /**
   *
   * @param mapParent         : the mapPent which will be updated
   * @param listPathsParents  : list of the path of the parents directory of the linesChildren
   * @param listLinesChildren : lines which will be added in the value each path parent of the listPathsParents in the mapParent
   * @return the mapParent updated for a list of a path parent corresponding to a list of children
   */
  @tailrec
  def updateMapParent(mapParent: Map[String, Array[String]], listPathsParents: List[String], listLinesChildren: List[String]): Map[String, Array[String]] = {
    if (listPathsParents == Nil) {
      mapParent
    } else {
      val mapParentUpdated = updateMapParentElement(mapParent, listPathsParents.head, listLinesChildren.head)
      updateMapParent(mapParentUpdated, listPathsParents.tail, listLinesChildren.tail)
    }
  }


  /**
   *
   * @param mapParent
   * @param pathParent
   * @param lineChild
   * @return update the mapParent updated for one parent and one element
   */
  def updateMapParentElement(mapParent: Map[String, Array[String]], pathParent: String, lineChild: String): Map[String, Array[String]] = {
    if (mapParent.contains(pathParent)) {
      val oldListElements = mapParent(pathParent)
      val newListElements = oldListElements.patch(0, Array(lineChild), 0)
      mapParent + (pathParent -> newListElements)
    } else {
      mapParent + (pathParent -> Array(lineChild))
    }
  }

  /**
   *
   * @param tab   : the tab to slice
   * @param depth : the current size from the tree method
   * @return the array tab without the elements at the current size position.
   */
  def removeLastMax(tab: Array[String], depth: Int): Array[String] = {
    if (tab.length == depth) {
      tab.slice(0, tab.length - 1)
    } else {
      tab
    }
  }
}
