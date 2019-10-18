package command

import java.io.{File, FileOutputStream, FileWriter, PrintWriter}
import java.nio.file.{Files, Paths, StandardCopyOption}

import util.FileUtil._
import util.IndexUtil._
import util.SgitObjectUtil

import scala.annotation.tailrec
import scala.io.Source

//type filesToIndex to check
class FilesToIndex(val pathRelative: String, val content: String)

object Add {

  /**
   *
   * @param repoPath : path of the sgit repo
   * @param files    : seq of the files to add
   *                 this function realise the sgit add command
   */
  def add(repoPath: String, files: Seq[String]): Unit = {


    //----------------------- IO READING STEP

    // create the index path if it's not
    val indexPath = getIndexPath(repoPath)
    val linesIndex = readIndexToList(repoPath)

    //retrieve all the files asked in the type FilesToIndex (see implementation higher up)
    val filesToIndexList = getListFilesToIndex(repoPath, files)

    //retrieve all the files which does not exist for a  future deletion if the path is in the index
    val otherFiles = files.map(new File(_)).filterNot(_.exists()).map(_.getAbsolutePath)



    //--------------------- PURE FUNCTIONAL STEP

    //filter the blob to create
    val blobsToCreate = filesToIndexList.filterNot(isAlreadyIndexedWithSameContent(repoPath, _, linesIndex))



    //---------------------- IO WRITING STEP

    //create the blob and update the index for each new blob
    blobsToCreate.foreach(f => SgitObjectUtil.createSgitObject(repoPath, f.content))
    blobsToCreate.foreach(f => updateIndex(repoPath, indexPath, linesIndex, f))

    //remove the paths from the index if they was indexed
    otherFiles.foreach(p => removeIfPathAlreadyIndexed(repoPath, p.replace(repoPath + File.separator, ""), linesIndex))

  }


  /**
   *
   * @param repoPath   : the path of the sgit repo
   * @param file       : the file to check
   * @param linesIndex : the content of index in list of lines
   * @return if the file is already indexed with the same content
   */
  def isAlreadyIndexedWithSameContent(repoPath: String, file: FilesToIndex, linesIndex: List[String]): Boolean = {
    val hash = sha1Hash(file.content)
    linesIndex.contains(hash + " " + file.pathRelative)
  }

  /**
   *
   * @param repoPath : the path of the Sgit repo
   * @param files    : the names of the files pass in arg by the user
   * @return a list of FilesToIndex of all the files asked (IO Read)
   */
  def getListFilesToIndex(repoPath: String, files: Seq[String]): Seq[FilesToIndex] = {

    //For each pattern after the sgit add, we retrieve the path of all the corresponding files
    val filesListCurDir = files.map(f => new File(f)).filter(_.isFile)

    //if a dir is passed in param, retrieve all the files of the dir recursively and add it to the list filesListCurDir
    val dirsListCurDir = files.map(f => new File(f)).filter(_.isDirectory)
    val allFilesListBrut = filesListCurDir ++ dirsListCurDir.flatMap(d => recursiveListFiles(d))

    //filter the .sgit dir
    val pathsAllFilesListFilter = allFilesListBrut.filter(_.isFile).filter(f => !f.getAbsolutePath.contains(".sgit"))

    //pass the files to the type FilesToIndex
    pathsAllFilesListFilter.map(f => new FilesToIndex(f.getAbsolutePath.replace(repoPath + File.separator, ""), readFileToList(f.getAbsolutePath) mkString "\n"))
  }

  /**
   *
   * @param repoPath  : the path of the sgit repo
   * @param indexPath : the path of the index
   * @param file      : the file to add/update
   *                  update the index file with the file in param (IO Write)
   */
  def updateIndex(repoPath: String, indexPath: String, linesIndex: List[String], file: FilesToIndex): Unit = {

    //remove from the index the old line corresponding to the path of the file
    removeIfPathAlreadyIndexed(repoPath, file.pathRelative, linesIndex)

    val hash = sha1Hash(file.content)

    //add in the index file the line with the hash and the path
    val lineBlob = hash + " " + file.pathRelative + "\n"
    editFile(indexPath, lineBlob, append = true)
  }

  /**
   *
   * @param repoPath : the path of the sgit repo
   * @param path     : the path of the file to remove of the index
   *                 remove the line corresponding to the path in the index (IO Write)
   */
  def removeIfPathAlreadyIndexed(repoPath: String, path: String, linesIndex: List[String]): Unit = {

    val indexString = linesIndex mkString "\n"
    if (indexString.contains(path)) {
      val indexPath = getIndexPath(repoPath)
      val linesList = linesIndex.filterNot(l => l.split(" ")(1) == path)
      if (linesList.isEmpty)
        editFile(indexPath, "", append = false)
      else
        editFile(indexPath, (linesList mkString "\n") + "\n", append = false)
    }
  }
}
