package command

import java.io.{File, FileOutputStream, FileWriter, PrintWriter}
import java.nio.file.{Files, Paths, StandardCopyOption}

import util.FileUtil._
import util.IndexUtil._
import util.SgitObjectUtil

import scala.annotation.tailrec
import scala.io.Source

object Add {

  /**
   *
   * @param repoPath : path of the sgit repo
   * @param files    : seq of the files to add
   *                 this function realise the sgit add command
   */
  def add(repoPath: String, files: Seq[String]): Unit = {


    // ---------------- IO READING STEP

    // create the index path if it's not
    getIndexPath(repoPath)

    //For each pattern after the sgit add, we retrieve the path of all the corresponding files
    val filesListCurDir = files.map(f => new File(f)).filter(_.isFile)
    val dirsListCurDir = files.map(f => new File(f)).filter(_.isDirectory)
    val allFilesListBrut = filesListCurDir ++ dirsListCurDir.flatMap(d => recursiveListFiles(d)).toList
    val pathsAllFilesList = allFilesListBrut.filter(_.isFile).filter(f => !f.getAbsolutePath.contains(".sgit")).map(_.getAbsolutePath)

    val otherFiles = files.map(new File(_)).filterNot(_.exists()).map(_.getAbsolutePath)


    //Create Blob file and edit index file for each path
    pathsAllFilesList.foreach(p => addBlob(repoPath, p.replace(repoPath + File.separator, "")))

    //remove the paths from the index if they was indexed
    otherFiles.foreach(p => removeIfPathAlreadyIndexed(repoPath, p.replace(repoPath + File.separator, "")))

  }

  //Create Blob file and edit index file for each path
  def addBlob(repoPath: String, path: String): Unit = {

    //retrieve content of the file
    val content = readFileToList(repoPath + File.separator + path) mkString "\n"

    //create the hash with the content of the file
    val hash = sha1Hash(content)

    //check if the file is not already indexed with the same content(hash)
    if (!isAlreadyIndexed(repoPath, hash, path)) {


      //if the blob does not exist, we create the blob file
      SgitObjectUtil.createSgitObject(repoPath, content)

      //update the index file
      updateIndex(repoPath, hash, path)
    }
  }

  def updateIndex(repoPath: String, hash: String, path: String): Unit = {

    //retrieve the path of the index file
    val indexPath = getIndexPath(repoPath)

    //remove the old line
    removeIfPathAlreadyIndexed(repoPath, path)

    //add in the index file the line with the hash and the path
    val lineBlob = hash + " " + path + "\n"
    editFile(indexPath, lineBlob, append = true)
  }

  def isAlreadyIndexed(repoPath: String, hash: String, path: String): Boolean = {
    val lines = readIndexToList(repoPath) mkString "\n"
    lines.contains(hash + " " + path)
  }

  def removeIfPathAlreadyIndexed(repoPath: String, path: String): Unit = {
    val lines = readIndexToList(repoPath)
    val indexString = lines mkString "\n"
    if (indexString.contains(path)) {
      val indexPath = getIndexPath(repoPath)
      val linesList = lines.filterNot(l => l.split(" ")(1) == path)
      if (linesList.isEmpty)
        editFile(indexPath, "", append = false)
      else
        editFile(indexPath, (linesList mkString "\n") + "\n", append = false)
    }
  }


}
