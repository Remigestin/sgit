package command

import java.io.{File, FileOutputStream, FileWriter, PrintWriter}
import java.nio.file.{Files, Paths, StandardCopyOption}

import util.FileUtil._
import util.IndexUtil._
import util.SgitObjectUtil

import scala.annotation.tailrec
import scala.io.Source

object Add {

  def add(repoPath: String, files: Seq[String]): Unit = {


    // retrieve the index file path and create it if it is not
    val indexPath = getIndexPath(repoPath).get

    //For each pattern after the sgit add, we retrieve the path of all the corresponding files
    val filesListCurDir = files.map(f => new File(f)).filter(_.isFile)
    val dirsListCurDir = files.map(f => new File(f)).filter(_.isDirectory)
    val allFilesListBrut = filesListCurDir ++ dirsListCurDir.flatMap(d => recursiveListFiles(d)).toList
    val pathsAllFilesList = allFilesListBrut.filter(_.isFile).filter(f => !f.getAbsolutePath.contains(".sgit")).map(_.getAbsolutePath)

    //Create Blob file and edit index file for each path
    pathsAllFilesList.foreach(p => addBlob(repoPath, p.replace(repoPath + File.separator, "")))

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
    val indexPath = getIndexPath(repoPath).get

    //remove the old line
    removeIfPathAlreadyIndexed(repoPath, path)

    //add in the index file the line with the hash and the path
    val lineBlob = hash + " " + path + "\n"
    editFile(indexPath, lineBlob, append = true)
  }

  def isAlreadyIndexed(repoPath:String, hash: String, path: String): Boolean = {
    val lines = readIndexToList(repoPath) mkString "\n"
    lines.contains(hash + " " + path)
  }

  def removeIfPathAlreadyIndexed(repoPath: String, path: String): Unit = {
    val lines = readIndexToList(repoPath) mkString "\n"
    if (lines.contains(path)) {
      val indexPath = getIndexPath(repoPath).get
      val linesList = lines.split("\n").toList.filter(l => !l.contains(path))
      val content = linesList mkString "\n"
      editFile(indexPath, content, append = true)
    }
  }


}
