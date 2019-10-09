package command

import java.io.{File, FileOutputStream, FileWriter, PrintWriter}
import java.nio.file.{Files, Paths, StandardCopyOption}

import util.FileUtil
import util.IndexUtil._

import scala.annotation.tailrec
import scala.io.Source

object Add {

  def add(files: Seq[File]): String = {

    // retrieve the index file path
    val indexPath = Repo.getIndexPath(System.getProperty("user.dir")).get

    //For each pattern after the sgit add, we retrieve the path of all the corresponding files
    val pathList = getListPaths(files)


    //Create Blob file and edit index file for each path
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get


    pathList.foreach(p => addBlob(p.replace(repoPath + File.separator, "")))

    "hey"
  }

  //TODO : glob
  //For each pattern after the sgit add, we get the path of all the corresponding files
  def getListPaths(files: Seq[File]): List[String] = {
    @tailrec
    def loop(listPath: List[String], f: Seq[File]): List[String] = {
      f match {
        case Nil => listPath
        case head :: tail =>
          val list = FileUtil.recursiveListFiles(new File(System.getProperty("user.dir")), head.getName.r).filter(a => a.isFile).map(a => a.getAbsolutePath)
          loop(listPath ++ list, tail)
      }
    }

    loop(List[String](), files)
  }

  //Create Blob file and edit index file for each path
  def addBlob(path: String): Unit = {

    //retrieve content of the file
    val source = scala.io.Source.fromFile(Repo.getRepoPath(System.getProperty("user.dir")).get + File.separator + path)
    val lines = try source.getLines mkString "\n" finally source.close()

    //create the hash with the content of the file
    val hash = FileUtil.sha1Hash(lines)

    //check if the file is not already indexed with the same content(hash)
    if (!isAlreadyIndexed(hash, path)) {

      //create the path of the blob file
      val dirSgit = Repo.getSgitPath(System.getProperty("user.dir")).get
      val blobPath = dirSgit + File.separator + "objects" + File.separator + hash

      //if the blob does not exist, we create the blob file
      if (!new File(blobPath).exists()) {

        //create the blob file
        new File(blobPath).createNewFile()

        //fill the blob file
        val fw = new FileWriter(blobPath, true)
        fw.write(lines)
        fw.close()
      }

      //update the index file
      updateIndex(hash, path)
    }
  }

  def updateIndex(hash: String, path: String): Unit = {

    //retrieve the path of the index file
    val indexPath = Repo.getIndexPath(System.getProperty("user.dir")).get

    //remove the old line
    removeIfPathAlreadyIndexed(path)

    //add in the index file the line with the hash and the path
    val fw = new FileWriter(indexPath, true)
    fw.write(hash + " " + path + "\n")
    fw.close()

  }

  def isAlreadyIndexed(hash: String, path: String): Boolean = {
    val lines = readIndexToList() mkString "\n"
    lines.contains(hash + " " + path)
  }

  def removeIfPathAlreadyIndexed(path: String): Unit = {

    val lines = readIndexToList() mkString "\n"

    if (lines.contains(path)) {
      val indexPath = Repo.getIndexPath(System.getProperty("user.dir")).get
      val linesList = lines.split("\n").toList.filter(l => !l.contains(path))
      val fw = new FileWriter(indexPath, false)
      linesList.foreach(ll => fw.write(ll + "\n"))
      fw.close()

    }
  }


}
