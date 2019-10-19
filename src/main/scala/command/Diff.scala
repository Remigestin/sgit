package command

import java.io.File

import util.{FileUtil, IndexUtil, SgitObjectUtil}

import scala.annotation.tailrec

class FilesToDiff(val contentNewFile: List[String], val contentOldFile: List[String], val nameFileToPrint: String)

object Diff {

  def diff(repoPath: String): String = {

    //--------------- IO READING STEP
    val listIndex = IndexUtil.readIndexToMap(repoPath).toList
    val listFilesToDiff = getListFilesToDiff(repoPath, listIndex)


    //--------------------- PURE FUNCTIONAL STEP

    getDiffAllFiles(listFilesToDiff, repoPath)
  }

  /**
   *
   * @param repoPath  : the path of the sgit repo
   * @param listIndex : the index in the form List[(src, blob)]
   * @return the list of all the FilesToDiff wanted (IO Read)
   */
  def getListFilesToDiff(repoPath: String, listIndex: List[(String, String)]): List[FilesToDiff] = {

    @tailrec
    def loop(listResult: List[FilesToDiff], listIndexCurrent: List[(String, String)]): List[FilesToDiff] = {

      listIndexCurrent match {

        case Nil => listResult
        case head :: tail =>

          val contentNewFile = FileUtil.readFileToList(repoPath + File.separator + head._1)
          val contentOldFile = SgitObjectUtil.readSgitObjectToList(repoPath, head._2)
          val nameFileToPrint = head._1

          val fileToDiff = new FilesToDiff(contentNewFile, contentOldFile, nameFileToPrint)

          val listResultUpdated = fileToDiff :: listResult

          loop(listResultUpdated, tail)
      }
    }

    loop(List(), listIndex)
  }


  /**
   *
   * @param newFile : content of the new file to diff
   * @param oldFile : content of the old file to diff
   * @return the matrix for the LCS Algorithm in order to have the longest common subsequence
   */
  def constructMatrix(newFile: List[String], oldFile: List[String]): Map[(Int, Int), Int] = {

    @tailrec
    def loop(mapMatrix: Map[(Int, Int), Int], newLine: Int, oldLine: Int): Map[(Int, Int), Int] = {

      //if all the matrix is completed, return it
      if (newLine == newFile.length + 1) {
        mapMatrix
      }

      //if a line of the matrix is completed, go the next line
      else if (oldLine == oldFile.length + 1) {
        loop(mapMatrix, newLine + 1, 0)
      }

      //fill the first line and the first column with 0
      else if (newLine == 0 || oldLine == 0) {
        val newMapMatrix = mapMatrix + ((newLine, oldLine) -> 0)
        loop(newMapMatrix, newLine, oldLine + 1)
      }

      else {

        val contentNewLine = newFile(newLine - 1)
        val contentOldLine = oldFile(oldLine - 1)

        if (contentNewLine == contentOldLine) {
          val value = mapMatrix((newLine - 1, oldLine - 1)) + 1
          val newMapMatrix = mapMatrix + ((newLine, oldLine) -> value)
          loop(newMapMatrix, newLine, oldLine + 1)
        } else {
          val value = Integer.max(mapMatrix(newLine, oldLine - 1), mapMatrix(newLine - 1, oldLine))
          val newMapMatrix = mapMatrix + ((newLine, oldLine) -> value)
          loop(newMapMatrix, newLine, oldLine + 1)
        }
      }
    }


    loop(Map(), 0, 0)
  }

  /**
   *
   * @param mapMatrix   : the matrix of the LCS algorithm
   * @param sizeNewFile : nb line of the new file to diff (height of the matrix)
   * @param sizeOldFile : nb line of the old file to diff (width of the matrix)
   * @return the list of all the differences between the two files (+/-, numLine)
   */
  def getDiffList(mapMatrix: Map[(Int, Int), Int], sizeNewFile: Int, sizeOldFile: Int): List[(String, Int)] = {

    @tailrec
    def loop(newLine: Int, oldLine: Int, listRep: List[(String, Int)]): List[(String, Int)] = {

      //if all the matrix is read, return the listRep
      if (newLine == 0 && oldLine == 0) {
        listRep
      }

      //if  the cursor is at the top of the matrix, go left
      else if (newLine == 0) {
        val newList = ("-", oldLine) :: listRep
        loop(newLine, oldLine - 1, newList)
      }

      // if the cursor is at the left of the matrix, go up
      else if (oldLine == 0) {
        val newList = ("+", newLine) :: listRep
        loop(newLine - 1, oldLine, newList)
      }

      //if the element at the left is equal to the element at the top
      else if (mapMatrix((newLine, oldLine - 1)) == mapMatrix(newLine - 1, oldLine)) {

        // if the element at the top left is equal to us - 1, go to this element
        if (mapMatrix(newLine, oldLine) - 1 == mapMatrix(newLine - 1, oldLine - 1)) {
          loop(newLine - 1, oldLine - 1, listRep)
        }
        //else go to the left
        else {
          val newList = ("-", oldLine) :: listRep
          loop(newLine, oldLine - 1, newList)
        }
      }


      //if the element at the left is not equal the element at the top
      else {

        //if the element at the top is greater than the element at the left, go up
        if (mapMatrix(newLine - 1, oldLine) > mapMatrix(newLine, oldLine - 1)) {
          val newList = ("+", newLine) :: listRep
          loop(newLine - 1, oldLine, newList)
        }
        //if the element at the left is greater than the element at the top, go left
        else {
          val newList = ("-", oldLine) :: listRep
          loop(newLine, oldLine - 1, newList)
        }
      }
    }

    loop(sizeNewFile, sizeOldFile, List())
  }

  /**
   *
   * @param listDiff : list of all the differences for one diff (+/-, numLine)
   * @param newFile  : content of the new file
   * @param oldFile  : content of the old file
   * @return the string to print for the difference of one file
   */
  def getDiffStringOneFile(listDiff: List[(String, Int)], newFile: List[String], oldFile: List[String]): String = {

    @tailrec
    def loop(result: String, listDiffUpdated: List[(String, Int)]): String = {

      listDiffUpdated match {
        case Nil => result

        case head :: tail =>
          if (head._1 == "+") {
            val newResult = result + Console.GREEN + "l." + head._2 + " ++ " + newFile(head._2 - 1) + "\n" + Console.RESET
            loop(newResult, tail)
          } else {
            val newResult = result + Console.RED + "l." + head._2 + " -- " + oldFile(head._2 - 1) + "\n" + Console.RESET
            loop(newResult, tail)
          }
      }
    }

    loop("", listDiff)
  }

  /**
   *
   * @param filesToDiff : list of all the diff to effectuate
   * @param repoPath    : path of the sgit repo
   * @return the string of all the diffs for all the filesToDiff
   */
  def getDiffAllFiles(filesToDiff: List[FilesToDiff], repoPath: String): String = {

    @tailrec
    def loop(filesToDiffCurrent: List[FilesToDiff], result: String): String = {

      filesToDiffCurrent match {
        case Nil => result
        case head :: tail =>

          val matrix = constructMatrix(head.contentNewFile, head.contentOldFile)
          val listDif = getDiffList(matrix, head.contentNewFile.length, head.contentOldFile.length)

          if (listDif.nonEmpty) {
            val newResult = result + head.nameFileToPrint.replace(repoPath + File.separator, "") + " :\n" + getDiffStringOneFile(listDif, head.contentNewFile, head.contentOldFile) + "\n\n"
            loop(tail, newResult)
          } else {
            loop(tail, result)
          }
      }
    }

    loop(filesToDiff, "")

  }

  /**
   *
   * @param filesToDif : Paths to diffs
   * @param repoPath   : the path of the sgit repo
   * @return the string of all the diffs for all the filesToDiff
   */
  def getDiffStatAllFiles(filesToDif: List[FilesToDiff], repoPath: String): String = {

    @tailrec
    def loop(filesToDifCurrent: List[FilesToDiff], result: String, sumFiles: Int, sumAddition: Int, sumDeletion: Int): String = {

      filesToDifCurrent match {
        case Nil => result + sumFiles + " files changed, " + sumAddition + " insertions(+), " + sumDeletion + " deletions(-)\n"
        case head :: tail =>

          val matrix = constructMatrix(head.contentNewFile, head.contentOldFile)
          val listDif = getDiffList(matrix, head.contentNewFile.length, head.contentOldFile.length)

          if (listDif.nonEmpty) {
            val nbAddition = listDif.count(_._1 == "+")
            val sumAdditionUpdated = sumAddition + nbAddition

            val nbDeletion = listDif.count(_._1 == "-")
            val sumDeletionUpdated = sumDeletion + nbDeletion

            val sumFilesUpdated = sumFiles + 1

            val sumAdditionDeletion = nbAddition + nbDeletion

            val resultUpdated = result + head.nameFileToPrint.replace(repoPath + File.separator, "") + " | " + sumAdditionDeletion + " (" + Console.GREEN + nbAddition + "++" + Console.RESET + " " + Console.RED + nbDeletion + "--" + Console.RESET + ")" + "\n"
            loop(tail, resultUpdated, sumFilesUpdated, sumAdditionUpdated, sumDeletionUpdated)

          }
          else {
            loop(tail, result, sumFiles, sumAddition, sumDeletion)
          }
      }
    }


    loop(filesToDif, "", 0, 0, 0)
  }


}
