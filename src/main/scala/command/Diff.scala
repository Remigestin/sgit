package command

import java.io.File

import util.{FileUtil, IndexUtil, SgitObjectUtil}

import scala.annotation.tailrec

object Diff {

  def diff(repoPath: String): String = {

    val mapIndex = IndexUtil.readIndexToMap(repoPath)
    val listCouplesIndex = mapIndex.toList.map(c => (repoPath + File.separator + c._1, repoPath + File.separator + ".sgit" + File.separator + "objects" + File.separator + c._2))


    getDiffAllFiles(listCouplesIndex, repoPath)
  }


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


      //if the element at the left is not equal the element at the right
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

  def getDiffStringOneFile(listDiff: List[(String, Int)], newFile: List[String], oldFile: List[String]): String = {

    @tailrec
    def loop(result: String, listDiffUpdated: List[(String, Int)]): String = {

      listDiffUpdated match {
        case Nil => result

        case head :: tail =>
          if (head._1 == "+") {
            val newResult = result + "l." + head._2 + " ++ " + newFile(head._2 - 1) + "\n"
            loop(newResult, tail)
          } else {
            val newResult = result + "l." + head._2 + " -- " + oldFile(head._2 - 1) + "\n"
            loop(newResult, tail)
          }
      }
    }

    loop("", listDiff)
  }

  def getDiffAllFiles(pathsToDiff: List[(String, String)], repoPath: String): String = {

    @tailrec
    def loop(pathsToDiffCurrent: List[(String, String)], result: String): String = {

      pathsToDiffCurrent match {
        case Nil => result
        case head :: tail =>

          val newFile = FileUtil.readFileToList(head._1)
          val oldFile = FileUtil.readFileToList(head._2)

          val matrix = constructMatrix(newFile, oldFile)
          val listDif = getDiffList(matrix, newFile.length, oldFile.length)

          if (listDif.nonEmpty) {
            val newResult = result + head._1 + " :\n" + getDiffStringOneFile(listDif, newFile, oldFile) + "\n\n"
            loop(tail, newResult)
          } else {
            loop(tail, result + "\n\n")
          }
      }
    }

    loop(pathsToDiff, "")

  }


}
