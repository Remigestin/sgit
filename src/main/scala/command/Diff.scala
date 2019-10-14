package command

import scala.annotation.tailrec

object Diff {

  def Diff(repoPath: String): Unit = {


  }

  def ConstructMatrix(oldFile: List[String], newFile: List[String]): Map[(Int, Int), Int] = {

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


}
