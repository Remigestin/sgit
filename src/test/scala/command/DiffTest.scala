package command

import java.io.File

import org.scalatest.{BeforeAndAfterEach, FlatSpec}
import util.FileUtil

import scala.reflect.io.Directory

class DiffTest extends FlatSpec with BeforeAndAfterEach {

  //init an sgit repo and .test repo before each test
  override def beforeEach(): Unit = {
    Repo.init(System.getProperty("user.dir"))
    new File(".test").mkdir()
    FileUtil.editFile(".test" + File.separator + "test", "A\nL\nI\nA", append = true)
    FileUtil.editFile(".test" + File.separator + "test2", "B\nA\nL\nI\nR\nE", append = true)
  }

  //delete all files created in the .sgit and .test directory after each test
  override def afterEach(): Unit = {
    val sgitPath = Repo.getRepoPath(System.getProperty("user.dir")).get + File.separator + ".sgit"
    val sgitDir = new Directory(new File(sgitPath))
    sgitDir.deleteRecursively()

    new Directory(new File(".test")).deleteRecursively()
  }

  "The function constructMatrix" should "create the matrix of the lcs algo" in {
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get

    val testFilePath = repoPath + File.separator + ".test" + File.separator + "test"

    val testFilePath2 = repoPath + File.separator + ".test" + File.separator + "test2"

   println(Diff.ConstructMatrix(FileUtil.readFileToList(testFilePath), FileUtil.readFileToList(testFilePath2)))

  }

}
