package command

import java.io.File

import command.Diff.{constructMatrix, getDiffList}
import org.scalatest.{BeforeAndAfterEach, FlatSpec}
import util.FileUtil

import scala.reflect.io.Directory

class DiffTest extends FlatSpec with BeforeAndAfterEach {

  //init an sgit repo and .test repo before each test
  override def beforeEach(): Unit = {
    Repo.init(System.getProperty("user.dir"))
    new File(".test").mkdir()
    FileUtil.editFile(".test" + File.separator + "test", "Alia\nLire\nInes\nAlia", append = true)
    FileUtil.editFile(".test" + File.separator + "test2", "Bien\nAlia\nLire\nInes\nRemi\nEniram", append = true)
  }

  //delete all files created in the .sgit and .test directory after each test
  override def afterEach(): Unit = {
    val sgitPath = Repo.getRepoPath(System.getProperty("user.dir")).get + File.separator + ".sgit"
    val sgitDir = new Directory(new File(sgitPath))
    sgitDir.deleteRecursively()

    new Directory(new File(".test")).deleteRecursively()
  }

  "The diff command" should "create the good matrix of lcs" in {
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get

    val testFilePath = repoPath + File.separator + ".test" + File.separator + "test"
    val testFilePath2 = repoPath + File.separator + ".test" + File.separator + "test2"

    val contentTest = FileUtil.readFileToList(testFilePath)
    val contentTest2 = FileUtil.readFileToList(testFilePath2)

    val matrix = Diff.constructMatrix(contentTest,contentTest2)

    //(7 * 5)
    assert(matrix.toList.length == 35)
    assert(matrix(0,0) == 0)

    //alia / alia
    assert(matrix(1,2) == 1)


  }

  it should "create the good list of diff" in {



  }

}
