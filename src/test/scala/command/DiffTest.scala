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

  //TODO : the diff test
  "The diff command" should "return the good diffs" in {
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get

    val testFilePath = repoPath + File.separator + ".test" + File.separator + "test"
    val testFilePath2 = repoPath + File.separator + ".test" + File.separator + "test2"

    Add.add(repoPath, Seq(testFilePath, testFilePath2))

    FileUtil.editFile(testFilePath, "\nET AUSSI ALIAAAAA CA CEST UNE DIFF",append = true)
    FileUtil.editFile(testFilePath2, "j ai remis a 0 ton fichier!", append = false)
  }

}
