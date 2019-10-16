package command

import java.io.File

import org.scalatest.{BeforeAndAfterEach, FlatSpec}
import util.FileUtil

import scala.reflect.io.Directory

class LogTest extends FlatSpec with BeforeAndAfterEach {

  //init an sgit repo and .test repo before each test
  override def beforeEach(): Unit = {
    Repo.init(System.getProperty("user.dir"))
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    new File(".test").mkdir()
    FileUtil.editFile(".test" + File.separator + "test", "Hello World", append = true)
    FileUtil.editFile(".test" + File.separator + "test2", "hello, world", append = true)


  }

  //delete all files created in the .sgit and .test directory after each test
  override def afterEach(): Unit = {
    val sgitPath = Repo.getRepoPath(System.getProperty("user.dir")).get + File.separator + ".sgit"
    val sgitDir = new Directory(new File(sgitPath))
    sgitDir.deleteRecursively()

    new Directory(new File(".test")).deleteRecursively()
  }

  "The commande log" should "return the goot content" in {
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    val testFilePath =  repoPath + File.separator + ".test" + File.separator + "test"
    val testFilePath2 =  repoPath + File.separator + ".test" + File.separator + "test2"

    Add.add(repoPath,Seq(testFilePath))
    Commit.commit(repoPath, "message")
    Add.add(repoPath, Seq(testFilePath2))
    Commit.commit(repoPath, "commit numero 2")
    println(Log.log(repoPath))

  }

}
