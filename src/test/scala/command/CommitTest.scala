package command

import java.io.File

import org.scalatest.{BeforeAndAfterEach, FlatSpec, FunSuite}
import util.FileUtil

import scala.reflect.io.Directory

class CommitTest extends FlatSpec with BeforeAndAfterEach {

  //init an sgit repo and .test repo before each test
  override def beforeEach(): Unit = {
    Repo.init(System.getProperty("user.dir"))
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

  "A Commit" should "create the commit tree" in {
  }

  it should "create the branch in .sgit/branches if it is its first commit" in {
  }

  it should "not create commit tree if the previous commit is the same" in {
  }

  it should "create commit in .sgit/objects with the right content" in {
  }

  it should "create all trees and blobs of the commit tree in .sgit/objects" in {
  }

  it should "update the current branch with the commit" in {
  }

}
