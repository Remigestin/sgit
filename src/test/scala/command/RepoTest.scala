package command

import java.io.File

import org.scalatest.{BeforeAndAfterEach, FlatSpec}
import util.FileUtil

import scala.reflect.io.Directory


class RepoTest extends FlatSpec with BeforeAndAfterEach {


  //init an sgit repo before each test
  override def beforeEach(): Unit = {
    Repo.init(System.getProperty("user.dir"))
  }

  //delete all files created in the .sgit directory after each test
  override def afterEach(): Unit = {
    val sgitPath = Repo.getRepoPath(System.getProperty("user.dir")).get + File.separator + ".sgit"
    val sgitDir = new Directory(new File(sgitPath))
    sgitDir.deleteRecursively()
  }


  "The init command" should "create the .sgit directory with the right structure" in {
    assert(new File(".sgit").exists())
    assert(new File(".sgit" + File.separator + "objects").exists())
    assert(new File(".sgit" + File.separator + "branches").exists())
    assert(new File(".sgit" + File.separator + "tags").exists())
    assert(new File(".sgit" + File.separator + "HEAD").exists())
  }

  it should "put the right content in the HEAD file" in {
    val pathHead = ".sgit" + File.separator + "HEAD"
    val content = FileUtil.readFileToList(pathHead) mkString "\n"
    assert(content == "branches" + File.separator + "master")
  }

  it should "check if current directory is already initialized with .sgit" in {
    assert(Repo.isInASgitRepo(System.getProperty("user.dir")))
  }

  it should "check if a directory is in a sgit repository" in {
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    val pathTest =repoPath + File.separator + "testSgit"
    new File(pathTest).mkdir()
    assert(Repo.isInASgitRepo(pathTest))
    assert(!Repo.isInASgitRepo(""))

    new Directory(new File(pathTest)).deleteRecursively()
  }

  it should "get the repository path of a directory" in {
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    assert(repoPath  == System.getProperty("user.dir"))

    val pathTest = System.getProperty("user.dir") + File.separator + "testSgit"
    new File(pathTest).mkdir()
    val repoTestPath = Repo.getRepoPath(pathTest).get
    assert(repoTestPath  == System.getProperty("user.dir"))

    new Directory(new File(pathTest)).deleteRecursively()

  }

  it should "not initialize a directory if it is already initialized" in {
    assert(Repo.init(System.getProperty("user.dir")) == "This directory was already initialized with Sgit")
  }

}

