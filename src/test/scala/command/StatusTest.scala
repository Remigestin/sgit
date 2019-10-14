package command

import java.io.File
import java.nio.file.Paths

import org.scalatest.{BeforeAndAfterEach, FlatSpec, FunSuite}
import util.FileUtil

import scala.reflect.io.Directory

class StatusTest extends FlatSpec with BeforeAndAfterEach {

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

  "The status command" should "recover all the files untracked" in {
    val curdir = System.getProperty("user.dir")
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    val testFilePath =  repoPath + File.separator + ".test" + File.separator + "test"
    val testFilePath2 =  repoPath + File.separator + ".test" + File.separator + "test2"
    val res = Status.getAllPathsUntracked(repoPath, curdir)

    val pathRel = Paths.get(curdir).relativize(Paths.get(testFilePath)).toString
    val pathRel2 = Paths.get(curdir).relativize(Paths.get(testFilePath2)).toString

    assert(res.contains(pathRel))
    assert(res.contains(pathRel2))
  }

  it should "recover all indexed files edited but not added" in {
    val curdir = System.getProperty("user.dir")
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    val testFilePath =  repoPath + File.separator + ".test" + File.separator + "test"
    Add.add(repoPath,Seq(testFilePath))

    FileUtil.editFile(testFilePath,"this is an edit", append = true)

    val res = Status.getAllPathsTrackedModifiedNotAdd(repoPath, curdir)
    val pathRel = Paths.get(curdir).relativize(Paths.get(testFilePath)).toString

    assert(res.contains(pathRel))

  }

  it should "recover all files added but never committed (if there was 0 commit before)" in {
    val curdir = System.getProperty("user.dir")
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    val testFilePath =  repoPath + File.separator + ".test" + File.separator + "test"
    Add.add(repoPath,Seq(testFilePath))

    val res = Status.getAllPathTrackedNeverCommitted(repoPath, curdir)
    val pathRel = Paths.get(curdir).relativize(Paths.get(testFilePath)).toString

    assert(res.contains(pathRel))

  }

  it should "recover all files added but never committed (if there was commits before)" in {
    val curdir = System.getProperty("user.dir")
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    val testFilePath =  repoPath + File.separator + ".test" + File.separator + "test"
    val testFilePath2 =  repoPath + File.separator + ".test" + File.separator + "test2"
    Add.add(repoPath,Seq(testFilePath))
    Commit.commit(repoPath, "commit 1")

    Add.add(repoPath, Seq(testFilePath2))
    val res = Status.getAllPathTrackedNeverCommitted(repoPath, curdir)
    val pathRel2 = Paths.get(curdir).relativize(Paths.get(testFilePath2)).toString

    assert(res.contains(pathRel2))

  }

  it should "recover all files added, but different from the last commit (if there was 0 commit before)" in {
    val curdir = System.getProperty("user.dir")
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    val testFilePath =  repoPath + File.separator + ".test" + File.separator + "test"
    Add.add(repoPath,Seq(testFilePath))
    val res = Status.getAllPathTrackedAndCommittedModified(repoPath, curdir)


    assert(res.isEmpty)

  }

  it should "recover all files added, but different from the last commit (if there was commits before)" in {
    val curDir = System.getProperty("user.dir")
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    val testFilePath =  repoPath + File.separator + ".test" + File.separator + "test"
    Add.add(repoPath,Seq(testFilePath))
    Commit.commit(repoPath, "commit 1")
    FileUtil.editFile(testFilePath, "ceci est une modif", append = true)
    Add.add(repoPath,Seq(testFilePath))

    val res = Status.getAllPathTrackedAndCommittedModified(repoPath, curDir)
    val pathRel = Paths.get(curDir).relativize(Paths.get(testFilePath)).toString

    assert(res.contains(pathRel))
  }

  it should "recover all files in the index but which not exists anymore (with one file)" in {

    val curDir = System.getProperty("user.dir")
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    val testFilePath =  repoPath + File.separator + ".test" + File.separator + "test"
    Add.add(repoPath,Seq(testFilePath))

    new File(testFilePath).delete()

    val res = Status.getAllDeletionsNotStaged(repoPath, curDir)

    assert(res.length == 1)
    assert(res.head == ".test" + File.separator + "test")
  }

  it should "recover all files in the index but which not exists anymore (with multi files)" in {

    val curDir = System.getProperty("user.dir")
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    val testFilePath =  repoPath + File.separator + ".test" + File.separator + "test"
    val testFilePath2 =  repoPath + File.separator + ".test" + File.separator + "test2"
    Add.add(repoPath,Seq(testFilePath, testFilePath2))

    new File(testFilePath).delete()

    val res = Status.getAllDeletionsNotStaged(repoPath, curDir)

    assert(res.length == 1)
    assert(res.head == ".test" + File.separator + "test")
  }



}
