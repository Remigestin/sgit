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
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    val testFilePathRel =  ".test" + File.separator + "test"
    val testFilePathRel2 =  ".test" + File.separator + "test2"
    val res = Status.getAllPathsUntracked(repoPath)

    assert(res.contains(testFilePathRel))
    assert(res.contains(testFilePathRel2))
  }

  it should "recover all indexed files edited but not added" in {

    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    val testFilePath =  repoPath + File.separator + ".test" + File.separator + "test"
    Add.add(repoPath,Seq(testFilePath))

    FileUtil.editFile(testFilePath,"this is an edit", append = true)

    val res = Status.getAllPathsTrackedModifiedNotAdd(repoPath)
    val pathRel = testFilePath.replace(repoPath + File.separator, "")

    assert(res.contains(pathRel))

  }

  it should "recover all files added but never committed (if there was 0 commit before)" in {
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    val testFilePath =  repoPath + File.separator + ".test" + File.separator + "test"
    Add.add(repoPath,Seq(testFilePath))

    val res = Status.getAllPathTrackedNeverCommitted(repoPath)
    val pathRel = testFilePath.replace(repoPath + File.separator, "")

    assert(res.contains(pathRel))

  }

  it should "recover all files added but never committed (if there was commits before)" in {
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    val testFilePath =  repoPath + File.separator + ".test" + File.separator + "test"
    val testFilePath2 =  repoPath + File.separator + ".test" + File.separator + "test2"
    Add.add(repoPath,Seq(testFilePath))
    Commit.commit(repoPath, "commit 1")

    Add.add(repoPath, Seq(testFilePath2))
    val res = Status.getAllPathTrackedNeverCommitted(repoPath)
    val pathRel2 = testFilePath2.replace(repoPath + File.separator, "")

    assert(res.contains(pathRel2))

  }

  it should "recover all files added, but different from the last commit (if there was 0 commit before)" in {
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    val testFilePath =  repoPath + File.separator + ".test" + File.separator + "test"
    Add.add(repoPath,Seq(testFilePath))
    val res = Status.getAllPathTrackedAndCommittedModified(repoPath)

    assert(res.isEmpty)

  }

  it should "recover all files added, but different from the last commit (if there was commits before)" in {

    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    val testFilePath =  repoPath + File.separator + ".test" + File.separator + "test"
    Add.add(repoPath,Seq(testFilePath))
    Commit.commit(repoPath, "commit 1")
    FileUtil.editFile(testFilePath, "ceci est une modif", append = true)
    Add.add(repoPath,Seq(testFilePath))

    val res = Status.getAllPathTrackedAndCommittedModified(repoPath)
    val pathRel = testFilePath.replace(repoPath + File.separator, "")

    assert(res.contains(pathRel))
  }



}
