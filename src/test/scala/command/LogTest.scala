package command

import java.io.File

import org.scalatest.{BeforeAndAfterEach, FlatSpec}
import util.{BranchUtil, CommitUtil, FileUtil, RepoUtil, SgitObjectUtil}

import scala.reflect.io.Directory

class LogTest extends FlatSpec with BeforeAndAfterEach {

  //init an sgit repo and .test repo before each test
  override def beforeEach(): Unit = {
    Init.init(System.getProperty("user.dir"))
    val repoPath = RepoUtil.getRepoPath(System.getProperty("user.dir")).get
    new File(".test").mkdir()
    FileUtil.editFile(".test" + File.separator + "test", "Hello World", append = true)
    FileUtil.editFile(".test" + File.separator + "test2", "deuxieme, fichier", append = true)


  }

  //delete all files created in the .sgit and .test directory after each test
  override def afterEach(): Unit = {
    val sgitPath = RepoUtil.getRepoPath(System.getProperty("user.dir")).get + File.separator + ".sgit"
    val sgitDir = new Directory(new File(sgitPath))
    sgitDir.deleteRecursively()

    new Directory(new File(".test")).deleteRecursively()
  }

  "The command log" should "return the good content" in {
    val repoPath = RepoUtil.getRepoPath(System.getProperty("user.dir")).get
    val testFilePath = repoPath + File.separator + ".test" + File.separator + "test"
    val testFilePath2 = repoPath + File.separator + ".test" + File.separator + "test2"

    Add.add(repoPath, Seq(testFilePath))
    Commit.commit(repoPath, "commit number 1")
    val shaCommit1 = CommitUtil.getLastCommitObject(repoPath, BranchUtil.getCurrentBranchName(repoPath)).get
    val contentCommit1 = SgitObjectUtil.readSgitObjectToList(repoPath, shaCommit1) mkString "\n"


    Add.add(repoPath, Seq(testFilePath2))
    Commit.commit(repoPath, "commit number 2")
    val shaCommit2 = CommitUtil.getLastCommitObject(repoPath, BranchUtil.getCurrentBranchName(repoPath)).get
    val contentCommit2 = SgitObjectUtil.readSgitObjectToList(repoPath, shaCommit2) mkString "\n"

    val listToTest = List((shaCommit1, contentCommit1), (shaCommit2, contentCommit2))

    val listRep = Log.getAllCommits(repoPath, shaCommit2)

    assert(listToTest == listRep)

  }


  it should "return the good content in option p" in {
    val repoPath = RepoUtil.getRepoPath(System.getProperty("user.dir")).get
    val testFilePath = repoPath + File.separator + ".test" + File.separator + "test"
    val testFilePath2 = repoPath + File.separator + ".test" + File.separator + "test2"

    Add.add(repoPath, Seq(testFilePath))
    Commit.commit(repoPath, "commit number 1")
    val shaCommit1 = CommitUtil.getLastCommitObject(repoPath, BranchUtil.getCurrentBranchName(repoPath)).get
    val contentCommit1 = SgitObjectUtil.readSgitObjectToList(repoPath, shaCommit1) mkString "\n"
    FileUtil.editFile(testFilePath, "\nthis is an append", append = true)

    Add.add(repoPath, Seq(testFilePath, testFilePath2))
    Commit.commit(repoPath, "commit number 2")
    val shaCommit2 = CommitUtil.getLastCommitObject(repoPath, BranchUtil.getCurrentBranchName(repoPath)).get
    val contentCommit2 = SgitObjectUtil.readSgitObjectToList(repoPath, shaCommit2) mkString "\n"

    new File(testFilePath).delete()

    Add.add(repoPath, Seq(testFilePath, testFilePath2))
    Commit.commit(repoPath, "commit number 3")


     assert(Log.log(repoPath, "patch").contains("l.1 -- Hello World"))

  }

  it should "return the good content in log option -s" in {
    val repoPath = RepoUtil.getRepoPath(System.getProperty("user.dir")).get
    val testFilePath = repoPath + File.separator + ".test" + File.separator + "test"
    val testFilePath2 = repoPath + File.separator + ".test" + File.separator + "test2"

    Add.add(repoPath, Seq(testFilePath))
    Commit.commit(repoPath, "commit number 1")
    val shaCommit1 = CommitUtil.getLastCommitObject(repoPath, BranchUtil.getCurrentBranchName(repoPath)).get
    val contentCommit1 = SgitObjectUtil.readSgitObjectToList(repoPath, shaCommit1) mkString "\n"
    FileUtil.editFile(testFilePath, "\nthis is an append", append = true)

    Add.add(repoPath, Seq(testFilePath, testFilePath2))
    Commit.commit(repoPath, "commit number 2")
    val shaCommit2 = CommitUtil.getLastCommitObject(repoPath, BranchUtil.getCurrentBranchName(repoPath)).get
    val contentCommit2 = SgitObjectUtil.readSgitObjectToList(repoPath, shaCommit2) mkString "\n"

    new File(testFilePath).delete()

    Add.add(repoPath, Seq(testFilePath, testFilePath2))
    Commit.commit(repoPath, "commit number 3")


    assert(Log.log(repoPath, "stat").contains("2 files changed, 2 insertions(+)"))
  }

}
