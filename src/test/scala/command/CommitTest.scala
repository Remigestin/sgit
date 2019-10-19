package command

import java.io.File

import org.scalatest.{BeforeAndAfterEach, FlatSpec, FunSuite}
import util.BranchUtil.getCurrentBranchPath
import util.{BranchUtil, CommitUtil, FileUtil}

import scala.reflect.io.Directory

class CommitTest extends FlatSpec with BeforeAndAfterEach {

  //init an sgit repo and .test repo before each test
  override def beforeEach(): Unit = {
    Repo.init(System.getProperty("user.dir"))
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    new File(".test").mkdir()
    FileUtil.editFile(".test" + File.separator + "test", "Hello World", append = true)
    FileUtil.editFile(".test" + File.separator + "test2", "hello, world", append = true)
    Add.add(repoPath,Seq(".test" + File.separator + "test"))
  }

  //delete all files created in the .sgit and .test directory after each test
  override def afterEach(): Unit = {
    val sgitPath = Repo.getRepoPath(System.getProperty("user.dir")).get + File.separator + ".sgit"
    val sgitDir = new Directory(new File(sgitPath))
    sgitDir.deleteRecursively()

    new Directory(new File(".test")).deleteRecursively()
  }

  "The command commit"  should "create the branch in .sgit/branches if it is its first commit" in {
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    val pathBranch = getCurrentBranchPath(repoPath)
    assert(!new File(pathBranch).exists())
    Commit.commit(repoPath,"commit")
    assert(new File(pathBranch).exists())
  }

  it should "not create commit object if the previous commit is the same" in {
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    Commit.commit(repoPath, "commit")
    val shaCommitObject = CommitUtil.getLastCommitObject(repoPath, BranchUtil.getCurrentBranchName(repoPath)).get
    Commit.commit(repoPath, "commit 2")
    val shaCommitObject2 = CommitUtil.getLastCommitObject(repoPath, BranchUtil.getCurrentBranchName(repoPath)).get

    assert(shaCommitObject == shaCommitObject2)

  }

  it should "create commit in .sgit/objects with the right content" in {
  }

  it should "create all trees of the commit tree in .sgit/objects" in {
  }

  it should "update the current branch with the commit" in {
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    Commit.commit(repoPath, "commit")
    val shaCommitObject = CommitUtil.getLastCommitObject(repoPath, BranchUtil.getCurrentBranchName(repoPath)).get

    Add.add(repoPath, Seq(".test" + File.separator + "test2"))

    Commit.commit(repoPath, "commit 2")
    val shaCommitObject2 = CommitUtil.getLastCommitObject(repoPath, BranchUtil.getCurrentBranchName(repoPath)).get

    assert(shaCommitObject != shaCommitObject2)
  }

}
