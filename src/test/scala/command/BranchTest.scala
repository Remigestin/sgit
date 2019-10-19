package command

import java.io.File

import org.scalatest.{BeforeAndAfterEach, FlatSpec}
import util.{BranchUtil, CommitUtil, FileUtil}

import scala.reflect.io.Directory

class BranchTest extends FlatSpec with BeforeAndAfterEach {

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

  "The branch command" should "create a branch file in .sgit/branches with the right content" in {

    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    val nameBranch = "name"

    Commit.commit(repoPath, "commit")

    Branch.branch(repoPath, nameBranch)
    val pathBranch = repoPath + File.separator + ".sgit" + File.separator + "branches" + File.separator + nameBranch

    assert(new File(pathBranch).exists())

    val content = FileUtil.readFileToList(pathBranch).head
    val shaCommit = CommitUtil.getLastCommitObject(repoPath, BranchUtil.getCurrentBranchName(repoPath)).get
    assert(shaCommit == content)
  }

  it should "not create a branch if there is no commit" in {

    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    val nameBranch = "name"
    Branch.branch(repoPath, nameBranch)

    val pathBranch = repoPath + File.separator + ".sgit" + File.separator + "branches" + File.separator + nameBranch

    assert(!new File(pathBranch).exists())
  }

  it should "not create a branch if it is already exist with the same name" in {
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    val nameBranch = "name"
    val pathBranch = repoPath + File.separator + ".sgit" + File.separator + "branches" + File.separator + nameBranch
    Commit.commit(repoPath, "commit")
    Branch.branch(repoPath, nameBranch)
    val contentBranch1 = FileUtil.readFileToList(pathBranch).head

    Add.add(repoPath, Seq(".test" + File.separator + "test2"))
    Commit.commit(repoPath, "commit 2")
    Branch.branch(repoPath, nameBranch)

    val contentBranch2 = FileUtil.readFileToList(pathBranch).head

    assert(contentBranch1 == contentBranch2)
  }

  "The commande branch -av" should "print all the branches and tags" in {

    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    val nameBranch = "name"

    Commit.commit(repoPath, "commit")
    Branch.branch(repoPath, nameBranch)

    Add.add(repoPath, Seq(".test" + File.separator + "test2"))

    Commit.commit(repoPath, "deuxieme commit")
    Tag.tag(repoPath, "aalia")


    println(Branch.branchAV(repoPath))


  }

}


