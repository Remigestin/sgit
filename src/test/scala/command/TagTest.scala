package command

import java.io.File

import org.scalatest.{BeforeAndAfterEach, FlatSpec}
import util.{CommitUtil, FileUtil}

import scala.reflect.io.Directory

class TagTest extends FlatSpec with BeforeAndAfterEach {

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

  "The tag command" should "create a tag file in .sgit/tags with the right content" in {

    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    val nameTag = "name"

    Commit.commit(repoPath, "commit")

    Tag.tag(repoPath, nameTag)
    val pathTag = repoPath + File.separator + ".sgit" + File.separator + "tags" + File.separator + nameTag

    assert(new File(pathTag).exists())

    val content = FileUtil.readFileToList(pathTag).head
    val shaCommit = CommitUtil.getLastCommitObject(repoPath)
    assert(shaCommit == content)
  }

  it should "not create a tag file if there is no commit" in {

    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    val nameTag = "name"
    Tag.tag(repoPath, nameTag)

    val pathTag = repoPath + File.separator + ".sgit" + File.separator + "tags" + File.separator + nameTag

    assert(!new File(pathTag).exists())
  }

  it should "not create a tag if it is already exist" in {
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir")).get
    val nameTag = "name"
    val pathTag = repoPath + File.separator + ".sgit" + File.separator + "tags" + File.separator + nameTag
    Commit.commit(repoPath, "commit")
    Tag.tag(repoPath, nameTag)
    val contentTag1 = FileUtil.readFileToList(pathTag).head

    Add.add(repoPath, Seq(".test" + File.separator + "test2"))
    Commit.commit(repoPath, "commit 2")
    Tag.tag(repoPath, nameTag)

    val contentTag2 = FileUtil.readFileToList(pathTag).head

    assert(contentTag1 == contentTag2)
  }
}
