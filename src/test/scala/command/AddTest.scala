package command

import java.io.File

import org.scalatest.{BeforeAndAfterEach, FlatSpec}
import util.{FileUtil, IndexUtil, RepoUtil}

import scala.reflect.io.Directory

class AddTest extends FlatSpec with BeforeAndAfterEach {

  //init an sgit repo and .test repo before each test
  override def beforeEach(): Unit = {
    Init.init(System.getProperty("user.dir"))
    new File(".test").mkdir()
    FileUtil.editFile(".test" + File.separator + "test", "Hello World", append = true)
    FileUtil.editFile(".test" + File.separator + "test2", "hello, world", append = true)
  }

  //delete all files created in the .sgit and .test directory after each test
  override def afterEach(): Unit = {
    val sgitPath = RepoUtil.getRepoPath(System.getProperty("user.dir")).get + File.separator + ".sgit"
    val sgitDir = new Directory(new File(sgitPath))
    sgitDir.deleteRecursively()

    new Directory(new File(".test")).deleteRecursively()
  }

  "the add command" should "create index file if it is not already created in .sgit" in {
    val repoPath = RepoUtil.getRepoPath(System.getProperty("user.dir")).get
    val testFilePath = repoPath + File.separator + ".test" + File.separator + "test"

    val indexPath = repoPath + File.separator + ".sgit" + File.separator + "index"
    assert(!new File(indexPath).exists())

    Add.add(repoPath, Seq(new File(testFilePath).getAbsolutePath))
    assert(new File(indexPath).exists())
  }

  it should "add the good line for the given file in .sgit/INDEX file" in {
    val repoPath = RepoUtil.getRepoPath(System.getProperty("user.dir")).get
    val testFilePath = repoPath + File.separator + ".test" + File.separator + "test"

    Add.add(repoPath, Seq(testFilePath))

    val pathRel = testFilePath.replace(repoPath + File.separator, "")

    val content = FileUtil.readFileToList(testFilePath) mkString "\n"
    val sha = FileUtil.sha1Hash(content)

    assert(IndexUtil.readIndexToList(repoPath).head == sha + " " + pathRel)
  }

  it should "add multiple lines in the index file if there is several files added at the same time" in {
    val repoPath = RepoUtil.getRepoPath(System.getProperty("user.dir")).get
    val testFilePath = repoPath + File.separator + ".test" + File.separator + "test"

    val testFilePath2 = repoPath + File.separator + ".test" + File.separator + "test2"

    Add.add(repoPath, Seq(testFilePath, testFilePath2))

    val pathRel = testFilePath.replace(repoPath + File.separator, "")
    val pathRel2 = testFilePath2.replace(repoPath + File.separator, "")

    val content = FileUtil.readFileToList(testFilePath) mkString "\n"
    val sha = FileUtil.sha1Hash(content)

    val content2 = FileUtil.readFileToList(testFilePath2) mkString "\n"
    val sha2 = FileUtil.sha1Hash(content2)

    assert((IndexUtil.readIndexToList(repoPath) mkString "\n") == sha + " " + pathRel +"\n" + sha2 + " " + pathRel2)

  }

  it should "create the blob file which correspond to the content of the file added" in {
    val repoPath = RepoUtil.getRepoPath(System.getProperty("user.dir")).get
    val testFilePath = repoPath + File.separator + ".test" + File.separator + "test"
    val testFile = new File(testFilePath)

    Add.add(repoPath, Seq(testFile.getAbsolutePath))

    val contentFile = FileUtil.readFileToList(testFilePath) mkString "\n"
    val sha = FileUtil.sha1Hash(contentFile)

    val blobPath = repoPath + File.separator + ".sgit" + File.separator + "objects" + File.separator + sha
    val contentBlob = FileUtil.readFileToList(blobPath) mkString "\n"

    assert(contentBlob == contentFile)
  }

  it should "updates index file with the last version of each files" in {
    val repoPath = RepoUtil.getRepoPath(System.getProperty("user.dir")).get
    val testFilePath = repoPath + File.separator + ".test" + File.separator + "test"

    Add.add(repoPath, Seq(testFilePath))

    val shaPreviousUpdate = IndexUtil.readIndexToList(repoPath).head.split(" ")(0)

    FileUtil.editFile(testFilePath, "it is an update", append = true)

    Add.add(repoPath, Seq(testFilePath))

    val shaAfterUpdate = IndexUtil.readIndexToList(repoPath).head.split(" ")(0)

    assert(shaPreviousUpdate != shaAfterUpdate)
  }

  it should "not add a line in INDEX if arguments do not match any files" in {
    val currentDir = System.getProperty("user.dir")
    val repoPath = RepoUtil.getRepoPath(System.getProperty("user.dir")).get
    Add.add(repoPath, Seq("anything"))
    val indexLines = IndexUtil.readIndexToList(repoPath)
    assert(indexLines.isEmpty)
  }

  it should "remove from the index a deleted file" in {
    val repoPath = RepoUtil.getRepoPath(System.getProperty("user.dir")).get
    val testFilePath = repoPath + File.separator + ".test" + File.separator + "test"

    Add.add(repoPath, Seq(testFilePath))
    val indexLinesOld = IndexUtil.readIndexToList(repoPath)

    assert(indexLinesOld.length == 1)

    new File(testFilePath).delete()
    Add.add(repoPath, Seq(testFilePath))
    val indexLines = IndexUtil.readIndexToList(repoPath)
    assert(indexLines.isEmpty)
  }

  it should "remove from the index a deleted file (with some other file not deleted)" in {
    val repoPath = RepoUtil.getRepoPath(System.getProperty("user.dir")).get
    val testFilePath = repoPath + File.separator + ".test" + File.separator + "test"
    val testFilePath2 = repoPath + File.separator + ".test" + File.separator + "test2"

    Add.add(repoPath, Seq(testFilePath, testFilePath2))

    new File(testFilePath).delete()
    Add.add(repoPath, Seq(testFilePath))
    val indexLines = IndexUtil.readIndexToList(repoPath)
    assert(indexLines.length == 1)
    assert(indexLines.head.split(" ")(1) != testFilePath)
  }

}
