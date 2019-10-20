package command

import java.io.File
import java.nio.file.Paths

import org.scalatest.{BeforeAndAfterEach, FlatSpec}
import util.{BranchUtil, CommitUtil, FileUtil, IndexUtil}

import scala.reflect.io.Directory

class StatusTest extends FlatSpec with BeforeAndAfterEach {

  //init an sgit repo and .test repo before each test
  override def beforeEach(): Unit = {

    new File(".test").mkdir()
    new File(".test" + File.separator + "test").mkdir()
    Repo.init(System.getProperty("user.dir") + File.separator + ".test")
    FileUtil.editFile(".test" + File.separator +  "test" + File.separator + "test", "Hello World", append = true)
    FileUtil.editFile(".test" + File.separator + "test" + File.separator + "test2", "hello, world", append = true)
  }

  //delete all files created in the .sgit and .test directory after each test
  override def afterEach(): Unit = {
    val sgitPath = Repo.getRepoPath(System.getProperty("user.dir") + File.separator + ".test").get + File.separator + ".sgit"
    val sgitDir = new Directory(new File(sgitPath))
    sgitDir.deleteRecursively()

    new Directory(new File(".test")).deleteRecursively()
  }

  "The status command" should "recover all the files untracked" in {
    val curdir =  System.getProperty("user.dir") + File.separator + ".test"
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir") + File.separator + ".test").get
    val mapIndex = IndexUtil.readIndexToMap(repoPath)

    //recover the path of all the files in the repoPath
    val allFilesRepoList = FileUtil
      .recursiveListFiles(new File(repoPath))
      .filter(_.isFile)
      .filter(!_.getAbsolutePath.contains(".sgit"))
      .map(_.getAbsolutePath)
      .map(_.replace(repoPath + File.separator, "")).toList

    val contentFilesRepoList = allFilesRepoList.map(p => FileUtil.readFileToList(repoPath + File.separator + p))

    val allFilesRepoMap = (allFilesRepoList zip contentFilesRepoList).toMap

    val testFilePath =  repoPath + File.separator + "test" + File.separator + "test"
    val testFilePath2 =  repoPath + File.separator + "test" + File.separator + "test2"
    val res = Status.getAllPathsUntracked(mapIndex,allFilesRepoMap)

    val pathRel = Paths.get(curdir).relativize(Paths.get(testFilePath)).toString
    val pathRel2 = Paths.get(curdir).relativize(Paths.get(testFilePath2)).toString

    assert(res.contains(pathRel))
    assert(res.contains(pathRel2))
  }

  it should "recover all indexed files edited but not added" in {
    val curdir =  System.getProperty("user.dir") + File.separator + ".test"
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir") + File.separator + ".test").get
    val testFilePath =  repoPath + File.separator + "test" + File.separator + "test"



    Add.add(repoPath,Seq(testFilePath))



    FileUtil.editFile(testFilePath,"this is an edit", append = true)

    val mapIndex = IndexUtil.readIndexToMap(repoPath)


    //recover the path of all the files in the repoPath
    val allFilesRepoList = FileUtil
      .recursiveListFiles(new File(repoPath))
      .filter(_.isFile)
      .filter(!_.getAbsolutePath.contains(".sgit"))
      .map(_.getAbsolutePath)
      .map(_.replace(repoPath + File.separator, "")).toList

    val contentFilesRepoList = allFilesRepoList.map(p => FileUtil.readFileToList(repoPath + File.separator + p))

    val allFilesRepoMap = (allFilesRepoList zip contentFilesRepoList).toMap





    val res = Status.getAllPathsTrackedModifiedNotAdd(mapIndex,allFilesRepoMap)
    val pathRel = Paths.get(curdir).relativize(Paths.get(testFilePath)).toString

    assert(res.contains(pathRel))

  }


  it should "recover all files added but never committed (if there was commits before)" in {
    val curdir =  System.getProperty("user.dir") + File.separator + ".test"
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir") + File.separator + ".test").get
    val testFilePath =  repoPath + File.separator + "test" + File.separator + "test"
    val testFilePath2 =  repoPath + File.separator + "test" + File.separator + "test2"
    Add.add(repoPath,Seq(testFilePath))
    Commit.commit(repoPath, "commit 1")

    Add.add(repoPath, Seq(testFilePath2))

    val mapIndex = IndexUtil.readIndexToMap(repoPath)

    val branchName = BranchUtil.getCurrentBranchName(repoPath)
    val shaCommit = CommitUtil.getLastCommitObject(repoPath, branchName)
    val mapCommit = CommitUtil.getCommitMap(repoPath, shaCommit)
    val res = Status.getAllPathTrackedNotCommitted(mapIndex,mapCommit)
    val pathRel2 = Paths.get(curdir).relativize(Paths.get(testFilePath2)).toString

    assert(res.contains(pathRel2))

  }

  it should "recover all files added, but different from the last commit (if there was commits before)" in {
    val curdir =  System.getProperty("user.dir") + File.separator + ".test"
    val repoPath = Repo.getRepoPath(System.getProperty("user.dir") + File.separator + ".test").get
    val testFilePath =  repoPath + File.separator + "test" + File.separator + "test"
    Add.add(repoPath,Seq(testFilePath))
    Commit.commit(repoPath, "commit 1")
    FileUtil.editFile(testFilePath, "ceci est une modif", append = true)
    Add.add(repoPath,Seq(testFilePath))

    val mapIndex = IndexUtil.readIndexToMap(repoPath)

    val branchName = BranchUtil.getCurrentBranchName(repoPath)
    val shaCommit = CommitUtil.getLastCommitObject(repoPath, branchName)
    val mapCommit = CommitUtil.getCommitMap(repoPath, shaCommit)


    val res = Status.getAllPathTrackedAndCommittedModified(mapIndex,mapCommit)
    val pathRel = Paths.get(curdir).relativize(Paths.get(testFilePath)).toString

    assert(res.contains(pathRel))
  }

  it should "recover all files in the index but which not exists anymore (with one file)" in {

    val repoPath = Repo.getRepoPath(System.getProperty("user.dir") + File.separator + ".test").get
    val testFilePath =  repoPath + File.separator + "test" + File.separator + "test"
    Add.add(repoPath,Seq(testFilePath))

    new File(testFilePath).delete()

    val mapIndex = IndexUtil.readIndexToMap(repoPath)


    //recover the path of all the files in the repoPath
    val allFilesRepoList = FileUtil
      .recursiveListFiles(new File(repoPath))
      .filter(_.isFile)
      .filter(!_.getAbsolutePath.contains(".sgit"))
      .map(_.getAbsolutePath)
      .map(_.replace(repoPath + File.separator, "")).toList

    val contentFilesRepoList = allFilesRepoList.map(p => FileUtil.readFileToList(repoPath + File.separator + p))

    val allFilesRepoMap = (allFilesRepoList zip contentFilesRepoList).toMap



    val res = Status.getAllDeletionsNotStaged(mapIndex,allFilesRepoMap)

    assert(res.length == 1)
    assert(res.head == "test" + File.separator + "test")
  }

  it should "recover all files in the index but which not exists anymore (with multi files)" in {

    val repoPath = Repo.getRepoPath(System.getProperty("user.dir") + File.separator + ".test").get
    val testFilePath =  repoPath + File.separator + "test" + File.separator + "test"
    val testFilePath2 =  repoPath + File.separator + "test" + File.separator + "test2"
    Add.add(repoPath,Seq(testFilePath, testFilePath2))

    new File(testFilePath).delete()

    val mapIndex = IndexUtil.readIndexToMap(repoPath)


    //recover the path of all the files in the repoPath
    val allFilesRepoList = FileUtil
      .recursiveListFiles(new File(repoPath))
      .filter(_.isFile)
      .filter(!_.getAbsolutePath.contains(".sgit"))
      .map(_.getAbsolutePath)
      .map(_.replace(repoPath + File.separator, "")).toList

    val contentFilesRepoList = allFilesRepoList.map(p => FileUtil.readFileToList(repoPath + File.separator + p))

    val allFilesRepoMap = (allFilesRepoList zip contentFilesRepoList).toMap

    val res = Status.getAllDeletionsNotStaged(mapIndex,allFilesRepoMap)

    assert(res.length == 1)
    assert(res.head == "test" + File.separator + "test")
  }

  it should "recover all files not in the index but in the last commit" in {

    val repoPath = Repo.getRepoPath(System.getProperty("user.dir") + File.separator + ".test").get
    val testFilePath =  repoPath + File.separator + "test" + File.separator + "test"
    val testFilePath2 =  repoPath + File.separator + "test" + File.separator + "test2"
    Add.add(repoPath,Seq(testFilePath, testFilePath2))

    Commit.commit(repoPath, "commit")

    new File(testFilePath).delete()
    Add.add(repoPath, Seq(testFilePath))

    val mapIndex = IndexUtil.readIndexToMap(repoPath)

    val branchName = BranchUtil.getCurrentBranchName(repoPath)
    val shaCommit = CommitUtil.getLastCommitObject(repoPath, branchName)
    val mapCommit = CommitUtil.getCommitMap(repoPath, shaCommit)

    val res = Status.getAllDeletionsNotCommitted(mapIndex,mapCommit)

    assert(res.length == 1)
    assert(res.head == "test" + File.separator + "test")

  }



}
