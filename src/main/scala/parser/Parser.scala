package parser

import java.io.File

import command.{Add, Branch, Commit, Diff, Repo, Status, Tag}
import scopt.OParser
import util.{CommitUtil, IndexUtil}
import parser.ErrorMessage._


object Parser extends App {

  val builder = OParser.builder[Config]
  val parser1 = {
    import builder._
    OParser.sequence(
      programName("sgit"),
      head("sgit", "0.1"),
      help("help")
        .text("this is sgit"),
      cmd("init")
        .action((_, c) => c.copy(mode = "init"))
        .text("create a sgit repository"),
      cmd("add")
        .action((_, c) => c.copy(mode = "add"))
        .text("adds the files to the index")
        .children(
          arg[String]("<file>...")
            .required()
            .unbounded()
            .action((x, c) => c.copy(files = c.files :+ x))
            .text("file to add to index")
        ),
      cmd("commit")
        .action((_, c) => c.copy(mode = "commit"))
        .text("Create a new commit containing the current contents of the index and the given log message describing the changes.")
        .children(
          opt[String]('m', name = "message")
            .required()
            .maxOccurs(1)
            .action((x, c) => c.copy(commitMessage = x))
            .text("commit message")
        ),
      cmd("status")
        .action((_, c) => c.copy(mode = "status"))
        .text("list the status of the repo"),
      cmd("tag")
        .action((_, c) => c.copy(mode = "tag"))
        .text("create tag for the current commit")
        .children(
          arg[String]("name")
            .required()
            .maxOccurs(1)
            .action((x, c) => c.copy(libName = x))
            .text("name of the tag")
        ),
      cmd("branch")
        .action((_, c) => c.copy(mode = "branch"))
        .text("create a branch")
        .children(
          arg[String]("branchName")
            .required()
            .maxOccurs(1)
            .action((x, c) => c.copy(libName = x))
            .text("name of the branch")
        ),
      cmd("diff")
        .action((_, c) => c.copy(mode = "diff"))
        .text("show the diff between the index and the working tree")
    )
  }


  // OParser.parse returns Option[app.parser.Config]
  OParser.parse(parser1, args, Config()) match {
    case Some(config) =>
      config.mode match {
        case "init" =>
          println(Repo.init(System.getProperty("user.dir")))

        case "add" =>
          if (Repo.isInASgitRepo(System.getProperty("user.dir")))
            Add.add(Repo.getRepoPath(System.getProperty("user.dir")).get, config.files)
          else
            repoNotFound()

        case "commit" =>
          if (Repo.isInASgitRepo(System.getProperty("user.dir")))
            if (IndexUtil.isIndexCreated(Repo.getRepoPath(System.getProperty("user.dir")).get))
              println(Commit.commit(Repo.getRepoPath(System.getProperty("user.dir")).get, config.commitMessage))
            else
              indexNotCreated()
          else
            repoNotFound()

        case "status" =>
          if (Repo.isInASgitRepo(System.getProperty("user.dir")))
            println(Status.status(System.getProperty("user.dir")))
          else
            repoNotFound()

        case "tag" =>
          if (Repo.isInASgitRepo(System.getProperty("user.dir")))
            if (CommitUtil.isThereACommit(Repo.getRepoPath(System.getProperty("user.dir")).get))
              println(Tag.tag(Repo.getRepoPath(System.getProperty("user.dir")).get, config.libName))
            else
              noCommit()
          else
            repoNotFound()

        case "branch" =>
          if (Repo.isInASgitRepo(System.getProperty("user.dir")))
            if (CommitUtil.isThereACommit(Repo.getRepoPath(System.getProperty("user.dir")).get))
              Branch.branch(Repo.getRepoPath(System.getProperty("user.dir")).get, config.libName)
            else
              noCommit()
          else
            repoNotFound()

        case "diff" =>
          if (Repo.isInASgitRepo(System.getProperty("user.dir")))
            Diff.diff(Repo.getRepoPath(System.getProperty("user.dir")).get)
          else
            repoNotFound()

        case _ =>
          println("error")


      }
    case _ =>
    //arguments are bad
  }


}
