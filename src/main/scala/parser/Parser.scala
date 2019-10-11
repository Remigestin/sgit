package parser

import java.io.File

import command.{Add, Commit, Repo}
import scopt.OParser
import util.IndexUtil


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
        )
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
            ErrorMessage.repoNotFound()
        case "commit" =>
          if (Repo.isInASgitRepo(System.getProperty("user.dir")))
            if (IndexUtil.isIndexCreated(Repo.getRepoPath(System.getProperty("user.dir")).get))
              println(Commit.commit(Repo.getRepoPath(System.getProperty("user.dir")).get, config.commitMessage))
            else
              ErrorMessage.indexNotCreated()
          else
            ErrorMessage.repoNotFound()
        case _ =>
          println("error")
      }
    case _ =>
    //arguments are bad
  }


}
