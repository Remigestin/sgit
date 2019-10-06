package app

import java.io.File

import scopt.OParser

case class Config(
                   foo: Int = -1,
                   out: File = new File("."),
                   xyz: Boolean = false,
                   libName: String = "",
                   maxCount: Int = -1,
                   verbose: Boolean = false,
                   debug: Boolean = false,
                   mode: String = "",
                   files: Seq[File] = Seq(),
                   keepalive: Boolean = false,
                   jars: Seq[File] = Seq(),
                   kwargs: Map[String, String] = Map())

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
          arg[File]("<file>...")
            .required()
            .unbounded()
            .action((x, c) => c.copy(files = c.files :+ x))
            .text("file to add to index")
        ),
    )
  }

  // OParser.parse returns Option[app.Config]
  OParser.parse(parser1, args, Config()) match {
    case Some(config) =>
      config.mode match {
        case "init" =>
          println(Repo.init())
        case "add" =>
          add.add(config.files)
        case _ =>
        println("error")
      }
  }
}
