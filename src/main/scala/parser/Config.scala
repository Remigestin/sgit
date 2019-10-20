package parser

import java.io.File

case class Config(
                   mode: String = "",
                   libName: String = "",
                   commitMessage: String = "",
                   files: Seq[String] = Seq(),
                   option: String = "",
                   av: Boolean = false)

