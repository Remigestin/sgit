package parser

import java.io.File

case class Config(
                   foo: Int = -1,
                   out: File = new File("."),
                   xyz: Boolean = false,
                   libName: String = "",
                   commitMessage: String = "",
                   maxCount: Int = -1,
                   verbose: Boolean = false,
                   debug: Boolean = false,
                   mode: String = "",
                   files: Seq[String] = Seq(),
                   keepalive: Boolean = false,
                   patch: Boolean = false,
                   jars: Seq[File] = Seq(),
                   option: String = "",
                   av: Boolean = false,
                   kwargs: Map[String, String] = Map())

