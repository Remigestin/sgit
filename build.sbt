import sbtassembly.AssemblyPlugin.defaultUniversalScript


name := "sgit"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.0-RC2"

assemblyOption in assembly := (assemblyOption in assembly).value.copy(prependShellScript = Some(defaultUniversalScript(shebang = false)))

assemblyJarName in assembly := s"sgit"