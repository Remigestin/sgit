# Sgit

Sgit is a git-like cli developed in Scala

Here is a list of the commands implemented:
* sgit init
* sgit add \<file>...
* sgit commit -m \<message>
* sgit status
* sgit branch
    * -av
    * \<branch name>
* sgit tag
    * \<name tag>
* sgit diff
* sgit log
    * -p
    * --stat

## Installation of Sgit
```bash
sbt assembly
```
then add sgit/target/scala-2.13 to your path