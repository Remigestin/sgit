package parser

object ErrorMessage {

  def repoNotFound(): Unit = {
    println("fatal: not a sgit repository (or any of the parent directories): .sgit")
  }

  def indexNotCreated(): Unit = {
    println("nothing to commit")
  }

  def noCommit(): Unit = {
    println("Failed to resolve 'Head' as a valid ref")
  }

}
