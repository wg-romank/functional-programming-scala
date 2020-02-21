package io

object Main {
  import Monad.MonadSyntax

  // we can describe our programs in terms of what typeclasses they use
  // e.g. what kind of 'abilities' they invoke
  def helloWorldProgram[F[_] : Monad : Console](): F[Unit] = {
    Console().putStrLn("Hello")
  }

  // important note here is invoking 'readAndPrintBackProgram'
  // does not actually execute any code
  // it just creates program description that can be
  // passed around and executed later or combined with other programs as shown below
  def readAndPrintBackProgram[F[_] : Monad : Console](): F[Unit] = {
    for {
      _ <- Console().putStrLn("Input some value followed by return")
      input <- Console().readStrLn()
      _ <- Console().putStrLn(s"You have wrote $input")
    } yield ()
  }

  def main(args: Array[String]): Unit = {
    // here we import our typeclass implementations
    // the code that actualy does things for us
    import IO._

    // here we just create a program
    val hello = helloWorldProgram[IO]()

    // to execute we would actually need to explicitly run them
    hello.run()

    // then you can combine programs
    val combined = for {
      _ <- helloWorldProgram()
      _ <- readAndPrintBackProgram()
    } yield ()

    // and pass them around or just execute
    combined.run()
  }

}
