package io

// we define a simple type that represents real world interactions or 'Effects'
class IO[A](thunk: => A) {
  def run(): A = thunk

  def map[B](f: A => B): IO[B] = IO.pure(f(thunk))
  def flatMap[B](f: A => IO[B]): IO[B] = f(thunk)
}

object IO {
  def pure[A](a: A) = new IO(a)

  // in context of this type we define all required typeclasses we want to use in our programs
  implicit val ioMonad: Monad[IO] = new Monad[IO] {
    override def pure[A](a: A): IO[A] = IO.pure(a)
    override def map[A, B](fa: IO[A])(f: A => B): IO[B] = fa.map(f)
    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
  }

  // we can have multiple implementations of Console, those that actually print stuff
  // of those that just store things internally that can be examined later on for testing
  implicit def ioConsole: Console[IO] = new Console[IO] {
    override def putStrLn(line: String): IO[Unit] = IO.pure(println(line))
    override def readStrLn(): IO[String] = IO.pure(scala.io.StdIn.readLine())
  }
}

