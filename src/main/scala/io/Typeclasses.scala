package io

// typeclasses are like 'abilities' that our programs can invoke
// they defined on certain effect type 'F'
// 'F' can be is Future, zio.Task, monix.Task or any other IO library primitive for execution
// we define our typeclasses over abstract 'F' so that it will be easy for us to switch between
// different execution environments

trait Monad[F[_]] {
  def pure[A](a: A): F[A]
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

object Monad {
  implicit class MonadSyntax[F[_], A](fa: F[A])(implicit e: Monad[F]) {
    def pure(a: A): F[A] = e.pure(a)
    def map[B](f: A => B): F[B] = e.map(fa)(f)
    def flatMap[B](f: A => F[B]): F[B] = e.flatMap(fa)(f)
  }
}

trait Console[F[_]] {
  // each typeclass defines set of methods that enable interaction with this 'ability'
  // like priting / reading from console etc
  def putStrLn(line: String): F[Unit]
  def readStrLn(): F[String]
}

object Console {
  def apply[F[_] : Console](): Console[F] = implicitly[Console[F]]
}
