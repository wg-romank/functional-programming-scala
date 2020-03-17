# Tagless Final

## Motivation

When talking about programming in general (especially execution flow / debugging) people often talk about contexts that in turn are linked with state.

<img src="https://i.redd.it/o1gnotfxi4k01.png" width="256"/>

When doing functional programming we tend to care more about *what* than the underlying *how*. This is possible due to one very powerful property that is called *referential transparency*.

In this article I attempt to share some of my thoughts on why using functional concepts might be a good idea as well as covering some of the known patterns and why I would use those over their OO counterparts. As we all are coming from different backgrounds your mileage may vary.

I came across functional programming as a thing while getting to know Scala and taking Martin Odersky's course [[1]]. The whole idea of representing computation as a value was really new to me so I had to spend some time understanding state management through recursion, but once past that stage things start to get really fun.

It has been quite a few conference talks and articles on a subject since my first exposure to the original course to this moment. Here I attempt to make a shortcut to some fundamental concepts that I realized through my journey as well as to improve my understanding by trying to explain them.

## Total functions

Here we go by example. One of simpler examples interacting with the outside world is parsing some kind of payload. Since Web is the world and Web servers are its backbone - this task is well known (from working with REST APIs for instance).

```scala
def parseOrder(payload: String): Order = {
  // parse a string and construct an Order object
}
```

Payload could be json or protobuf or any other serialization format of your liking along with a library that supports it. Either way we need to be able to handle cases when payload was parsed successfully and when it was not.

A well-known (for software engineers) way to represent something going wrong is either throwing an exception or returning a special kind of value (like null).

It can be very easy to forget to do a null check on your inputs before computing something and compilers won't tell you a thing about that mistake. In that case what you end up with is `NullPointerException` which is always fun to debug. (though it seem there has been some improvements on that side [[99]])

Throwing exceptions also has certain disadvantages like being implicit to the caller since information on how to handle failures is not encoded in function signature. Thus it is hard for the caller to reason about what the function is doing just by looking at the function call. More specifically can it fail and should it have special treatment with `try-catch` or not.

Using one of those approaches is an established procedure in many languages already that engineers understand so why change that, you might reasonably ask? Let's look at alternatives of what we have so far.

One way to represent failures as part of function signature exists in Java as *checked exceptions*. It in turn has controversial opinions due to being restrictive - caller has to handle exceptions immediately after performing function call. [[100]].

Only a fraction of exception types are checked so this approach also requires complementary measures. Extensive documentation can communicate what kind of possible behaviour caller needs to deal with. In the end that's what good libraries do. But documenting every package-private function in library internals might very soon become a maintenance hassle. After all docs do not compile along with your source code so it is easy for them to get outdated so there are no guarantees (unless you introduce custom tooling). Compilers would happily compile code that does not handle new exception types that were introduced while say migration to a newer library version.

What kind of alternative approach we could use to make things clear and concise but without introducing too much boilerplate and being too restrictive? 

We need to describe certain behavior for `parseOrder` which consist of two main scenarios: this function can either have or not have resulting value. And this information can be encoded in the return type itself (assuming a particular language type system allows).

Scala has at least two ways of describing this behavior already in standard libraries: those are `Either` (if we care for failure type) `Option` (if we don't). Monads, right.

From the mathematical standpoint `parseOrder` is not defined on certain values of `payload` argument (empty strings, malformed data, etc.) and by rewriting return type as `Option[String]` instead of plain `String` we can extend our `parseOrder` to be defined for all values of its arguments thus can be called *total*.

```scala
def parseOrderTotal(payload: String): Option[Order]
```

Here instead of using special values of the same type (null for instance) or throwing exceptions to represent failure we utilize builtin language primitives to make such behavior more explicit. 

We also don't have to handle results of function calls immediately (like in case of checked exceptions) as we can pass around `Option`. But whenever we want to get actual values we have to handle both cases and this is enforced at compilation time.

## Pure functions & referential transparency

Pure functions are those functions that evaluate to the same results each time they are called with particular arguments without any dependency on the outside world.

If our code consists only of *pure* and *total* functions then it satisfies a very powerful property called *referential transparency*. This property enables us to substitute any function call with its value and the way around any value can be factored out as a function and reused across databases with no need to worry about context and state management. Sounds like a place to be to do refactoring, right?

One slight problem here: there is a class of important functions that are not *pure* by definition. Those functions that communicate with the outside world.

As you might have guessed `System.currentTimeMillis()` is not a pure function since it evaluates to a different value each time it has been called. Similar issue we have when we talk about doing any other kind of IO which may fail or depends on the database state. Basically any function that does useful work for us is not *pure* by definition.

Later on we will cover how can we represent interaction with outside world as  a *pure* function call but let's first discuss why *referential transparency* is important.

## Things may fail

Let's get back to our payload example: assume that's once parsing is successfully completed we want to write our order to the database.

We might have defined in our code a trait that represents contact of communication with the database.

```scala
trait DAO[T] {
  def create(t: T): Unit
}
```

This contract is parametric on entity type `T`. Function `create` is returning us `Unit` which clearly indicates that purpose of this function is to communicate with outside world (meaning establish connection with database and write a new entity) which is also called a (*side-effect*)[[11]].

Because this function is not *pure* (as we discussed earlier) and since persisting data is usually an important operation for us any caller would naturally surround those calls within the `try-catch` block. This is another well established OO pattern.

```scala
def storeOrder(ordersDao: DAO[Order], payload: String): Unit =
  try {
    val order = parseOrder(payload)
    ordersDao.create(order)
  } catch {
    case e: RecoverableError =>
      // do recover logic
      // maybe log/send monitoring stats
    case e: FatalError =>
      // log/send monitoring
      // propagate failure
  }
```

This works ok for this small example even though we need to surround any call to database with *try-catch* block (here comes context). 

Now say we create an order then we need to send the user who created this order our acknowledgement through email. In order to do that we add another method to our contract for fetching objects (to get our user) as well as introducing a new contract for email communication.

```scala
trait DAO[T] {
  def create(t: T): Unit
  def get(id: UUID): T
}

trait Email {
  def send(address: EmailAddress, body: Html): Unit
}
```

Since sending acknowledgement might be a common action that can occur in several scenarios across our database we would naturally create another method.

```scala
def sendEmailToUser(usersDao: DAO[User], email: Email,
                    userId: UUID, body: Html): Unit =
  try {
    val user = usersDao.get(userId)
    email.send(user.email, body)
  } catch {
    case e: RecoverableError =>
      // do recover logic
      // maybe log/send monitoring stats
    case e: FatalError =>
      // log/send monitoring
      // propagate failure
  }
```

So in order to execute a full scenario we would need to compose those two operations, meaning we first need to execute validation/creation part and then send notification.

One of the issues we might encounter along the way is: where to put error handling? In example, we put error handling along with actual implementation, which might be handy in case we know what kind of exceptions we want to catch and how to recover from them. But in order to compose our two functions and ensure proper execution order we would need to put error handling to outer scope (caller scope), since we do not want the second function to be executed in case the first one fails.

And what about threading? Let's say we have been just creating our POC application and when it has been proven working we would like to scale existing code to production with minimal effort. Adding threading with the current approach will also need to be context aware.

Those issues we encounter start to sound like we are actually missing an abstraction in there. A *runtime* abstraction. In next paragraph we try to solve  this problems using existing IO primitives that would encapsulate *runtime* complexity for us.

## Composable IO

As we mentioned earlier we would very much like to use *pure* and *total* functions in our programs so it is easy for us to reason about our functions change them by refactoring or implementing new features leveraging property of *referential transparency*.

But we discovered already that *pure* functions can't do much when it comes to interacting with the outside world. What we could do instead is to use a value to represent our interaction. This value would be of certain type that represents computation that interacts with outside world (or performs *side-effect*).

We will use `Future` as an example of this type since it is already in the standard library even though it has certain shortcomings (like being an eager executor). But in reality we could pick any IO primitive to represent *side-effects*.

```scala
trait DAO[T] {
  def create(t: T): Future[Unit]
  def get(id: UUID): Future[T]
}

trait Email {
  def send(address: EmailAddress, body: Html): Future[Unit]
}
```

Using this kind of interface for interacting with database / email service we can describe the same logic as following:

```scala
def processOrder(ordersDao: DAO[Order], usersDao: DAO[User],
                 email: Email, order: Order, body: Html): Future[Unit] = for {
  _ <- ordersDao.create(order)
  user <- usersDao.get(order.userId)
  _ email.send(user.email, body)
} yield ()
```

This is possible due to Scala's syntactic sugar for working with `map` and `flatMap` operations that `Future` (implements) [[10]].

Assuming user notification is a common operation we might want to do in different scenarios we could extract it to a separate method. Using this compositional approach with `Future` allows us to do this easily without extra attention to context. Since all the typical error handling will be taken care of on the `Future` level we do not have to mix our business logic with implementation details.

```scala
def sendEmailNotifiation(usersDao: DAO[User], email: Email,
                         userId: UUID, body: Html): Future[Unit] =
  for {
    user <- usersDao.get(order.userId)
    _ <- email.send(user.email, body)
  } yield ()
  
def processOrder(usersDao: DAO[User], email: Email, order: Order, body: Html): Future[Unit] = for {
  _ <- ordersDao.create(order)
  _ <- sendEmailNotification(usersDao, email, order.userId, body)
} yield ()
```

This already may seem like a very powerful technique but it has certain shortcomings. The most prominent one as we mentioned already: `Future` is eager primitive and we do not necessarily want our order processing code to fire immediately. Also Scala's `Future` does not support such important functionality like timeout which can be crucial for processing REST requests or working with databases.

Fortunately there are a lot of great IO libraries already in Scala's ecosystem such as Monix, ZIO, Cats-Effect and others.

But as our initial intention was to decouple business logic from implementation as much as possible to achieve more flexibility we could completely abstract away our performance primitive. We do so using notion of *typeclasses* covered in the next paragraph.

## Typeclasses

Typeclasses are the way to define and implement functionality on some type without directly extending it and thus introduces a weaker coupling between original type and new functionality added.

There is no dedicated typeclass syntax in Scala, those are defined using traits. Typeclasses will be a handy way to extend our existing contracts, in particular, to abstract them from concrete type `Future` that we used to contain error handling and compose IO operations.

Let's look at Email contract definition that is IO-primitive agnostic:

```scala
trait Email[F[_]] {
  def send(address: EmailAddress, body: Html): F[Unit]
}

object Email {
  def apply[F[_]]()(implicit e: Email[F]): Email[F] = e
}
```

Now `Email` is not just a contract of our email client implementation but is a *typeclass* on type `F`. Here `F[_]` represents our IO-primitive type (or effect type) that can be either `Future` from standard library or `ZIO.Task` or any other type that will abstract *runtime* implementation.

We have to define our type parameter `F` with what is called *existential type syntax* because our IO primitive takes its own type parameter. In case of `send` method `Unit` is the underlying type in `F` since sending emails we do not expect any results when method completes but for cases when our computation results in some value we will use type of this value respectively (like when fetching entities from database for instance).

We also wrote down our *summoner* method `apply` that would allow us to write our programs using this interface without requiring any implementation to be declared in scope. The only thing we would need, to convince the compiler that there exists some kind of implementation, is to add an implicit parameter of type `Email[F]` to our business logic that wants to send emails.

Let's declare a method to send a few emails just to make things clear.

```scala
def sendFewEmails[F : Email](alice: User, bob: User, manInTheMiddle: User, body: Html): F[Unit] =
  for {
    _ <- Email().send(alice.email, body)
    _ <- Email().send(manInTheMiddle.email, body)
    _ <- Email().send(bob.email, body)
  } yield ()
```

Another syntactic sugar we use for implicit parameters: having `F : Email` means the same as adding `(implicit e: Email[F])` to our implicit parameter list.

Example is rather simple but this way we can describe our user logic without any dependency on `Email` implementation (similar to inversion of control/DI) as well as *runtime* (meaning threading, async execution, etc. = more powerful than DI).

In real world example our methods would probably have more interfaces they want to access (sending emails is always fun but we also need to fetch those email addresses from somewhere), so you would naturally add those *typeclasses* to function signature (e.g. `F : Email : Users : ... etc.)

Using specific type for type parameter `F` we can define several implementations that in order can use different IO libraries without any changes to our existing business logic. Let's define one for `Future`.

```scala
implicit val futureEmail = new Email[Future] {
  def send(address: EmailAddress, body: Html): Future[Unit] = Future {
    // send email logic here 
  }
}
```

Then when calling `sendFewEmail` method we just need to make sure our desired `Email` *typeclass* implementation is in scope.

The only remaining problem to bright future of using *typeclasses* is that current implementation will not compile. Here we again rely on Scala's syntactic sugar for using *map* and *flatMap* methods. So in order for it to compile type parameter `F` must have those methods declared, meaning it needs to be a monad.

## Monad to bind them all

As we mentioned earlier all we wanted from `Future` initially were `map` and `flatMap` methods so we could leverage Scala's own syntactic sugar to make our programs more expressive when defining business logic as well as encapsulating *runtime* complexity.

There are myriad posts on what Monad is so here we just want to focus on its usage purposely not including definition in here. [12]

In most of Scala libraries that have monad implementation we would see something along those lines below

```scala
trait Monad[F[_], A] {
  // ...
  def map[B](fa: F[A])(f: A => B): F[B]
  def flatMap[B](fa: F[A])(f: A => F[B]): F[B]
  // ...
}
```

Those are the same operations that make our IO primitive composeable. They both take wrapped-in-IO-action value `A` (`F[A]`) and transform it into some other wrapped-in-IO-action value `F[B]` given transformation function `f`.

Let's get back to our order acknowledgement notification using `Future` and break it down.

```scala
def sendEmailNotifiation(userId: UUID, body: Html): Future[Unit] =
  for {
    user <- usersDao.get(userId)
    _ <- email.send(user.email, body)
  } yield ()
```

Here we have `user` as a result of first operation - getting user from database, we then call `email.send` transformation on `user.email` and some email body `Html` that is not of interest here so it is hidden with `???`.

`send` method from the `Email` contract has a signature matching the second argument list in Monad's `flatMap` definition.

```scala
def send(email: Email, ...): Future[Unit]
```

It takes `A` which is `Email` and transforms it to `Future[Unit]` (return type `F[B]`).

So if we were to rewrite `sendEmailNotification` exactly without Scala's syntactic sugar we could come up with following:

```scala
usersDao.get(userId).map {
  user => sendEmailNotification(order.userId, body).flatMap {
    _ => ()
  }
}
```
                                                   
Let's go line by line here. First we call `get` on our users `DAO` which returns us `Future[User]`. Then we call `map` on `Future[User]` to get the resulting `User` object to transform it to `Future[Unit]` using `sendEmailNotification` by calling `flatMap` on it.

We cannot yet use our `Monad` trait with the exact syntax we use for `Future`, but the idea stays the same. We can achieve exactly the same syntax using Scala's *implicit conversion* to declare `MonadSyntax` described below.

```scala
object Monad {
  implicit class MonadSyntax[F[_], A](fa: F[A])(implicitly m: Monad[F, A]) {
    def map[B](f: A => B): F[B] = m.map(fa)(f)
    def flatMap(f: A => F[B]): F[B] = m.flatMap(fa)(f)
  }
}
```

Assuming we have `MonadSyntax` in scope as well as some *typeclass* defined for *Users* table we could rewrite `sendEmailNotification` business logic using *runtime*-agnostic syntax.

```scala
import Monad.MonadSyntax

trait Users[F[_]] {
  def get(userId: UUID): F[User]
}

object Users {
  def apply()(implicit e: Users[F]): Users[F] = e
}

def sendEmailNotifiation[F : Monad : Users : Email](userId: UUID, body: Html): F[Unit] =
  for {
    user <- Users()..get(userId)
    _ <- Email().send(user.email, body)
  } yield ()
```

If you have made it this far - congratulations! You have just implemented a pattern that is called *Tagless-Final*. If you feel like you get the idea but would require some more simple examples I cannot highly recommend this YouTube stream on applying the same patterns [[2]].

## Takeaway

*Referential transparency* is a really powerful and important property in your codebase to have since it enables you to reason about your code differently freeing you from the requirement to be constantly context-aware.

If you are interested in the idea but not yet fully convinced with provided arguments consider checking out this talk [[3]].

*Tagless final* is a pattern that enables writing referentially-transparent code using Scala programming language by leveraging typeclasses.

## References

[[1]]. Functional Programming Principles in Scala by Martin Odersky

[[2]]. John De Goes Fp To The Max

[[3]]. Runar Bjarnasson: Constrains Liberate Liberties Contrain, Scala World 2015 

[[10]]. Scala Reference on Futures composition

[1]: https://www.coursera.org/learn/progfun1
[2]: https://youtu.be/sxudIMiOo68
[3]: https://youtu.be/GqmsQeSzMdw
[10]: https://docs.scala-lang.org/overviews/core/futures.html#functional-composition-and-for-comprehensions
[11]: https://en.wikipedia.org/wiki/Side_effect_(computer_science)
[12]: https://en.wikipedia.org/wiki/Monad_(functional_programming)
[99]: https://openjdk.java.net/jeps/358
[100]: http://radio-weblogs.com/0122027/stories/2003/04/01/JavasCheckedExceptionsWereAMistake.html             
