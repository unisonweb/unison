package org.unisonweb

import java.util.concurrent.{BlockingQueue,LinkedBlockingQueue,CountDownLatch}
import java.util.Random
import java.lang.Character

object EasyTest {

  case class Env(rand: Random, scope: String, output: BlockingQueue[Msg], activePrefix: String) {
    def int = rand.nextInt
    def intIn(low: Int, highExclusive: Int): Int = low + (rand.nextDouble * (highExclusive - low)).toInt
    def ints(n: Int): Vector[Int] = replicate(n)(int)
    def intsIn(n: Int)(low: Int, highExclusive: Int): Vector[Int] = replicate(n)(intIn(low,highExclusive))

    def long = rand.nextLong
    def longIn(low: Long, highExclusive: Long): Long = low + (rand.nextDouble * (highExclusive - low)).toLong
    def longs(n: Int): Vector[Long] = replicate(n)(long)
    def longsIn(n: Int)(low: Long, highExclusive: Long): Vector[Long] = replicate(n)(longIn(low, highExclusive))

    def double = rand.nextDouble
    def doubleIn(low: Double, highExclusive: Double): Double = low + (rand.nextDouble * (highExclusive - low))
    def doubles(n: Int): Vector[Double] = (0 until n).map(_ => double).toVector
    def doublesIn(n: Int)(low: Int, highExclusive: Int): Vector[Double] = replicate(n)(doubleIn(low,highExclusive))

    def alpha = intIn('A'.toInt, 'z'.toInt).toString
    def alphas(n: Int) = replicate(n)(alpha)
    def alpha123 = if (double < .2) intIn(0,10).toString else alpha
    def alphas123(n: Int) = replicate(n)(alpha123).mkString
    def alphas = replicate(intIn(0,10))(alpha).mkString
    def alphas123 = replicate(intIn(0,10))(alpha123).mkString

    def codepoint: Int =
      intIn(Character.MIN_CODE_POINT, Character.MAX_CODE_POINT + 1)

    /** A codepoint that can be encoded with a single `char` value. */
    @annotation.tailrec
    final def narrowCodepoint: Int = {
      val ch = codepoint
      if (Character.charCount(ch) == 2) narrowCodepoint
      else ch
    }

    /** Occupies 2 Java `char` values. */
    def wideCodepoint: Int = {
      val ch = Character.toCodePoint(
        intIn(Character.MIN_HIGH_SURROGATE, Character.MAX_HIGH_SURROGATE + 1).toChar,
        intIn(Character.MIN_LOW_SURROGATE, Character.MAX_LOW_SURROGATE + 1).toChar)
      require(Character.isValidCodePoint(ch))
      require(Character.charCount(ch) == 2)
      ch
    }

    def string(numCodepoints: Int): String =
      replicate(numCodepoints)(codepoint).foldLeft(new java.lang.StringBuilder)(_ appendCodePoint _).toString

    def string: String = string(intIn(0,10))

    def replicate[A](n: Int)(rand: => A): Vector[A] = (0 until n).map(_ => rand).toVector

    def ok = output.put(Success(scope))
    def fail(reason: String): Nothing = { output.put(Failure(scope, Failed(reason))); throw Skip }
    def note(msg: => Any, includeAlways: Boolean = false) = {
      val latch = new CountDownLatch(1)
      output.put(Note(scope, () => msg.toString, includeAlways, latch))
      latch.await
    }

    def equal[A](a: A, b: A): Unit =
      if (a == b) ok
      else fail(s"$a != $b")

    def equal1[A](a: A, b: A): Unit =
      if (a == b) ()
      else fail(s"$a != $b")

    def expect(b: Boolean): Unit =
      if (b) ok
      else fail("expectation failure")

    def expect1(b: Boolean): Unit =
      if (b) ()
      else fail("expectation failure")
  }

  class Test[+A](val run0: Env => A) {
    def run(implicit env: Env): A = run0(env)
    def map[B](f: A => B): Test[B] = new Test(run0 andThen f)
    def flatMap[B](f: A => Test[B]): Test[B] = new Test(env => f(run0(env)).run(env))
  }

  sealed trait Msg
  case object Done extends Msg
  case class Success(scope: String) extends Msg
  case class Failure(scope: String, cause: Throwable) extends Msg
  case class Note(scope: String, msg: () => String, includeAlways: Boolean = false, latch: CountDownLatch) extends Msg
  case class Failed(reason: String) extends Throwable { override def toString = reason }

  case object Skip extends Throwable { override def fillInStackTrace = this }
  case object Disable extends Throwable { override def fillInStackTrace = this }

  def test[A](s: String)(run0: Env => A): Test[A] = scope(s)(test(run0))

  def test[A](run0: Env => A): Test[A] = new Test[A](env => {
    if (env.scope.startsWith(env.activePrefix)) run0(env)
    else throw Skip
  })

  def note(msg: => Any, includeAlways: Boolean = false)(implicit T: Env): Unit = T.note(msg, includeAlways)
  def ok(implicit T: Env): Unit = T.ok
  def fail(reason: String)(implicit T: Env): Nothing = T.fail(reason)
  def disable(implicit T: Env): Nothing = { T.output.put(Failure(T.scope, Disable)); throw Skip }
  def exception(t: Throwable)(implicit T: Env): Nothing = { T.output.put(Failure(T.scope, t)); throw Skip }
  def expect(b: Boolean)(implicit T: Env): Unit = T.expect(b)
  def expect1(b: Boolean)(implicit T: Env): Unit = T.expect1(b)
  def equal[A](a1: A, a2: A)(implicit T: Env): Unit = T.equal(a1, a2)
  def equal1[A](a1: A, a2: A)(implicit T: Env): Unit = T.equal1(a1, a2)
  def int(implicit T: Env) = T.rand.nextInt
  def intIn(low: Int, highExclusive: Int)(implicit T: Env): Int = low + (T.rand.nextDouble * (highExclusive - low)).toInt
  def ints(n: Int)(implicit T: Env): Vector[Int] = replicate(n)(int)
  def intsIn(n: Int)(low: Int, highExclusive: Int)(implicit T: Env): Vector[Int] = replicate(n)(intIn(low,highExclusive))
  def long(implicit T: Env) = T.rand.nextLong
  def longIn(low: Long, highExclusive: Long)(implicit T: Env): Long = low + (T.rand.nextDouble * (highExclusive - low)).toLong
  def longs(n: Int)(implicit T: Env): Vector[Long] = replicate(n)(long)
  def longsIn(n: Int)(low: Long, highExclusive: Long)(implicit T: Env): Vector[Long] = replicate(n)(longIn(low, highExclusive))
  def double(implicit T: Env) = T.rand.nextDouble
  def doubleIn(low: Double, highExclusive: Double)(implicit T: Env): Double = low + (T.rand.nextDouble * (highExclusive - low))
  def doubles(n: Int)(implicit T: Env): Vector[Double] = (0 until n).map(_ => double).toVector
  def doublesIn(n: Int)(low: Int, highExclusive: Int)(implicit T: Env): Vector[Double] = replicate(n)(doubleIn(low,highExclusive))
  def alpha(implicit T: Env) = intIn('A'.toInt, 'z'.toInt).toString
  def alphas(n: Int)(implicit T: Env) = replicate(n)(alpha)
  def alpha123(implicit T: Env) = if (double < .2) intIn(0,10).toString else alpha
  def alphas123(n: Int)(implicit T: Env) = replicate(n)(alpha123).mkString
  def alphas(implicit T: Env) = replicate(intIn(0,10))(alpha).mkString
  def alphas123(implicit T: Env) = replicate(intIn(0,10))(alpha123).mkString
  def codepoint(implicit T: Env) = T.codepoint
  def narrowCodepoint(implicit T: Env) = T.narrowCodepoint
  def wideCodepoint(implicit T: Env) = T.wideCodepoint
  def string(numCodepoints: Int)(implicit T: Env) = T.string(numCodepoints)
  def string(implicit T: Env) = T.string(intIn(0,10))
  def replicate[A](n: Int)(rand: => A)(implicit T: Env): Vector[A] = (0 until n).map(_ => rand).toVector
  def choose[A](a1: => A, a2: => A)(implicit T: Env): A =
    if (double < .5) a1 else a2

  /** Push `s` onto the scope stack. */
  def scope[A](s: String)(t: Test[A]): Test[A] = test { env =>
    try t.run(env.copy(scope = concatScope(env.scope, s)))
    catch {
      case Skip => throw Skip
      case t: Throwable => env.output.put(Failure(concatScope(env.scope, s), t)); throw Skip
    }
  }
  private def concatScope(a: String, b: String) =
    if (a == "") b
    else if (b == "") a
    else a + "." + b

  def suite(s: String)(t: Test[Unit]*): Test[Unit] = scope(s)(suite(t: _*))

  def suite(t: Test[Unit]*): Test[Unit] = test { implicit T =>
    val n = T.long
    (0 until t.size).foreach { i =>
      T.rand.setSeed(n)
      try t(i).run
      catch {
        case Skip => ()
        case e: Throwable => exception(e)
      }
    }
  }

  def run(seed: Long = (math.random * 100000).toLong, prefix: String = "")(t: Test[Unit]): Unit = {
    val q = new LinkedBlockingQueue[Msg]()
    val latch = new CountDownLatch(1)
    val rand = new Random(seed)
    implicit val T = Env(rand, "", q, prefix)
    var failures = Vector.empty[(String, Throwable)]
    var successes: Int = 0
    val bg = new Thread { override def run = {
      var moar = true
      var lastScope = "**"
      var numSuccessCurrentScope = 1
      var pendingNotes = Vector.empty[(String, () => String)]

      def printNote(scope: String, msg: () => String, printPendingOnFailure: Boolean = false): Unit =
        try println("   " + scope + " - " + msg().replace("\n", "\n    "))
        catch { case cause: Throwable => q.put(Failure(scope, cause)) }

      while(moar) {
        val msg = q.take
        msg match {
          case Done => { moar = false; latch.countDown }
          case Success(scope) =>
            successes += 1
            pendingNotes = Vector.empty
            if (scope == lastScope) {
              numSuccessCurrentScope += 1
              println("🦄  " + scope + s"($numSuccessCurrentScope)")
            }
            else { println("🦄  " + scope); lastScope = scope; numSuccessCurrentScope = 0 }
          case Failure(scope, Disable) =>
            println("😴  " + scope + " - disabled")
          case Failure(scope, cause) =>
            failures = failures :+ (scope -> cause)
            pendingNotes.foreach { case (scope,msg) => println("   " + scope + " - " + msg().replace("\n", "\n    ")) }
            pendingNotes = Vector.empty
            println("💥  " + scope + " - " + cause.toString.replace("\n","\n  "))
            cause.printStackTrace
          case Note(scope, msg, includeAlways, latch) =>
            if (includeAlways) printNote(scope, msg)
            else pendingNotes = pendingNotes :+ (scope -> msg)
            latch.countDown
        }
      }
    }}
    bg.start
    println("STARTING TESTS, raw output to follow ... ")
    println("--------------------------------------------------\n")
    try t.run
    catch {
      case Skip => ()
      case t: Throwable => exception(t)
    }
    finally q.put(Done)
    latch.await
    println("\n--------------------------------------------------\n")
    if (failures.isEmpty && successes == 0) {
      println("😶  hmm ... no test results recorded")
      println("Tip: use `ok`, `expect`, or `fail` to record results")
      println("Tip: also check for typos if you are running a subsuite of overall tests")
    }
    else if (failures.isEmpty) {
      println(s"✅  $successes tests passed, no failures! 👍 🎉 🦄 💯 🌈")
    }
    else {
      println(s"  $successes passed")
      println(s"  ${failures.size} FAILED (failed scopes below)")
      failures.foreach { case (scope, reason) =>
        println("    " + scope + " - " + reason.toString)
      }
      val formattedFailure = '"' + failures(0)._1 + '"'
      println("\n  Oh NOES! There were failures. Rerun with: \n")
      println(s"  EasyTest.run(seed = $seed, prefix = $formattedFailure)(tests)")
      println("")
      println("👹 💥 ")
      System.exit(1)
    }
  }
}
