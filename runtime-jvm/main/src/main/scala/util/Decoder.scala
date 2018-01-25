package org.unisonweb.util

import java.nio.ByteBuffer
import java.nio.charset.Charset
import Decoder._

abstract class Decoder[+A] { self =>
  def stage(buf: ByteBuffer): () => A

  def scope(msg: String) = new Decoder[A] {
    def stage(buf: ByteBuffer) = {
      val inner = self.stage(buf)
      () => {
        try inner()
        catch { case Failure(msgs) => throw Failure(msg :: msgs) }
      }
    }
  }

  def map[B](f: A => B): Decoder[B] = new Decoder[B] {
    def stage(buf: ByteBuffer) = {
      val inner = self.stage(buf)
      () => f(inner())
    }
  }
}

object Decoder {

  case class Failure(msgs: List[String]) extends Throwable

  def double: Decoder[Double] = new Decoder[Double] {
    def stage(buf: ByteBuffer) = () => buf.getDouble
  }

  /**
   * Strings are encoded as a length (encoded as a signed int value)
   * followed by exactly that many bytes, utf8 encoded.
   */
  def utf8: Decoder[String] = new Decoder[String] {
    def stage(buf: ByteBuffer) = {
      val utf8Charset = Charset.forName("UTF-8")
      val scratch = new Array[Byte](128)
      () => {
        val length = buf.getInt
        if (length < scratch.length) {
          buf.get(scratch, 0, length)
          utf8Charset.decode(ByteBuffer.wrap(scratch, 0, length)).toString
        }
        else {
          val scratch = new Array[Byte](length)
          buf.get(scratch)
          utf8Charset.decode(ByteBuffer.wrap(scratch)).toString
        }
      }
    }
  }

  def product[A,B,R](a: Decoder[A], b: Decoder[B])(f: (A,B) => R): Decoder[R] = new Decoder[R] {
    def stage(buf: ByteBuffer) = {
      val sa = a.stage(buf)
      val sb = b.stage(buf)
      () => f(sa(), sb())
    }
  }

  def product[A,B,C,R](a: Decoder[A], b: Decoder[B], c: Decoder[C])(f: (A,B,C) => R): Decoder[R] = new Decoder[R] {
    def stage(buf: ByteBuffer) = {
      val sa = a.stage(buf)
      val sb = b.stage(buf)
      val sc = c.stage(buf)
      () => f(sa(), sb(), sc())
    }
  }

  def sum[R](r1: Decoder[R], r2: Decoder[R], r3: Decoder[R]): Decoder[R] = new Decoder[R] {
    def stage(buf: ByteBuffer) = {
      val sr1 = r1.stage(buf)
      val sr2 = r2.stage(buf)
      val sr3 = r3.stage(buf)
      () => {
        (buf.get : @annotation.switch) match {
          case 0 => sr1()
          case 1 => sr2()
          case 2 => sr3()
          case b => throw Failure(List("unknown byte in 3-way sum: " + b))
        }
      }
    }
  }

  def sum[R](rs: Decoder[R]*): Decoder[R] = new Decoder[R] {
    def stage(buf: ByteBuffer) = {
      val srs = rs.map(_ stage buf).toArray
      () => srs(buf.get.toInt)()
    }
  }

  def fix[R](f: (=> Decoder[R]) => Decoder[R]): Decoder[R] = ???
}
