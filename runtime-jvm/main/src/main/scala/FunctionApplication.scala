package org.unisonweb

import Runtime._
import Term.{Name,Term}
import annotation.switch

object FunctionApplication {

  def staticCall(fn: Rt, args: Array[Rt], decompile: Term, isTail: Boolean): Rt =
    if (isTail) staticTailCall(fn, args, decompile)
    else staticNonTailCall(fn, args, decompile)

  def staticRecCall(args: Array[Rt], decompile: Term, isTail: Boolean): Rt =
    if (isTail) staticRecTailCall(args, decompile)
    else staticRecNonTailCall(args, decompile)

  def staticRecNonTailCall(args: Array[Rt], decompile: Term): Rt = {
    val arity = args.map(_.arity).max
    val args2 = args
    trait A0 { self: Rt => def bind(env: Map[Name,Rt]) = args2.foreach(_.bind(env)) }
    (arity: @switch) match {
      case 0 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity0(decompile) with A0 { def apply(rec: Rt, r: R) =
            rec(rec,{ try arg0(rec, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
          }
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity0(decompile) with A0 { def apply(rec: Rt, r: R) = {
            val x1 = { try arg0(rec, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            rec(rec,x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity0(decompile) with A0 { def apply(rec: Rt, r: R) = {
            val x1 = { try arg0(rec, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            rec(rec,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity0(decompile) with A0 { def apply(rec: Rt, r: R) = {
            val x1 = { try arg0(rec, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            val x4 = { try arg3(rec, r) catch { case e: TC => loop(e,r) }}; val x4b = r.boxed
            rec(rec,x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity0(decompile) with A0 { def apply(rec: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              slot.unboxed = { try args(i)(rec, r) catch { case e: TC => loop(e,r) }}
              slot.boxed = r.boxed
              i += 1
            }
            rec(rec,slots, r)
          }}
      }
      case 1 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity1(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, r: R) =
            rec(rec,{ try arg0(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
          }
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity1(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            rec(rec,x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity1(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            rec(rec,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity1(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            val x4 = { try arg3(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x4b = r.boxed
            rec(rec,x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity1(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              slot.unboxed = { try args(i)(rec, a1, a1b, r) catch { case e: TC => loop(e,r) }}
              slot.boxed = r.boxed
              i += 1
            }
            rec(rec,slots, r)
          }}
      }
      case 2 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity2(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) =
            rec(rec,{ try arg0(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
          }
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity2(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            rec(rec,x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity2(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            rec(rec,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity2(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            val x4 = { try arg3(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x4b = r.boxed
            rec(rec,x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity2(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              slot.unboxed = { try args(i)(rec, a1, a1b, a2, a2b, r) catch { case e: TC => loop(e,r) }}
              slot.boxed = r.boxed
              i += 1
            }
            rec(rec,slots, r)
          }}
      }
      case 3 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity3(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) =

            rec(rec,{ try arg0(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
          }
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity3(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            rec(rec,x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity3(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            rec(rec,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity3(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            val x4 = { try arg3(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x4b = r.boxed
            rec(rec,x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity3(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              slot.unboxed = { try args(i)(rec, a1, a1b, a2, a2b, a3, a3b, r) catch { case e: TC => loop(e,r) }}
              slot.boxed = r.boxed
              i += 1
            }
            rec(rec,slots, r)
          }}
      }
      case 4 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity4(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            rec(rec,{ try arg0(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity4(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            rec(rec,x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity4(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            rec(rec,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity4(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            val x4 = { try arg3(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x4b = r.boxed
            rec(rec,x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity4(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              slot.unboxed = { try args(i)(rec, a1, a1b, a2, a2b, a3, a3b, a4, a4b, r) catch { case e: TC => loop(e,r) }}
              slot.boxed = r.boxed
              i += 1
            }
            rec(rec,slots, r)
          }}
      }
      case n => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new ArityN(n, decompile) with A0 { def apply(rec: Rt, args: Array[Slot], r: R) = {
            rec(rec,{ try arg0(rec, args, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new ArityN(n, decompile) with A0 { def apply(rec: Rt, args: Array[Slot], r: R) = {
            val x1 = { try arg0(rec, args, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec, args, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            rec(rec,x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new ArityN(n, decompile) with A0 { def apply(rec: Rt, args: Array[Slot], r: R) = {
            val x1 = { try arg0(rec, args, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec, args, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec, args, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            rec(rec,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new ArityN(n, decompile) with A0 { def apply(rec: Rt, args: Array[Slot], r: R) = {
            val x1 = { try arg0(rec, args, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec, args, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec, args, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            val x4 = { try arg3(rec, args, r) catch { case e: TC => loop(e,r) }}; val x4b = r.boxed
            rec(rec,x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case m =>
          new ArityN(n, decompile) with A0 { def apply(rec: Rt, args0: Array[Slot], r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              slot.unboxed = { try args(i)(rec, args0, r) catch { case e: TC => loop(e,r) }}
              slot.boxed = r.boxed
              i += 1
            }
            rec(rec,slots, r)
          }}
      }
    }
  }

  def staticRecTailCall(args: Array[Rt], decompile: Term): Rt = {
    val arity = args.map(_.arity).max
    val args2 = args
    trait A0 { self: Rt => def bind(env: Map[Name,Rt]) = args2.foreach(_.bind(env)) }
    (arity: @switch) match {
      case 0 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity0(decompile) with A0 { def apply(rec: Rt, r: R) =
            tailCall(rec, { try arg0(rec, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
          }
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity0(decompile) with A0 { def apply(rec: Rt, r: R) = {
            val x1 = { try arg0(rec, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            tailCall(rec, x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity0(decompile) with A0 { def apply(rec: Rt, r: R) = {
            val x1 = { try arg0(rec, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            tailCall(rec, x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity0(decompile) with A0 { def apply(rec: Rt, r: R) = {
            val x1 = { try arg0(rec, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            val x4 = { try arg3(rec, r) catch { case e: TC => loop(e,r) }}; val x4b = r.boxed
            tailCall(rec, x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity0(decompile) with A0 { def apply(rec: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              slot.unboxed = { try args(i)(rec, r) catch { case e: TC => loop(e,r) }}
              slot.boxed = r.boxed
              i += 1
            }
            tailCall(rec, slots, r)
          }}
      }
      case 1 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity1(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, r: R) =
            tailCall(rec, { try arg0(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
          }
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity1(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            tailCall(rec, x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity1(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            tailCall(rec, x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity1(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            val x4 = { try arg3(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x4b = r.boxed
            tailCall(rec, x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity1(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              slot.unboxed = { try args(i)(rec, a1, a1b, r) catch { case e: TC => loop(e,r) }}
              slot.boxed = r.boxed
              i += 1
            }
            tailCall(rec, slots, r)
          }}
      }
      case 2 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity2(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) =
            tailCall(rec, { try arg0(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
          }
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity2(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            tailCall(rec, x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity2(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            tailCall(rec, x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity2(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            val x4 = { try arg3(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x4b = r.boxed
            tailCall(rec, x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity2(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              slot.unboxed = { try args(i)(rec, a1, a1b, a2, a2b, r) catch { case e: TC => loop(e,r) }}
              slot.boxed = r.boxed
              i += 1
            }
            tailCall(rec, slots, r)
          }}
      }
      case 3 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity3(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) =

            tailCall(rec, { try arg0(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
          }
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity3(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            tailCall(rec, x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity3(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            tailCall(rec, x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity3(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            val x4 = { try arg3(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x4b = r.boxed
            tailCall(rec, x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity3(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              slot.unboxed = { try args(i)(rec, a1, a1b, a2, a2b, a3, a3b, r) catch { case e: TC => loop(e,r) }}
              slot.boxed = r.boxed
              i += 1
            }
            tailCall(rec, slots, r)
          }}
      }
      case 4 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity4(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            tailCall(rec, { try arg0(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity4(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            tailCall(rec, x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity4(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            tailCall(rec, x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity4(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            val x4 = { try arg3(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x4b = r.boxed
            tailCall(rec, x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity4(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              slot.unboxed = { try args(i)(rec, a1, a1b, a2, a2b, a3, a3b, a4, a4b, r) catch { case e: TC => loop(e,r) }}
              slot.boxed = r.boxed
              i += 1
            }
            tailCall(rec, slots, r)
          }}
      }
      case n => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new ArityN(n, decompile) with A0 { def apply(rec: Rt, args: Array[Slot], r: R) = {
            tailCall(rec, { try arg0(rec, args, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new ArityN(n, decompile) with A0 { def apply(rec: Rt, args: Array[Slot], r: R) = {
            val x1 = { try arg0(rec, args, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec, args, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            tailCall(rec, x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new ArityN(n, decompile) with A0 { def apply(rec: Rt, args: Array[Slot], r: R) = {
            val x1 = { try arg0(rec, args, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec, args, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec, args, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            tailCall(rec, x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new ArityN(n, decompile) with A0 { def apply(rec: Rt, args: Array[Slot], r: R) = {
            val x1 = { try arg0(rec, args, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec, args, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec, args, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            val x4 = { try arg3(rec, args, r) catch { case e: TC => loop(e,r) }}; val x4b = r.boxed
            tailCall(rec, x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case m =>
          new ArityN(n, decompile) with A0 { def apply(rec: Rt, args0: Array[Slot], r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              slot.unboxed = { try args(i)(rec, args0, r) catch { case e: TC => loop(e,r) }}
              slot.boxed = r.boxed
              i += 1
            }
            tailCall(rec, slots, r)
          }}
      }
    }
  }

  def staticTailCall(fn: Rt, args: Array[Rt], decompile: Term): Rt = {
    val arity = args.map(_.arity).max
    val fn2 = fn; val args2 = args
    trait A0 { self: Rt => def bind(env: Map[Name,Rt]) = { fn2.bind(env); args2.foreach(_.bind(env)) }}
    (arity: @switch) match {
      case 0 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity0(decompile) with A0 { def apply(rec: Rt, r: R) =
            tailCall(fn, { try arg0(rec, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
          }
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity0(decompile) with A0 { def apply(rec: Rt, r: R) = {
            val x1 = { try arg0(rec, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            tailCall(fn, x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity0(decompile) with A0 { def apply(rec: Rt, r: R) = {
            val x1 = { try arg0(rec, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            tailCall(fn, x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity0(decompile) with A0 { def apply(rec: Rt, r: R) = {
            val x1 = { try arg0(rec, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            val x4 = { try arg3(rec, r) catch { case e: TC => loop(e,r) }}; val x4b = r.boxed
            tailCall(fn, x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity0(decompile) with A0 { def apply(rec: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              slot.unboxed = { try args(i)(rec, r) catch { case e: TC => loop(e,r) }}
              slot.boxed = r.boxed
              i += 1
            }
            tailCall(fn, slots, r)
          }}
      }
      case 1 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity1(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, r: R) =
            tailCall(fn, { try arg0(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
          }
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity1(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            tailCall(fn, x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity1(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            tailCall(fn, x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity1(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            val x4 = { try arg3(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x4b = r.boxed
            tailCall(fn, x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity1(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              slot.unboxed = { try args(i)(rec, a1, a1b, r) catch { case e: TC => loop(e,r) }}
              slot.boxed = r.boxed
              i += 1
            }
            tailCall(fn, slots, r)
          }}
      }
      case 2 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity2(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) =
            tailCall(fn, { try arg0(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
          }
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity2(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            tailCall(fn, x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity2(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            tailCall(fn, x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity2(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            val x4 = { try arg3(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x4b = r.boxed
            tailCall(fn, x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity2(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              slot.unboxed = { try args(i)(rec, a1, a1b, a2, a2b, r) catch { case e: TC => loop(e,r) }}
              slot.boxed = r.boxed
              i += 1
            }
            tailCall(fn, slots, r)
          }}
      }
      case 3 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity3(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) =

            tailCall(fn, { try arg0(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
          }
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity3(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            tailCall(fn, x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity3(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            tailCall(fn, x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity3(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            val x4 = { try arg3(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x4b = r.boxed
            tailCall(fn, x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity3(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              slot.unboxed = { try args(i)(rec, a1, a1b, a2, a2b, a3, a3b, r) catch { case e: TC => loop(e,r) }}
              slot.boxed = r.boxed
              i += 1
            }
            tailCall(fn, slots, r)
          }}
      }
      case 4 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity4(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            tailCall(fn, { try arg0(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity4(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            tailCall(fn, x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity4(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            tailCall(fn, x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity4(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            val x4 = { try arg3(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x4b = r.boxed
            tailCall(fn, x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity4(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              slot.unboxed = { try args(i)(rec, a1, a1b, a2, a2b, a3, a3b, a4, a4b, r) catch { case e: TC => loop(e,r) }}
              slot.boxed = r.boxed
              i += 1
            }
            tailCall(fn, slots, r)
          }}
      }
      case n => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new ArityN(n, decompile) with A0 { def apply(rec: Rt, args: Array[Slot], r: R) = {
            tailCall(fn, { try arg0(rec, args, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new ArityN(n, decompile) with A0 { def apply(rec: Rt, args: Array[Slot], r: R) = {
            val x1 = { try arg0(rec, args, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec, args, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            tailCall(fn, x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new ArityN(n, decompile) with A0 { def apply(rec: Rt, args: Array[Slot], r: R) = {
            val x1 = { try arg0(rec, args, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec, args, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec, args, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            tailCall(fn, x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new ArityN(n, decompile) with A0 { def apply(rec: Rt, args: Array[Slot], r: R) = {
            val x1 = { try arg0(rec, args, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec, args, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec, args, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            val x4 = { try arg3(rec, args, r) catch { case e: TC => loop(e,r) }}; val x4b = r.boxed
            tailCall(fn, x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case m =>
          new ArityN(n, decompile) with A0 { def apply(rec: Rt, args0: Array[Slot], r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              slot.unboxed = { try args(i)(rec, args0, r) catch { case e: TC => loop(e,r) }}
              slot.boxed = r.boxed
              i += 1
            }
            tailCall(fn, slots, r)
          }}
      }
    }
  }

  def staticNonTailCall(fn: Rt, args: Array[Rt], decompile: Term): Rt = {
    val arity = args.map(_.arity).max
    val fn2 = fn; val args2 = args
    trait A0 { self: Rt => def bind(env: Map[Name,Rt]) = { fn2.bind(env); args2.foreach(_.bind(env)) }}
    (arity: @switch) match {
      case 0 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity0(decompile) with A0 { def apply(rec: Rt, r: R) =
            fn(fn, { try arg0(rec, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
          }
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity0(decompile) with A0 { def apply(rec: Rt, r: R) = {
            val x1 = { try arg0(rec, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            fn(fn, x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity0(decompile) with A0 { def apply(rec: Rt, r: R) = {
            val x1 = { try arg0(rec, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            fn(fn, x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity0(decompile) with A0 { def apply(rec: Rt, r: R) = {
            val x1 = { try arg0(rec, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            val x4 = { try arg3(rec, r) catch { case e: TC => loop(e,r) }}; val x4b = r.boxed
            fn(fn, x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity0(decompile) with A0 { def apply(rec: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              slot.unboxed = { try args(i)(rec, r) catch { case e: TC => loop(e,r) }}
              slot.boxed = r.boxed
              i += 1
            }
            fn(fn, slots, r)
          }}
      }
      case 1 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity1(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, r: R) =
            fn(fn, { try arg0(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
          }
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity1(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            fn(fn, x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity1(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            fn(fn, x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity1(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            val x4 = { try arg3(rec,a1,a1b, r) catch { case e: TC => loop(e,r) }}; val x4b = r.boxed
            fn(fn, x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity1(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              slot.unboxed = { try args(i)(rec, a1, a1b, r) catch { case e: TC => loop(e,r) }}
              slot.boxed = r.boxed
              i += 1
            }
            fn(fn, slots, r)
          }}
      }
      case 2 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity2(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) =
            fn(fn, { try arg0(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
          }
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity2(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            fn(fn, x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity2(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            fn(fn, x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity2(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            val x4 = { try arg3(rec,a1,a1b,a2,a2b, r) catch { case e: TC => loop(e,r) }}; val x4b = r.boxed
            fn(fn, x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity2(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              slot.unboxed = { try args(i)(rec, a1, a1b, a2, a2b, r) catch { case e: TC => loop(e,r) }}
              slot.boxed = r.boxed
              i += 1
            }
            fn(fn, slots, r)
          }}
      }
      case 3 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity3(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) =

            fn(fn, { try arg0(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
          }
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity3(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            fn(fn, x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity3(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            fn(fn, x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity3(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            val x4 = { try arg3(rec,a1,a1b,a2,a2b,a3,a3b, r) catch { case e: TC => loop(e,r) }}; val x4b = r.boxed
            fn(fn, x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity3(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              slot.unboxed = { try args(i)(rec, a1, a1b, a2, a2b, a3, a3b, r) catch { case e: TC => loop(e,r) }}
              slot.boxed = r.boxed
              i += 1
            }
            fn(fn, slots, r)
          }}
      }
      case 4 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity4(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            fn(fn, { try arg0(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity4(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            fn(fn, x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity4(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            fn(fn, x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity4(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            val x1 = { try arg0(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            val x4 = { try arg3(rec,a1,a1b,a2,a2b,a3,a3b,a4,a4b, r) catch { case e: TC => loop(e,r) }}; val x4b = r.boxed
            fn(fn, x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity4(decompile) with A0 { def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              slot.unboxed = { try args(i)(rec, a1, a1b, a2, a2b, a3, a3b, a4, a4b, r) catch { case e: TC => loop(e,r) }}
              slot.boxed = r.boxed
              i += 1
            }
            fn(fn, slots, r)
          }}
      }
      case n => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new ArityN(n, decompile) with A0 { def apply(rec: Rt, args: Array[Slot], r: R) = {
            fn(fn, { try arg0(rec, args, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new ArityN(n, decompile) with A0 { def apply(rec: Rt, args: Array[Slot], r: R) = {
            val x1 = { try arg0(rec, args, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec, args, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            fn(fn, x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new ArityN(n, decompile) with A0 { def apply(rec: Rt, args: Array[Slot], r: R) = {
            val x1 = { try arg0(rec, args, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec, args, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec, args, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            fn(fn, x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new ArityN(n, decompile) with A0 { def apply(rec: Rt, args: Array[Slot], r: R) = {
            val x1 = { try arg0(rec, args, r) catch { case e: TC => loop(e,r) }}; val x1b = r.boxed
            val x2 = { try arg1(rec, args, r) catch { case e: TC => loop(e,r) }}; val x2b = r.boxed
            val x3 = { try arg2(rec, args, r) catch { case e: TC => loop(e,r) }}; val x3b = r.boxed
            val x4 = { try arg3(rec, args, r) catch { case e: TC => loop(e,r) }}; val x4b = r.boxed
            fn(fn, x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case m =>
          new ArityN(n, decompile) with A0 { def apply(rec: Rt, args0: Array[Slot], r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              slot.unboxed = { try args(i)(rec, args0, r) catch { case e: TC => loop(e,r) }}
              slot.boxed = r.boxed
              i += 1
            }
            fn(fn, slots, r)
          }}
      }
    }
  }
}

