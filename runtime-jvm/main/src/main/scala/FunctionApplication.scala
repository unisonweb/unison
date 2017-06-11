package org.unisonweb

import Runtime._
import Term.{Name,Term}
import annotation.switch

object FunctionApplication {

  def staticCall(fn: Rt, args: Array[Rt], decompile: Term, isTail: Boolean): Rt =
    if (isTail) staticTailCall(fn, args, decompile)
    else staticNonTailCall(fn, args, decompile)

  def staticTailCall(fn: Rt, args: Array[Rt], decompile: Term): Rt = {
    val arity = args.map(_.arity).max
    trait A0 { self: Rt => def bind(env: Map[Name,Rt]) = { fn.bind(env); args.foreach(_.bind(env)) }}
    (arity: @switch) match {
      case 0 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity0(decompile) with A0 { def apply(r: R) = {
            eval(arg0, r)
            tailCall(fn, r.unboxed, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity0(decompile) with A0 { def apply(r: R) = {
            eval(arg0, r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1, r); val x2 = r.unboxed; val x2b = r.boxed
            tailCall(fn, x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity0(decompile) with A0 { def apply(r: R) = {
            eval(arg0, r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1, r); val x2 = r.unboxed; val x2b = r.boxed
            eval(arg2, r); val x3 = r.unboxed; val x3b = r.boxed
            tailCall(fn, x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity0(decompile) with A0 { def apply(r: R) = {
            eval(arg0, r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1, r); val x2 = r.unboxed; val x2b = r.boxed
            eval(arg2, r); val x3 = r.unboxed; val x3b = r.boxed
            eval(arg3, r); val x4 = r.unboxed; val x4b = r.boxed
            tailCall(fn, x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity0(decompile) with A0 { def apply(r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              eval(args(i), r)
              slot.unboxed = r.unboxed
              slot.boxed = r.boxed
              i += 1
            }
            tailCall(fn, slots, r)
          }}
      }
      case 1 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity1(decompile) with A0 { def apply(a1: D, a1b: Rt, r: R) = {
            eval(arg0,a1,a1b,r)
            tailCall(fn, r.unboxed, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity1(decompile) with A0 { def apply(a1: D, a1b: Rt, r: R) = {
            eval(arg0,a1,a1b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1,a1,a1b,r); val x2 = r.unboxed; val x2b = r.boxed
            tailCall(fn, x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity1(decompile) with A0 { def apply(a1: D, a1b: Rt, r: R) = {
            eval(arg0,a1,a1b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1,a1,a1b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval(arg2,a1,a1b,r); val x3 = r.unboxed; val x3b = r.boxed
            tailCall(fn, x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity1(decompile) with A0 { def apply(a1: D, a1b: Rt, r: R) = {
            eval(arg0,a1,a1b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1,a1,a1b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval(arg2,a1,a1b,r); val x3 = r.unboxed; val x3b = r.boxed
            eval(arg3,a1,a1b,r); val x4 = r.unboxed; val x4b = r.boxed
            tailCall(fn, x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity1(decompile) with A0 { def apply(a1: D, a1b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              eval(args(i), a1, a1b, r)
              slot.unboxed = r.unboxed
              slot.boxed = r.boxed
              i += 1
            }
            tailCall(fn, slots, r)
          }}
      }
      case 2 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity2(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            eval(arg0,a1,a1b,a2,a2b,r)
            tailCall(fn, r.unboxed, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity2(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            eval(arg0,a1,a1b,a2,a2b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1,a1,a1b,a2,a2b,r); val x2 = r.unboxed; val x2b = r.boxed
            tailCall(fn, x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity2(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            eval(arg0,a1,a1b,a2,a2b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1,a1,a1b,a2,a2b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval(arg2,a1,a1b,a2,a2b,r); val x3 = r.unboxed; val x3b = r.boxed
            tailCall(fn, x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity2(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            eval(arg0,a1,a1b,a2,a2b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1,a1,a1b,a2,a2b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval(arg2,a1,a1b,a2,a2b,r); val x3 = r.unboxed; val x3b = r.boxed
            eval(arg3,a1,a1b,a2,a2b,r); val x4 = r.unboxed; val x4b = r.boxed
            tailCall(fn, x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity2(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              eval(args(i), a1, a1b, a2, a2b, r)
              slot.unboxed = r.unboxed
              slot.boxed = r.boxed
              i += 1
            }
            tailCall(fn, slots, r)
          }}
      }
      case 3 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity3(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            eval(arg0,a1,a1b,a2,a2b,a3,a3b,r)
            tailCall(fn, r.unboxed, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity3(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            eval(arg0,a1,a1b,a2,a2b,a3,a3b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1,a1,a1b,a2,a2b,a3,a3b,r); val x2 = r.unboxed; val x2b = r.boxed
            tailCall(fn, x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity3(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            eval(arg0,a1,a1b,a2,a2b,a3,a3b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1,a1,a1b,a2,a2b,a3,a3b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval(arg2,a1,a1b,a2,a2b,a3,a3b,r); val x3 = r.unboxed; val x3b = r.boxed
            tailCall(fn, x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity3(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            eval(arg0,a1,a1b,a2,a2b,a3,a3b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1,a1,a1b,a2,a2b,a3,a3b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval(arg2,a1,a1b,a2,a2b,a3,a3b,r); val x3 = r.unboxed; val x3b = r.boxed
            eval(arg3,a1,a1b,a2,a2b,a3,a3b,r); val x4 = r.unboxed; val x4b = r.boxed
            tailCall(fn, x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity3(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              eval(args(i), a1, a1b, a2, a2b, a3, a3b, r)
              slot.unboxed = r.unboxed
              slot.boxed = r.boxed
              i += 1
            }
            tailCall(fn, slots, r)
          }}
      }
      case 4 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity4(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            eval(arg0,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r)
            tailCall(fn, r.unboxed, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity4(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            eval(arg0,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x2 = r.unboxed; val x2b = r.boxed
            tailCall(fn, x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity4(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            eval(arg0,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval(arg2,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x3 = r.unboxed; val x3b = r.boxed
            tailCall(fn, x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity4(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            eval(arg0,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval(arg2,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x3 = r.unboxed; val x3b = r.boxed
            eval(arg3,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x4 = r.unboxed; val x4b = r.boxed
            tailCall(fn, x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity4(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              eval(args(i), a1, a1b, a2, a2b, a3, a3b, a4, a4b, r)
              slot.unboxed = r.unboxed
              slot.boxed = r.boxed
              i += 1
            }
            tailCall(fn, slots, r)
          }}
      }
      case n => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new ArityN(n, decompile) with A0 { def apply(args: Array[Slot], r: R) = {
            evalN(arg0, args, r)
            tailCall(fn, r.unboxed, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new ArityN(n, decompile) with A0 { def apply(args: Array[Slot], r: R) = {
            evalN(arg0, args, r); val x1 = r.unboxed; val x1b = r.boxed
            evalN(arg1, args, r); val x2 = r.unboxed; val x2b = r.boxed
            tailCall(fn, x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new ArityN(n, decompile) with A0 { def apply(args: Array[Slot], r: R) = {
            evalN(arg0, args, r); val x1 = r.unboxed; val x1b = r.boxed
            evalN(arg1, args, r); val x2 = r.unboxed; val x2b = r.boxed
            evalN(arg2, args, r); val x3 = r.unboxed; val x3b = r.boxed
            tailCall(fn, x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new ArityN(n, decompile) with A0 { def apply(args: Array[Slot], r: R) = {
            evalN(arg0, args, r); val x1 = r.unboxed; val x1b = r.boxed
            evalN(arg1, args, r); val x2 = r.unboxed; val x2b = r.boxed
            evalN(arg2, args, r); val x3 = r.unboxed; val x3b = r.boxed
            evalN(arg3, args, r); val x4 = r.unboxed; val x4b = r.boxed
            tailCall(fn, x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case m =>
          new ArityN(n, decompile) with A0 { def apply(args0: Array[Slot], r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              evalN(args(i), args0, r)
              slot.unboxed = r.unboxed
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
    trait A0 { self: Rt => def bind(env: Map[Name,Rt]) = { fn.bind(env); args.foreach(_.bind(env)) }}
    (arity: @switch) match {
      case 0 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity0(decompile) with A0 { def apply(r: R) = {
            eval(arg0, r)
            fn(r.unboxed, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity0(decompile) with A0 { def apply(r: R) = {
            eval(arg0, r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1, r); val x2 = r.unboxed; val x2b = r.boxed
            fn(x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity0(decompile) with A0 { def apply(r: R) = {
            eval(arg0, r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1, r); val x2 = r.unboxed; val x2b = r.boxed
            eval(arg2, r); val x3 = r.unboxed; val x3b = r.boxed
            fn(x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity0(decompile) with A0 { def apply(r: R) = {
            eval(arg0, r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1, r); val x2 = r.unboxed; val x2b = r.boxed
            eval(arg2, r); val x3 = r.unboxed; val x3b = r.boxed
            eval(arg3, r); val x4 = r.unboxed; val x4b = r.boxed
            fn(x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity0(decompile) with A0 { def apply(r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              eval(args(i), r)
              slot.unboxed = r.unboxed
              slot.boxed = r.boxed
              i += 1
            }
            fn(slots, r)
          }}
      }
      case 1 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity1(decompile) with A0 { def apply(a1: D, a1b: Rt, r: R) = {
            eval(arg0,a1,a1b,r)
            fn(r.unboxed, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity1(decompile) with A0 { def apply(a1: D, a1b: Rt, r: R) = {
            eval(arg0,a1,a1b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1,a1,a1b,r); val x2 = r.unboxed; val x2b = r.boxed
            fn(x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity1(decompile) with A0 { def apply(a1: D, a1b: Rt, r: R) = {
            eval(arg0,a1,a1b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1,a1,a1b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval(arg2,a1,a1b,r); val x3 = r.unboxed; val x3b = r.boxed
            fn(x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity1(decompile) with A0 { def apply(a1: D, a1b: Rt, r: R) = {
            eval(arg0,a1,a1b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1,a1,a1b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval(arg2,a1,a1b,r); val x3 = r.unboxed; val x3b = r.boxed
            eval(arg3,a1,a1b,r); val x4 = r.unboxed; val x4b = r.boxed
            fn(x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity1(decompile) with A0 { def apply(a1: D, a1b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              eval(args(i), a1, a1b, r)
              slot.unboxed = r.unboxed
              slot.boxed = r.boxed
              i += 1
            }
            fn(slots, r)
          }}
      }
      case 2 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity2(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            eval(arg0,a1,a1b,a2,a2b,r)
            fn(r.unboxed, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity2(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            eval(arg0,a1,a1b,a2,a2b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1,a1,a1b,a2,a2b,r); val x2 = r.unboxed; val x2b = r.boxed
            fn(x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity2(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            eval(arg0,a1,a1b,a2,a2b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1,a1,a1b,a2,a2b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval(arg2,a1,a1b,a2,a2b,r); val x3 = r.unboxed; val x3b = r.boxed
            fn(x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity2(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            eval(arg0,a1,a1b,a2,a2b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1,a1,a1b,a2,a2b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval(arg2,a1,a1b,a2,a2b,r); val x3 = r.unboxed; val x3b = r.boxed
            eval(arg3,a1,a1b,a2,a2b,r); val x4 = r.unboxed; val x4b = r.boxed
            fn(x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity2(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              eval(args(i), a1, a1b, a2, a2b, r)
              slot.unboxed = r.unboxed
              slot.boxed = r.boxed
              i += 1
            }
            fn(slots, r)
          }}
      }
      case 3 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity3(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            eval(arg0,a1,a1b,a2,a2b,a3,a3b,r)
            fn(r.unboxed, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity3(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            eval(arg0,a1,a1b,a2,a2b,a3,a3b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1,a1,a1b,a2,a2b,a3,a3b,r); val x2 = r.unboxed; val x2b = r.boxed
            fn(x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity3(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            eval(arg0,a1,a1b,a2,a2b,a3,a3b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1,a1,a1b,a2,a2b,a3,a3b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval(arg2,a1,a1b,a2,a2b,a3,a3b,r); val x3 = r.unboxed; val x3b = r.boxed
            fn(x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity3(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            eval(arg0,a1,a1b,a2,a2b,a3,a3b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1,a1,a1b,a2,a2b,a3,a3b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval(arg2,a1,a1b,a2,a2b,a3,a3b,r); val x3 = r.unboxed; val x3b = r.boxed
            eval(arg3,a1,a1b,a2,a2b,a3,a3b,r); val x4 = r.unboxed; val x4b = r.boxed
            fn(x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity3(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              eval(args(i), a1, a1b, a2, a2b, a3, a3b, r)
              slot.unboxed = r.unboxed
              slot.boxed = r.boxed
              i += 1
            }
            fn(slots, r)
          }}
      }
      case 4 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity4(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            eval(arg0,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r)
            fn(r.unboxed, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity4(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            eval(arg0,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x2 = r.unboxed; val x2b = r.boxed
            fn(x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity4(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            eval(arg0,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval(arg2,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x3 = r.unboxed; val x3b = r.boxed
            fn(x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity4(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            eval(arg0,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval(arg1,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval(arg2,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x3 = r.unboxed; val x3b = r.boxed
            eval(arg3,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x4 = r.unboxed; val x4b = r.boxed
            fn(x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case n =>
          new Arity4(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              eval(args(i), a1, a1b, a2, a2b, a3, a3b, a4, a4b, r)
              slot.unboxed = r.unboxed
              slot.boxed = r.boxed
              i += 1
            }
            fn(slots, r)
          }}
      }
      case n => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new ArityN(n, decompile) with A0 { def apply(args: Array[Slot], r: R) = {
            evalN(arg0, args, r)
            fn(r.unboxed, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new ArityN(n, decompile) with A0 { def apply(args: Array[Slot], r: R) = {
            evalN(arg0, args, r); val x1 = r.unboxed; val x1b = r.boxed
            evalN(arg1, args, r); val x2 = r.unboxed; val x2b = r.boxed
            fn(x2,x2b,x1,x1b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new ArityN(n, decompile) with A0 { def apply(args: Array[Slot], r: R) = {
            evalN(arg0, args, r); val x1 = r.unboxed; val x1b = r.boxed
            evalN(arg1, args, r); val x2 = r.unboxed; val x2b = r.boxed
            evalN(arg2, args, r); val x3 = r.unboxed; val x3b = r.boxed
            fn(x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new ArityN(n, decompile) with A0 { def apply(args: Array[Slot], r: R) = {
            evalN(arg0, args, r); val x1 = r.unboxed; val x1b = r.boxed
            evalN(arg1, args, r); val x2 = r.unboxed; val x2b = r.boxed
            evalN(arg2, args, r); val x3 = r.unboxed; val x3b = r.boxed
            evalN(arg3, args, r); val x4 = r.unboxed; val x4b = r.boxed
            fn(x4,x4b,x3,x3b,x2,x2b,x1,x1b,r)
          }}
        case m =>
          new ArityN(n, decompile) with A0 { def apply(args0: Array[Slot], r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              evalN(args(i), args0, r)
              slot.unboxed = r.unboxed
              slot.boxed = r.boxed
              i += 1
            }
            fn(slots, r)
          }}
      }
    }
  }
}

