package org.unisonweb

import Runtime._
import annotation.switch

object FunctionApplication {

  def staticFullySaturatedCall(fn: Rt, args: Array[Rt], decompile: TermC, isTail: Boolean): Rt =
    if (isTail) staticFullySaturatedTailCall(fn, args, decompile)
    else staticFullySaturatedNonTailCall(fn, args, decompile)

  def staticFullySaturatedTailCall(fn: Rt, args: Array[Rt], decompile: TermC): Rt = {
    val arity = args.map(_.arity).max
    (arity: @switch) match {
      case 0 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity0(decompile) { def apply(r: R) = {
            eval0(arg0, r)
            tailCall(fn, r.unboxed, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity0(decompile) { def apply(r: R) = {
            eval0(arg0, r); val x1 = r.unboxed; val x1b = r.boxed
            eval0(arg1, r); val x2 = r.unboxed; val x2b = r.boxed
            tailCall(fn, x1,x1b,x2,x2b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity0(decompile) { def apply(r: R) = {
            eval0(arg0, r); val x1 = r.unboxed; val x1b = r.boxed
            eval0(arg1, r); val x2 = r.unboxed; val x2b = r.boxed
            eval0(arg2, r); val x3 = r.unboxed; val x3b = r.boxed
            tailCall(fn, x1,x1b,x2,x2b,x3,x3b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity0(decompile) { def apply(r: R) = {
            eval0(arg0, r); val x1 = r.unboxed; val x1b = r.boxed
            eval0(arg1, r); val x2 = r.unboxed; val x2b = r.boxed
            eval0(arg2, r); val x3 = r.unboxed; val x3b = r.boxed
            eval0(arg3, r); val x4 = r.unboxed; val x4b = r.boxed
            tailCall(fn, x1,x1b,x2,x2b,x3,x3b,x4,x4b,r)
          }}
        case n =>
          new Arity0(decompile) { def apply(r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(i)
              eval0(args(i), r)
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
          new Arity1(decompile) { def apply(a1: D, a1b: Rt, r: R) = {
            eval1(arg0,a1,a1b,r)
            tailCall(fn, r.unboxed, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity1(decompile) { def apply(a1: D, a1b: Rt, r: R) = {
            eval1(arg0,a1,a1b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval1(arg1,a1,a1b,r); val x2 = r.unboxed; val x2b = r.boxed
            tailCall(fn, x1,x1b,x2,x2b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity1(decompile) { def apply(a1: D, a1b: Rt, r: R) = {
            eval1(arg0,a1,a1b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval1(arg1,a1,a1b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval1(arg2,a1,a1b,r); val x3 = r.unboxed; val x3b = r.boxed
            tailCall(fn, x1,x1b,x2,x2b,x3,x3b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity1(decompile) { def apply(a1: D, a1b: Rt, r: R) = {
            eval1(arg0,a1,a1b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval1(arg1,a1,a1b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval1(arg2,a1,a1b,r); val x3 = r.unboxed; val x3b = r.boxed
            eval1(arg3,a1,a1b,r); val x4 = r.unboxed; val x4b = r.boxed
            tailCall(fn, x1,x1b,x2,x2b,x3,x3b,x4,x4b,r)
          }}
        case n =>
          new Arity1(decompile) { def apply(a1: D, a1b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(i)
              eval1(args(i), a1, a1b, r)
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
          new Arity2(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            eval2(arg0,a1,a1b,a2,a2b,r)
            tailCall(fn, r.unboxed, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity2(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            eval2(arg0,a1,a1b,a2,a2b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval2(arg1,a1,a1b,a2,a2b,r); val x2 = r.unboxed; val x2b = r.boxed
            tailCall(fn, x1,x1b,x2,x2b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity2(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            eval2(arg0,a1,a1b,a2,a2b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval2(arg1,a1,a1b,a2,a2b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval2(arg2,a1,a1b,a2,a2b,r); val x3 = r.unboxed; val x3b = r.boxed
            tailCall(fn, x1,x1b,x2,x2b,x3,x3b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity2(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            eval2(arg0,a1,a1b,a2,a2b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval2(arg1,a1,a1b,a2,a2b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval2(arg2,a1,a1b,a2,a2b,r); val x3 = r.unboxed; val x3b = r.boxed
            eval2(arg3,a1,a1b,a2,a2b,r); val x4 = r.unboxed; val x4b = r.boxed
            tailCall(fn, x1,x1b,x2,x2b,x3,x3b,x4,x4b,r)
          }}
        case n =>
          new Arity2(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(i)
              eval2(args(i), a1, a1b, a2, a2b, r)
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
          new Arity3(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            eval3(arg0,a1,a1b,a2,a2b,a3,a3b,r)
            tailCall(fn, r.unboxed, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity3(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            eval3(arg0,a1,a1b,a2,a2b,a3,a3b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval3(arg1,a1,a1b,a2,a2b,a3,a3b,r); val x2 = r.unboxed; val x2b = r.boxed
            tailCall(fn, x1,x1b,x2,x2b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity3(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            eval3(arg0,a1,a1b,a2,a2b,a3,a3b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval3(arg1,a1,a1b,a2,a2b,a3,a3b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval3(arg2,a1,a1b,a2,a2b,a3,a3b,r); val x3 = r.unboxed; val x3b = r.boxed
            tailCall(fn, x1,x1b,x2,x2b,x3,x3b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity3(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            eval3(arg0,a1,a1b,a2,a2b,a3,a3b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval3(arg1,a1,a1b,a2,a2b,a3,a3b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval3(arg2,a1,a1b,a2,a2b,a3,a3b,r); val x3 = r.unboxed; val x3b = r.boxed
            eval3(arg3,a1,a1b,a2,a2b,a3,a3b,r); val x4 = r.unboxed; val x4b = r.boxed
            tailCall(fn, x1,x1b,x2,x2b,x3,x3b,x4,x4b,r)
          }}
        case n =>
          new Arity3(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(i)
              eval3(args(i), a1, a1b, a2, a2b, a3, a3b, r)
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
          new Arity4(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            eval4(arg0,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r)
            tailCall(fn, r.unboxed, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity4(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            eval4(arg0,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval4(arg1,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x2 = r.unboxed; val x2b = r.boxed
            tailCall(fn, x1,x1b,x2,x2b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity4(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            eval4(arg0,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval4(arg1,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval4(arg2,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x3 = r.unboxed; val x3b = r.boxed
            tailCall(fn, x1,x1b,x2,x2b,x3,x3b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity4(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            eval4(arg0,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval4(arg1,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval4(arg2,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x3 = r.unboxed; val x3b = r.boxed
            eval4(arg3,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x4 = r.unboxed; val x4b = r.boxed
            tailCall(fn, x1,x1b,x2,x2b,x3,x3b,x4,x4b,r)
          }}
        case n =>
          new Arity4(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(i)
              eval4(args(i), a1, a1b, a2, a2b, a3, a3b, a4, a4b, r)
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
          new ArityN(n, decompile) { def apply(args: Array[Slot], r: R) = {
            evalN(arg0, args, r)
            tailCall(fn, r.unboxed, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new ArityN(n, decompile) { def apply(args: Array[Slot], r: R) = {
            evalN(arg0, args, r); val x1 = r.unboxed; val x1b = r.boxed
            evalN(arg1, args, r); val x2 = r.unboxed; val x2b = r.boxed
            tailCall(fn, x1,x1b,x2,x2b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new ArityN(n, decompile) { def apply(args: Array[Slot], r: R) = {
            evalN(arg0, args, r); val x1 = r.unboxed; val x1b = r.boxed
            evalN(arg1, args, r); val x2 = r.unboxed; val x2b = r.boxed
            evalN(arg2, args, r); val x3 = r.unboxed; val x3b = r.boxed
            tailCall(fn, x1,x1b,x2,x2b,x3,x3b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new ArityN(n, decompile) { def apply(args: Array[Slot], r: R) = {
            evalN(arg0, args, r); val x1 = r.unboxed; val x1b = r.boxed
            evalN(arg1, args, r); val x2 = r.unboxed; val x2b = r.boxed
            evalN(arg2, args, r); val x3 = r.unboxed; val x3b = r.boxed
            evalN(arg3, args, r); val x4 = r.unboxed; val x4b = r.boxed
            tailCall(fn, x1,x1b,x2,x2b,x3,x3b,x4,x4b,r)
          }}
        case m =>
          new ArityN(n, decompile) { def apply(args0: Array[Slot], r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(i)
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

  def staticFullySaturatedNonTailCall(fn: Rt, args: Array[Rt], decompile: TermC): Rt = {
    val arity = args.map(_.arity).max
    (arity: @switch) match {
      case 0 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity0(decompile) { def apply(r: R) = {
            eval0(arg0, r)
            fn(r.unboxed, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity0(decompile) { def apply(r: R) = {
            eval0(arg0, r); val x1 = r.unboxed; val x1b = r.boxed
            eval0(arg1, r); val x2 = r.unboxed; val x2b = r.boxed
            fn(x1,x1b,x2,x2b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity0(decompile) { def apply(r: R) = {
            eval0(arg0, r); val x1 = r.unboxed; val x1b = r.boxed
            eval0(arg1, r); val x2 = r.unboxed; val x2b = r.boxed
            eval0(arg2, r); val x3 = r.unboxed; val x3b = r.boxed
            fn(x1,x1b,x2,x2b,x3,x3b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity0(decompile) { def apply(r: R) = {
            eval0(arg0, r); val x1 = r.unboxed; val x1b = r.boxed
            eval0(arg1, r); val x2 = r.unboxed; val x2b = r.boxed
            eval0(arg2, r); val x3 = r.unboxed; val x3b = r.boxed
            eval0(arg3, r); val x4 = r.unboxed; val x4b = r.boxed
            fn(x1,x1b,x2,x2b,x3,x3b,x4,x4b,r)
          }}
        case n =>
          new Arity0(decompile) { def apply(r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(i)
              eval0(args(i), r)
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
          new Arity1(decompile) { def apply(a1: D, a1b: Rt, r: R) = {
            eval1(arg0,a1,a1b,r)
            fn(r.unboxed, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity1(decompile) { def apply(a1: D, a1b: Rt, r: R) = {
            eval1(arg0,a1,a1b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval1(arg1,a1,a1b,r); val x2 = r.unboxed; val x2b = r.boxed
            fn(x1,x1b,x2,x2b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity1(decompile) { def apply(a1: D, a1b: Rt, r: R) = {
            eval1(arg0,a1,a1b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval1(arg1,a1,a1b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval1(arg2,a1,a1b,r); val x3 = r.unboxed; val x3b = r.boxed
            fn(x1,x1b,x2,x2b,x3,x3b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity1(decompile) { def apply(a1: D, a1b: Rt, r: R) = {
            eval1(arg0,a1,a1b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval1(arg1,a1,a1b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval1(arg2,a1,a1b,r); val x3 = r.unboxed; val x3b = r.boxed
            eval1(arg3,a1,a1b,r); val x4 = r.unboxed; val x4b = r.boxed
            fn(x1,x1b,x2,x2b,x3,x3b,x4,x4b,r)
          }}
        case n =>
          new Arity1(decompile) { def apply(a1: D, a1b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(i)
              eval1(args(i), a1, a1b, r)
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
          new Arity2(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            eval2(arg0,a1,a1b,a2,a2b,r)
            fn(r.unboxed, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity2(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            eval2(arg0,a1,a1b,a2,a2b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval2(arg1,a1,a1b,a2,a2b,r); val x2 = r.unboxed; val x2b = r.boxed
            fn(x1,x1b,x2,x2b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity2(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            eval2(arg0,a1,a1b,a2,a2b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval2(arg1,a1,a1b,a2,a2b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval2(arg2,a1,a1b,a2,a2b,r); val x3 = r.unboxed; val x3b = r.boxed
            fn(x1,x1b,x2,x2b,x3,x3b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity2(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            eval2(arg0,a1,a1b,a2,a2b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval2(arg1,a1,a1b,a2,a2b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval2(arg2,a1,a1b,a2,a2b,r); val x3 = r.unboxed; val x3b = r.boxed
            eval2(arg3,a1,a1b,a2,a2b,r); val x4 = r.unboxed; val x4b = r.boxed
            fn(x1,x1b,x2,x2b,x3,x3b,x4,x4b,r)
          }}
        case n =>
          new Arity2(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(i)
              eval2(args(i), a1, a1b, a2, a2b, r)
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
          new Arity3(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            eval3(arg0,a1,a1b,a2,a2b,a3,a3b,r)
            fn(r.unboxed, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity3(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            eval3(arg0,a1,a1b,a2,a2b,a3,a3b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval3(arg1,a1,a1b,a2,a2b,a3,a3b,r); val x2 = r.unboxed; val x2b = r.boxed
            fn(x1,x1b,x2,x2b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity3(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            eval3(arg0,a1,a1b,a2,a2b,a3,a3b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval3(arg1,a1,a1b,a2,a2b,a3,a3b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval3(arg2,a1,a1b,a2,a2b,a3,a3b,r); val x3 = r.unboxed; val x3b = r.boxed
            fn(x1,x1b,x2,x2b,x3,x3b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity3(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            eval3(arg0,a1,a1b,a2,a2b,a3,a3b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval3(arg1,a1,a1b,a2,a2b,a3,a3b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval3(arg2,a1,a1b,a2,a2b,a3,a3b,r); val x3 = r.unboxed; val x3b = r.boxed
            eval3(arg3,a1,a1b,a2,a2b,a3,a3b,r); val x4 = r.unboxed; val x4b = r.boxed
            fn(x1,x1b,x2,x2b,x3,x3b,x4,x4b,r)
          }}
        case n =>
          new Arity3(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(i)
              eval3(args(i), a1, a1b, a2, a2b, a3, a3b, r)
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
          new Arity4(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            eval4(arg0,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r)
            fn(r.unboxed, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity4(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            eval4(arg0,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval4(arg1,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x2 = r.unboxed; val x2b = r.boxed
            fn(x1,x1b,x2,x2b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity4(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            eval4(arg0,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval4(arg1,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval4(arg2,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x3 = r.unboxed; val x3b = r.boxed
            fn(x1,x1b,x2,x2b,x3,x3b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity4(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            eval4(arg0,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x1 = r.unboxed; val x1b = r.boxed
            eval4(arg1,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x2 = r.unboxed; val x2b = r.boxed
            eval4(arg2,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x3 = r.unboxed; val x3b = r.boxed
            eval4(arg3,a1,a1b,a2,a2b,a3,a3b,a4,a4b,r); val x4 = r.unboxed; val x4b = r.boxed
            fn(x1,x1b,x2,x2b,x3,x3b,x4,x4b,r)
          }}
        case n =>
          new Arity4(decompile) { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(i)
              eval4(args(i), a1, a1b, a2, a2b, a3, a3b, a4, a4b, r)
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
          new ArityN(n, decompile) { def apply(args: Array[Slot], r: R) = {
            evalN(arg0, args, r)
            fn(r.unboxed, r.boxed, r)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new ArityN(n, decompile) { def apply(args: Array[Slot], r: R) = {
            evalN(arg0, args, r); val x1 = r.unboxed; val x1b = r.boxed
            evalN(arg1, args, r); val x2 = r.unboxed; val x2b = r.boxed
            fn(x1,x1b,x2,x2b,r)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new ArityN(n, decompile) { def apply(args: Array[Slot], r: R) = {
            evalN(arg0, args, r); val x1 = r.unboxed; val x1b = r.boxed
            evalN(arg1, args, r); val x2 = r.unboxed; val x2b = r.boxed
            evalN(arg2, args, r); val x3 = r.unboxed; val x3b = r.boxed
            fn(x1,x1b,x2,x2b,x3,x3b,r)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new ArityN(n, decompile) { def apply(args: Array[Slot], r: R) = {
            evalN(arg0, args, r); val x1 = r.unboxed; val x1b = r.boxed
            evalN(arg1, args, r); val x2 = r.unboxed; val x2b = r.boxed
            evalN(arg2, args, r); val x3 = r.unboxed; val x3b = r.boxed
            evalN(arg3, args, r); val x4 = r.unboxed; val x4b = r.boxed
            fn(x1,x1b,x2,x2b,x3,x3b,x4,x4b,r)
          }}
        case m =>
          new ArityN(n, decompile) { def apply(args0: Array[Slot], r: R) = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(i)
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

