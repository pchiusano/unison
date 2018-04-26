package org.unisonweb

import org.unisonweb.Builtins.termFor
import org.unisonweb.Pattern._
import org.unisonweb.Term.Syntax._
import org.unisonweb.Term._
import org.unisonweb.compilation._
import org.unisonweb.util.PrettyPrint

object CompilationTests {
  import EasyTest._
  import Terms._

  val env = Environment(
    Builtins.builtins, _ => ???,
    BuiltinTypes.dataConstructors,
    BuiltinTypes.effects)

  def eval(t: Term): Term =
    normalize(env)(t)

  val tests = suite("compilation")(
    test("zero") { implicit T =>
      equal(eval(zero), zero)
    },
    test("one") { implicit T =>
      equal(eval(one), one)
    },
    test("id") { implicit T =>
      equal(eval(id(one)), one)
    },
    test("const") { implicit T =>
      equal(eval(const(one, 2)), one)
    },
    test("1 + 1 = 2") { implicit T =>
      equal(eval(onePlusOne), 2:Term)
    },
    test("1 + 2 = 3") { implicit T =>
      equal(eval((1:Term) + (2:Term)), 3:Term)
    },
    test("sum4(1,2,3,4)") { implicit T =>
      equal(eval(sum4(1,10,100,1000)), (1+10+100+1000):Term)
    },
    test("partially apply") { implicit T =>
      equal(eval(const(zero)), Lam('y)(zero))
    },
    test("partially apply builtin") { implicit T =>
      equal(eval(onePlus), onePlus)
      equal(eval(ap(onePlus, one)), eval(onePlusOne))
    },
    test("partially apply triangle") { implicit T =>
      val p: Term =
        Let('tri -> triangle(100))('tri.v(0))
      equal[Term](eval(p), eval(triangle(100,0)))
    },
    test("selfToLetRec") { implicit T =>
      fail("Might be causing the incorrect.")
    },
    test("let") { implicit T =>
      equal(eval(Let('x -> one)(one + 'x)), eval(onePlusOne))
    },
    test("let2") { implicit T =>
      equal(eval(Let('x -> one, 'y -> (2:Term))(
        'x.v + 'y
      )), 3:Term)
    },
    test("let3") { implicit T =>
      equal(eval(Let('x -> one, 'y -> (10:Term), 'z -> (100:Term))(
        'x.v + 'y + 'z
      )), 111:Term)
    },
    test("let7") { implicit T =>
      equal(eval(Let('x1 -> (5:Term), 'x2 -> (2:Term), 'x3 -> (3:Term),
                     'x4 -> (7:Term), 'x5 -> (11:Term), 'x6 -> (13:Term),
                     'x7 -> (17:Term))(
        'x1.v * 'x2 * 'x3 * 'x4 * 'x5 * 'x6 * 'x7
      )), 510510:Term)
    },
    test("dynamic non-tail call with K args") { implicit T =>
      equal(eval(Let('x -> const)('x.v(one, 2) + one)), 2:Term)
    },
    test("dynamic non-tail call with K+n args") { implicit T =>
      equal(eval(Let('x -> sum4)('x.v(one, 2, 3, 4) + one)), 11:Term)
    },
    test("if") { implicit T =>
      equal1(eval(If(one, one, zero + one + one)), one)
      equal1(eval(If(one - one, one, zero + one + one)), 2:Term)
      equal1(eval(If(one > zero, one, zero + one + one)), one)
      equal1(eval(If(one < zero, one, zero + one + one)), 2:Term)
      ok
    },
    test("fib") { implicit T =>
      0 to 20 foreach { n =>
        equal1(eval(fib(n:Term)), scalaFib(n):Term)
      }
      ok
    },
    test("fib-pretty-print") { implicit T =>
      note("pretty-printed fib implementation", includeAlways = true)
      note(PrettyPrint.prettyTerm(fib).render(40), includeAlways = true)
      note("pretty-printed fib implementation in ANF", includeAlways = true)
      note(PrettyPrint.prettyTerm(Term.ANF(fib)).render(40), includeAlways = true)
      ok
    },
    test("fib-ANF") { implicit T =>
      val fibANF = Term.ANF(fib)
      0 to 20 foreach { n =>
        equal1(eval(fibANF(n:Term)), scalaFib(n):Term)
      }
      ok
    },
    test("==") { implicit T =>
      equal1(eval(one unisonEquals one), eval(true))
      equal1(eval(one unisonEquals onePlusOne), eval(false))
      ok
    },
    test("triangle") { implicit T =>
      10 to 10 foreach { n =>
        equal1(eval(triangle(n:Term, zero)), (0 to n).sum:Term)
      }
      ok
    },
    test("triangle4arg") { implicit T =>
      10 to 50 foreach { n =>
        equal1(eval(triangle4arg(n:Term, zero, zero, zero)), (0 to n).sum:Term)
      }
      ok
    },
    test("evenOdd") { implicit T =>
      0 to 50 foreach { n =>
        equal1(eval(odd(n:Term)), n % 2 :Term)
      }
      ok
    },
    test("nested invokeDynamic") { implicit T =>
      val nestedInvokeDynamic =
        Let(
          'id0 -> id(id),
          'id1 -> 'id0.v('id0),
          'id2 -> 'id1.v('id1)
        ) {
          'id2.v(10)
        }
      equal(eval(nestedInvokeDynamic), 10:Term)
    },
    test("countDown") { implicit T =>
      val p = LetRec(
        'countDown ->
          Lam('n)(If('n.v, 'countDown.v('n.v-1), 42))
      )('countDown.v(10))
      equal[Term](eval(p), 42)
    },
    test("overapply") { implicit T =>
      equal(eval(id(id, id, 10:Term)), 10:Term)
    },
    test("shadow") { implicit T =>
      equal(eval(LetRec('fib -> Lam('fib)('fib.v + 1))('fib.v(41))), 42:Term)
    },
    test("shadow2") { implicit T =>
      val fib = LetRec('fib -> Lam('fib)('fib.v + 1))('fib.v(41))
      val fibNested = LetRec('fib -> (1: Term))(fib)
      equal(eval(fibNested), 42:Term)
    },
    test("let rec example 1") { implicit T =>
      val ex = LetRec(
        'a -> 1,
        'b -> 10,
        'x -> 100,
        'y -> 1000
      )('a.v + 'b + 'x + 'y)
      equal(eval(ex), 1111: Term)
    },
    test("let rec example 2") { implicit T =>
      val ex = LetRec(
        'a -> 1,
        'b -> 10,
        'x -> 100,
        'y -> 1000
      )('a.v + 'x + 'y)
      equal(eval(ex), 1101: Term)
    },
    test("let rec example 3") { implicit T =>
      val ex = LetRec(
        'a -> 1,
        'b -> 10,
        'x -> 100,
        'y -> 1000
      )('b.v + 'x + 'y)
      equal(eval(ex), 1110: Term)
    },
    test("let rec example 4") { implicit T =>
      val ex = LetRec(
        'a -> 1,
        'b -> 10,
        'x -> 100,
        'y -> 1000
      )('x.v + 'y)
      equal(eval(ex), 1100: Term)
    },
    {
      def ex(t: Term) =
        Let('a -> 1, 'b -> 10)(LetRec(
          'x -> 100, 'y -> 1000
        )(t))

      suite("let/letrec")(
        test("abxy") { implicit T =>
          equal(eval(ex('a.v + 'b + 'x + 'y)), 1111: Term)
        },
        test("axy") { implicit T =>
          equal(eval(ex('a.v + 'x + 'y)), 1101: Term)
        },
        test("bxy") { implicit T =>
          equal(eval(ex('b.v + 'x + 'y)), 1110: Term)
        },
        test("xy") { implicit T =>
          equal(eval(ex('x.v + 'y)), 1100: Term)
        }
      )
    },
    test("mutual non-tail recursion") { implicit T =>
      0 to 20 foreach { n =>
        equal1(eval(fibPrime(n:Term)), scalaFib(n):Term)
      }
      ok
    },
    suite("sequence")(
      test("take") { implicit T =>
        1 to 20 foreach { n =>
          val xs = replicate(intIn(0,n))(int).map(x => x: Term)
          val mySeq = Sequence(xs:_*)
          val theirSeq = Sequence(xs.take(n):_*)
          equal1(eval(Sequence.take(n, mySeq)), eval(theirSeq))
        }
        ok
      },
      test("ex1") { implicit T =>
        equal(eval(Sequence.size(Sequence(1,2,3))), 3: Term)
      },
      test("ex2 (underapplication)") { implicit T =>
        val t: Term =
          Let('x -> Sequence(1,2,3),
              'fn -> Sequence.take(2))(Sequence.size('fn.v('x)))
        equal(eval(t), 2: Term)
      }
    ),
    suite("text") (
      test("examples") { implicit T =>
        equal1(eval(Text.concatenate("abc", "123")), "abc123": Term)
        equal1(eval(Text.concatenate(Text.empty, "123")), "123": Term)
        equal1(eval(Text.concatenate("123", Text.empty)), "123": Term)
        equal1(eval(Text.drop(3, "abc123")), "123": Term)
        equal1(eval(Text.take(3, "abc123")), "abc": Term)
        equal1(eval(Text.equal("abc", "abc")), true: Term)
        equal1(eval(Text.lt("Alice", "Bob")), true: Term)
        equal1(eval(Text.lteq("Runar", "Runarorama")), true: Term)
        equal1(eval(Text.lt("Arya", "Arya-orama")), true: Term)
        equal1(eval(Text.gt("Bob", "Alice")), true: Term)
        equal1(eval(Text.gteq("Runarorama", "Runar")), true: Term)
        equal1(eval(Text.gteq("Arya-orama", "Arya")), true: Term)
        ok
      }
    ),
    suite("pattern")(
      test("literal") { implicit T =>
        /* let x = 42; case 10 of 10 -> x + 1 */
        val v: Term = 43
        val c = MatchCase(LiteralU(10, UnboxedType.Integer), 'x.v + 1)
        val p = Let('x -> (42:Term))(Match(10)(c))
        equal(eval(p), v)
      },
      test("pattern guard") { implicit T =>
        /* let x = 42
           case 10 of
             10 | true -> x + 1
        */
        val v: Term = 43
        val c = MatchCase(LiteralU(10, UnboxedType.Integer),
                          Some(true:Term), 'x.v + 1)
        val p = Let('x -> (42:Term))(Match(10)(c))
        equal(eval(p), v)
      },
      test("wildcard") { implicit T =>
        /* let x = 42; case 10 of
           10 | false -> x + 1;
           y -> y + 4
          should be 14
        */
        val v: Term = 14
        val c1 = MatchCase(LiteralU(10, UnboxedType.Integer),
                           Some(false:Term), 'x.v + 1)
        val c2 = MatchCase(Wildcard, ABT.Abs('y, 'y.v + 4))
        val p = Let('x -> (42:Term))(Match(10)(c1, c2))
        equal(eval(p), v)
      },
      test("wildcard0") { implicit T =>
        /* case 10 of y -> y + 4 */
        val v: Term = 14
        val c = MatchCase(Wildcard, ABT.Abs('y, 'y.v + 4))
        val p = Match(10)(c)
        equal(eval(p), v)
      },
      test("uncaptured") { implicit T =>
        /* let x = 42; case 10 of 10 | false -> x + 1; _ -> x + 2
           should return 44
        */
        val v: Term = 44
        val c1 = MatchCase(LiteralU(10, UnboxedType.Integer),
                           Some(false:Term), 'x.v + 1)
        val c2 = MatchCase(Uncaptured, 'x.v + 2)
        val p = Let('x -> (42:Term))(Match(10)(c1, c2))
        equal(eval(p), v)
      },
      test("shadowing") { implicit T =>
        /* let x = 42; case 10 of 10 | false -> x+1; x -> x+4 */
        val v: Term = 14
        val c1 = MatchCase(LiteralU(10, UnboxedType.Integer),
                           Some(false:Term), 'x.v + 1)
        val c2 = MatchCase(Wildcard, ABT.Abs('x, 'x.v + 4))
        val p = Let('x -> (42:Term))(Match(10)(c1, c2))
        equal(eval(p), v)
      },
      test("data pattern") { implicit T =>
        /* let x = 42; case (2,4) of (x,y) -> x+y; x -> x + 4 */
        val v: Term = 6
        val c1 = MatchCase(Pattern.Tuple(Wildcard, Wildcard),
                           ABT.Abs('x, ABT.Abs('y, 'x.v + 'y.v)))
        val c2 = MatchCase(Wildcard, ABT.Abs('x, 'x.v + 4))
        val p = Let('x -> (42:Term))(Match(intTupleTerm(2, 4))(c1, c2))
        equal(eval(p), v)
      },
      test("big non-nested data pattern") { implicit T =>
        /* let x = 42; case (1,10,100) of (a,b,c) -> a+b+c */
        val v: Term = 111
        val c1 = MatchCase(
          Pattern.Tuple(Wildcard, Wildcard, Wildcard),
          ABT.AbsChain('a, 'b, 'c)('a.v + 'b + 'c))
        val p = Let('x -> (42:Term))(Match(intTupleTerm(1, 10, 100))(c1))
        equal(eval(p), v)
      },
      test("bigger non-nested data pattern") { implicit T =>
        /* let x = 42; case (1,10,100,1000) of (a,b,c,d) -> a+b+c+d */
        val v: Term = 1111
        val c1 = MatchCase(
          Pattern.Tuple(Wildcard, Wildcard, Wildcard, Wildcard),
          ABT.AbsChain('a, 'b, 'c, 'd)('a.v + 'b + 'c + 'd))
        val p = Let('x -> (42:Term))(Match(intTupleTerm(1, 10, 100, 1000))(c1))
        equal(eval(p), v)
      },
      test("nested data patterns") { implicit T =>
        /* let x = 42; case ((3,4),(5,6)) of ((x,y),(_,z)) -> x+y+z; x -> x + 4 */
        val v: Term = 13
        val c1 =
          MatchCase(Pattern.Tuple(
            Pattern.Tuple(Wildcard, Wildcard),
            Pattern.Tuple(Uncaptured, Wildcard)),
                    ABT.AbsChain('x, 'y, 'z)('x.v + 'y + 'z))
        val c2 = MatchCase(Wildcard, ABT.Abs('x, 'x.v + 4))
        val p = Let('x -> (42:Term))(
          Match(Terms.tupleTerm(intTupleV(3, 4), intTupleV(5, 6)))(c1, c2))
        equal(eval(p), v)
      },
      test("fall through data pattern") { implicit T =>
        /* let x = 42; case (2,4) of (x,y) -> x+y; x -> x + 4 */
        val v: Term = 6
        val c1 = MatchCase(Pattern.Left(Wildcard), ABT.Abs('x, 'x))
        val c2 = MatchCase(Pattern.Right(Wildcard), ABT.Abs('x, 'x))
        val p = Match(intRightTerm(6))(c1, c2)
        equal(eval(p), v)
      },
      test("fall through data pattern 2") { implicit T =>
        /* let x = 42; case (2,4) of (x,y) -> x+y; x -> x + 4 */
        val v: Term = 8
        val c1 = MatchCase(Pattern.Left(Wildcard), ABT.Abs('x, 'x.v + 1))
        val c2 = MatchCase(Pattern.Right(Wildcard), ABT.Abs('x, 'x.v + 2))
        val p = Match(intRightTerm(6))(c1, c2)
        equal(eval(p), v)
      },
      test("patterns that read the stack array") { implicit T =>
        /* let x = 42; case (2,4) of (x,y) -> x+y; x -> x + 4 */
        val c1 = MatchCase(Pattern.Left(Wildcard), ABT.Abs('x, 'x.v + 'a))
        val c2 = MatchCase(Pattern.Right(Wildcard), ABT.Abs('x, 'x.v + 'a))
        val p =
          Let('a -> 1, 'b -> 10)(Match(intRightTerm(6))(c1, c2))
        equal[Term](eval(p), 7)
      },
      test("as pattern") { implicit T =>
        /* case 3 of x@(y) -> x + y */
        val v: Term = 6
        val c =
          MatchCase(Pattern.As(Pattern.Wildcard),
                    ABT.AbsChain('x, 'y)('x.v + 'y))
        val p = Match(3)(c)
        equal(eval(p), v)
      },
      test("as-as-literal") { implicit T =>
        /* case 3 of x@(y@(3)) -> x + y */
        val v: Term = 6
        val c =
          MatchCase(Pattern.As(Pattern.As(Pattern.LiteralU(3, UnboxedType.Integer))),
                    ABT.AbsChain('x, 'y)('x.v + 'y))
        val p = Match(3)(c)
        equal(eval(p), v)
      },
      test("as-guard-literal 1") { implicit T =>
        /*  case 3 of
              x@(y@(3)) | x + y > 4 -> x + y
         */
        val v: Term = 6
        val c =
          MatchCase(
            Pattern.As(
              Pattern.As(
                Pattern.LiteralU(3, UnboxedType.Integer)
              )), Some[Term](ABT.AbsChain('x, 'y)('x.v + 'y > 4)), ABT.AbsChain('x, 'y)('x.v + 'y))
        val p = Match(3)(c)
        equal(eval(p), v)
      },
      test("as-guard-literal 2") { implicit T =>
        /*  case 1 of
              x@(y@(3)) | x + y > 4 -> x + y
              _ -> 2
         */
        val v: Term = 2
        val c =
          MatchCase(
            Pattern.As(
              Pattern.As(
                Pattern.LiteralU(3, UnboxedType.Integer)
              )), Some[Term](ABT.AbsChain('x, 'y)('x.v + 'y > 4)), ABT.AbsChain('x, 'y)('x.v + 'y))
        val c2 = MatchCase[Term](Pattern.Uncaptured, 2)
        val p = Match(1)(c, c2)
        equal(eval(p), v)
      },
    ),
    test("nested applies") { implicit T =>
      val p = Apply(Apply(triangle, 10), 0)
      equal(eval(p), eval(triangle(10, 0)))
    },

    test("partially applied dynamic call") { implicit T =>
      val p = Let('f -> Lam('g)('g.v(1)),
                  'g -> Lam('a, 'b)('a.v + 'b))('f.v('g)(2))
      equal[Term](eval(p), 3)
    },

    test("fully applied self non-tail call with K args") { implicit T =>
      val fib2: Term =
        LetRec('fib ->
                 Lam('n, 'm)(If('n.v < 'm,
                                'n,
                                'fib.v('n.v - 1, 'm) + 'fib.v('n.v - 2, 'm)))
              )('fib)
      equal[Term](eval(fib2(10, 2)), scalaFib(10))
    },

    test("fully applied self non-tail call with K+1 args") { implicit T =>
      val fib2: Term =
        LetRec('fib ->
                 Lam('n, 'm, 'o)(If('n.v < 'm,
                                 If('n.v < 'o, 'o, 'n),
                                'fib.v('n.v - 1, 'm, 'o) + 'fib.v('n.v - 2, 'm, 'o)))
              )('fib)
      equal[Term](eval(fib2(10, 2, -1)), scalaFib(10))
    },

    test("let within body of tailrec function") { implicit T =>
      val ant: Term = Term.ANF(triangle)
      equal[Term](eval(triangle(10, 0)), (1 to 10).sum)
    },

    test("letrec within body of tailrec function") { implicit T =>
      val trianglePrime =
        LetRec('triangle ->
                 Lam('n, 'acc)(
                     If('n.v > 0,
                       LetRec('n2 -> ('n.v - 1), 'acc2 -> ('acc.v + 'n))(
                              'triangle.v('n2, 'acc2)),
                        'acc.v))
                     )('triangle)
      equal[Term](eval(trianglePrime(10, 0)), (1 to 10).sum)
    },

    test("lambda with non-recursive free variables") { implicit T =>
      equal(eval(Let('x -> 1, 'inc -> Lam('y)('x.v + 'y))('inc.v(one))), 2:Term)
    },

    suite("stream")(
      test("decompile-empty") { implicit T =>
        equal[Term](eval(termFor(Builtins.Stream_empty)),
                    termFor(Builtins.Stream_empty))
      },
      test("decompile-cons") { implicit T =>
        equal[Term](eval(termFor(Builtins.Stream_cons)(1, termFor(Builtins.Stream_empty))),
                    termFor(Builtins.Stream_cons)(1, termFor(Builtins.Stream_empty)))
      },
    ),
    { import BuiltinTypes._
      import Effects._

    suite("algebraic-effects")(
      test("ex1") { implicit T =>
        /*
          let
            state : s -> <State s> a -> a
            state s <a> = a
            state s <get -> k> = handle (state s) (k s)
            state _ <put s -> k> = handle (state s) (k ())

            handle (state 3)
              x = State.get + 1
              y = State.set (x + 1)
              State.get + 11
         */

        val p = LetRec(
          ('state, Lam('s, 'action) {
            Match('action)(
              // state s <a> = a
              MatchCase(Pattern.EffectPure(Pattern.Wildcard),
                        ABT.Abs('a, 'a)),

              // state s <get -> k> = handle (state s) (k s)
              MatchCase(State.Get.pattern(Pattern.Wildcard),
                        ABT.Abs('k, Handle('state.v('s))('k.v('s)))),

              // state _ <put s -> k> = handle (state s) (k ())
              MatchCase(State.Set.pattern(Pattern.Wildcard, Pattern.Wildcard),
                        ABT.AbsChain('s2, 'k)(Handle('state.v('s2))('k.v(BuiltinTypes.Unit.term))))
            )
          })
        ) {
          Handle('state.v(3)) {
            Let(
              ('x, State.Get.term + 1),
              ('y, State.Set.term('x.v + 1))
            )(State.Get.term + 11)
          }
        }
        note("pretty-printed algebraic effects program", includeAlways = true)
        note(PrettyPrint.prettyTerm(Term.ANF(p)).render(40), includeAlways = true)
        equal[Term](eval(Term.ANF(p)), 16)
      },
      test("nested effects handlers") { implicit T => fail("too big file") },
      test("simple effectful handlers") { implicit T =>
        /*
          let
            state : s -> {State Integer} a -> a
            state s {a} = a
            state s {get -> k} = handle (state s) (k s)
            state _ {put s -> k} = handle (state s) (k ())

            state' : s -> {State Integer} Integer -> {State Integer} Integer
            state' s {a} = State.get * s
            state' s {get -> k} = handle (state' s) (k s)
            state' _ {put s -> k} = handle (state' s) (k ())

            handle (state 10)
              handle (state' 3)
                2
        */

        val p = LetRec(
          ('state, /*Term.curry*/locally { Lam('s0, 'x, 'y, 'action0) {
            Match('action0)(
              MatchCase(Pattern.EffectPure(Pattern.Wildcard), ABT.Abs('a, 'a)),
              MatchCase(State.Get.pattern(Pattern.Wildcard),
                        ABT.Abs('k, Handle('state.v('s0,'x,'y))('k.v('s0)))),
              MatchCase(State.Set.pattern(Pattern.Wildcard, Pattern.Wildcard),
                        ABT.AbsChain('s2, 'k)(Handle('state.v('s2,'x,'y))('k.v(BuiltinTypes.Unit.term))))
            )
          }}),
          ('state2, /*Term.curry*/ locally { Lam('s1, 'x, 'action1) {
            Match('action1)(
              // state s {a} = State.get * s
              MatchCase(Pattern.EffectPure(Pattern.Wildcard),
                        ABT.Abs('a, State.Get.term * 's1)),
                        // ABT.Abs('a, 's1)), <-- this works fine!
              // state' s {get -> k} = handle (state' s) (k s)
              MatchCase(State.Get.pattern(Pattern.Wildcard),
                        ABT.Abs('k, Handle('state2.v('s1, 'x))('k.v('s1.v)))),
              // state' _ {put s -> k} = handle (state' s) (k ())
              MatchCase(State.Set.pattern(Pattern.Wildcard, Pattern.Wildcard),
                        ABT.AbsChain('s3, 'k)(
                          Handle('state2.v('s3, 'x))('k.v(BuiltinTypes.Unit.term))))
            )
          }})) {
            Handle('state.v(10, 3030, 9090))(Handle('state2.v(3, 2929))(2340983))
          }

        note(PrettyPrint.prettyTerm(p).render(80), includeAlways = true)
        note(PrettyPrint.prettyTerm(Term.ANF(p)).render(80), includeAlways = true)
        equal[Term](eval(Term.ANF(p)), 30)
      },
      test("effectful handlers") { implicit T =>
        /*
          let
            state : s -> {State Integer} a -> a
            state s {a} = a
            state s {get -> k} = handle (state s) (k s)
            state _ {put s -> k} = handle (state s) (k ())

            state' : s -> {State Integer} Integer -> {State Integer} Integer
            state' s {a} = State.get * s
            state' s {get -> k} = let
              outer-value = State.get
              handle (state s) (k (s + outer-value))
            state' _ {put s -> k} = handle (state s) (k ())

            handle (state 10)
              handle (state' 3)
                -- x is 14
                x = State.get + 1
                -- Inner state is 15
                y = State.set (x + 1)
                -- Should be 360
                State.get + 11
        */

        val p = LetRec(
          ('state, Term.curry { Lam('s, 'action) {
            Match('action)(
              // state s <a> = a
              MatchCase(Pattern.EffectPure(Pattern.Wildcard),
                        ABT.Abs('a, 'a)),

              // state s <get -> k> = handle (state s) (k s)
              MatchCase(State.Get.pattern(Pattern.Wildcard),
                        ABT.Abs('k, Handle('state.v('s))('k.v('s)))),

              // state _ <put s -> k> = handle (state s) (k ())
              MatchCase(State.Set.pattern(Pattern.Wildcard, Pattern.Wildcard),
                        ABT.AbsChain('s2, 'k)(Handle('state.v('s2))('k.v(BuiltinTypes.Unit.term))))
            )
          }}),
          ('state2, Term.curry { Lam('s, 'action) {
            Match('action)(
              // state s <a> = State.get * s
              MatchCase(Pattern.EffectPure(Pattern.Wildcard),
                        ABT.Abs('a, State.Get.term * 's)),

              /*
              let
                outer-value = State.get
                handle (state s) (k (s + outer-value))
              */
              MatchCase(State.Get.pattern(Pattern.Wildcard),
                        ABT.Abs('k, Let('outer -> State.Get.term)(
                          Handle('state2.v('s))('k.v('s.v + 'outer))))),

              // state _ <put s -> k> = handle (state s) (k ())
              MatchCase(State.Set.pattern(Pattern.Wildcard, Pattern.Wildcard),
                        ABT.AbsChain('s2, 'k)(
                          Handle('state2.v('s2))('k.v(BuiltinTypes.Unit.term))))
            )
          }}))(

          /* handle (state 10)
               handle (state' 3)
                 x = State.get + 1
                 y = State.set (x + 1)
                 State.get + 11 */
          Handle('state.v(10))(Handle('state2.v(3))(
            Let('x -> (State.Get.term + 1),
                'y -> State.Set.term('x.v + 1))(State.Get.term + 11))))

      note(PrettyPrint.prettyTerm(Term.ANF(p)).render(40), includeAlways = true)
      equal[Term](eval(Term.ANF(p)), 360)
    })}
  )
}

object Terms {
  val zero: Term = U0
  val one: Term = 1

  val id: Term = Lam('x)('x)

  val const: Term = Lam('x,'y)('x)

  val onePlusOne: Term = one + one

  val onePlus: Term = Builtins.termFor(Builtins.Integer_add)(one)

  val ap: Term = Lam('f,'x)('f.v('x))

  val sum4: Term = Lam('a,'b,'c,'d)('a.v + 'b + 'c + 'd)

  val fib: Term =
    LetRec('fib ->
             Lam('n)(If('n.v < 2, 'n, 'fib.v('n.v - 1) + 'fib.v('n.v - 2))))('fib)

  val fibPrime: Term =
    LetRec(
      'fib -> Lam('n)(If('n.v < 2, 'n, 'fib2.v('n.v - 1) + 'fib2.v('n.v - 2))),
      'fib2 -> Lam('n)(If('n.v < 2, 'n, 'fib.v('n.v - 1) + 'fib.v('n.v - 2)))
    )('fib)

  def scalaFib(n: Int): Int =
    if (n < 2) n else scalaFib(n - 1) + scalaFib(n - 2)

  val triangle =
    LetRec('triangle ->
             Lam('n, 'acc)(
               If('n.v > 0,
                  'triangle.v('n.v - 1, 'acc.v + 'n),
                  'acc.v))
    )('triangle)

  val triangle4arg =
    LetRec('triangle ->
             Lam('n, 'hahaha, 'hehehe, 'acc)(
               If('n.v > 0,
                  'triangle.v('n.v - 1, 'hahaha, 'hehehe, 'acc.v + 'n),
                  'acc.v))
    )('triangle)

  val odd =
    LetRec(
      'even -> Lam('n)(If('n.v > zero, 'odd.v ('n.v - 1), one)),
      'odd-> Lam('n)(If('n.v > zero, 'even.v ('n.v - 1), zero))
    )('odd)

  def tupleTerm(xs: Value*): Term =
    Term.Compiled(tupleV(xs :_*))

  def tupleV(xs: Value*): Value =
    Value.Data(Id.Builtin("Tuple"), ConstructorId(0), xs.toArray)

  def intTupleTerm(xs: Int*): Term =
    Term.Compiled(intTupleV(xs: _*))

  def intRightTerm(i: Int): Term =
    Term.Compiled(intRightV(i))

  def intRightV(i: Int): Value =
    Value.Data(Id.Builtin("Either"), ConstructorId(1), Array(intValue(i)))
  def intTupleV(xs: Int*): Value =
    tupleV(xs.map(intValue): _*)

  def intValue(x: Int): Value = Value.Unboxed(x.toLong, UnboxedType.Integer)

  implicit class Ops(t0: Term) {
    def +(t1: Term) = Builtins.termFor(Builtins.Integer_add)(t0, t1)
    def -(t1: Term) = Builtins.termFor(Builtins.Integer_sub)(t0, t1)
    def *(t1: Term) = Builtins.termFor(Builtins.Integer_mul)(t0, t1)
    def unisonEquals(t1: Term) =
      Builtins.termFor(Builtins.Integer_eq)(t0, t1)
    def <(t1: Term) = Builtins.termFor(Builtins.Integer_lt)(t0, t1)
    def >(t1: Term) = Builtins.termFor(Builtins.Integer_gt)(t0, t1)
  }

  object Sequence {
    import Builtins._

    def apply(terms: Term*): Term =
      Term.Sequence(util.Sequence(terms:_*))

    val empty = termFor(Sequence_empty)
    val cons = termFor(Sequence_cons)
    val snoc = termFor(Sequence_snoc)
    val take = termFor(Sequence_take)
    val size = termFor(Sequence_size)
  }

  object Text {
    import Builtins._

    val empty = termFor(Text_empty)
    val take = termFor(Text_take)
    val drop = termFor(Text_drop)
    val concatenate = termFor(Text_concatenate)
    val size = termFor(Text_size)
    val equal = termFor(Text_eq)
    val lt = termFor(Text_lt)
    val gt = termFor(Text_gt)
    val lteq = termFor(Text_lteq)
    val gteq = termFor(Text_gteq)
  }
}
