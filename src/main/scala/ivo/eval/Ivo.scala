package fretta.eval

import scalaz._
import Scalaz._
import com.typesafe.scalalogging._

object Ivo {
  val logger = Logger("IvoEval")

  // TODO: true backward functions, not just simple patterns
  // TODO: exceptions
  // TODO: imports (new design? -- need stable paths) and exports [need to make some fields hidden in records]
  // TODO: memoization of functions

  type Name = String

  implicit class IsValue(e: Exp) {
    def isValue: Boolean = e match {
      case Closure(_, _, _, _) => true
      case LitVal(_) => true
      case Union(e1, e2) => e1.isValue && e2.isValue
      case Record(tag, xes) => xes.forall { case (x, e) => e.isValue }
      case Thunk(_, _) => true
      case Loc(_) => true
      case AnyVal() => true
      case NilVal() => true
      case Native(_, _, _) => true
      case _ => false
    }

    def isPattern: Boolean = e match {
      case v if v.isValue => true
      case App(e, p) => ! e.isPattern && p.isPattern
      case Union(p1, p2) => p1.isPattern && p2.isPattern
      case Intersect(p1, p2) => p1.isPattern && p2.isPattern
      case Record(tag, xps) => xps.forall { case (x, p) => p.isPattern }
      case Unknown(x) => true
      case ByNameMarker(p) => p.isPattern
      case Select(e, x) => false
      case Any(p) => p.isPattern
      case Single(p) => p.isPattern
      case Random(p) => p.isPattern
      case All(p) => p.isPattern
      case Bind(p, e) => p.isPattern
      case _ => false
    }
  }

  type Value = Exp
  case class Closure(mode: Mode, pat: Pat, e: Exp, env: Env) extends Value
  case class LitVal(value: Int) extends Value
  case class Thunk(e: Exp, env: Env) extends Value
  case class NativeThunk(f: PartialFunction[Value, Value], v: Value) extends Exp
  case class Loc(address: Int) extends Value
  case class AnyVal() extends Value
  case class NilVal() extends Value

  sealed trait Exp
  case class Lambda(mode: Mode, pat: Pat, e: Exp) extends Exp
  case class App(e1: Exp, e2: Exp) extends Exp
  case class Var(x: Name) extends Exp
  case class Self() extends Exp
  case class Union(e1: Exp, e2: Exp) extends Exp
  case class For(p: Pat, e: Exp) extends Exp
  case class Record(tag: Name, xes: List[(Name, Exp)]) extends Exp
  case class Select(container: Exp, x: Name) extends Exp
  case class ModeSelect(mode: Mode, e: Exp) extends Exp
  case class Native(mode: Mode, tag: Name, fun: PartialFunction[Value, Value]) extends Exp
  case class Bind(p: Pat, e: Exp) extends Exp

  // TODO: Wrap in PatMarker any Exp containing an Unknown.
  // Then can use this marker as a tag for dispatch.

  // Pattern values
  type Pat = Exp
  case class ByNameMarker(p: Pat) extends Pat
  case class Unknown(x: Name) extends Pat
  case class Intersect(p1: Pat, p2: Pat) extends Pat

  sealed trait Mode
  case class In() extends Mode
  case class Out() extends Mode

  sealed trait Selector extends Exp
  case class Any(e: Exp) extends Selector
  case class Single(e: Exp) extends Selector
  case class Random(e: Exp) extends Selector
  case class All(e: Exp) extends Selector

  def Binary(op: String, x: Exp, y: Exp) = App(App(Var(op), x), y)
  def BinaryWithMode(m1: Mode, m2: Mode, op: String, x: Exp, y: Exp) = App(ModeSelect(m2, App(ModeSelect(m1, Var(op)), x)), y)
  def Unary(op: String, x: Exp) = App(Var(op), x)

  // Semantics
  // A pattern is a function that returns a stream of environments
  // An environment is just a record
  // Need to work out semantics of envrionments too. self returns what? How do outer environments work?

  // Dispatch: sort alternatives by priority. Find first match, in order.

  val ρ0 = Map(
    // TODO: add non-integer values.

    "(_ + _ = ?)" -> Native(In(), "(_ + _)", { case LitVal(x) => Native(In(), "\\(_ + _)", { case LitVal(y) => LitVal(x+y) } ) } ),
    "(? + _ = _)" -> Native(Out(), "(? + _)", { case LitVal(y) => Native(In(), "\\(? + _)", { case LitVal(z) => LitVal(z-y) } ) } ),
    "(_ + ? = _)" -> Native(In(), "(_ + ?)", { case LitVal(x) => Native(Out(), "\\(_ + ?)", { case LitVal(z) => LitVal(z-x) } ) } ),

    "(_ - _ = ?)" -> Native(In(), "(_ - _)", { case LitVal(x) => Native(In(), "\\(_ - _)", { case LitVal(y) => LitVal(x-y) } ) } ),
    "(? - _ = _)" -> Native(Out(), "(? - _)", { case LitVal(y) => Native(In(), "\\(? - _)", { case LitVal(z) => LitVal(z+y) } ) } ),
    "(_ - ? = _)" -> Native(In(), "(_ - ?)", { case LitVal(x) => Native(Out(), "\\(_ - ?)", { case LitVal(z) => LitVal(x-z) } ) } ),

    "(- _ = ?)" -> Native(In(), "(- _)", { case LitVal(x) => LitVal(-x) } ),
    "(- ? = _)" -> Native(Out(), "(- ?)", { case LitVal(z) => LitVal(-z) } ),

    "(_ * _ = ?)" -> Native(In(), "(_ * _)", { case LitVal(x) => Native(In(), "\\(_ * _)", { case LitVal(y) => LitVal(x*y) } ) } ),
    "(? * _ = _)" -> Native(Out(), "(? * _)", { case LitVal(y) if y != 0 => Native(In(), "\\(? * _)", { case LitVal(z) if (z/y)*y == z => LitVal(z/y) } ) } ),
    "(_ * ? = _)" -> Native(In(), "(_ * ?)", { case LitVal(x) if x != 0 => Native(Out(), "\\(_ * ?)", { case LitVal(z) if x*(z/x) == z => LitVal(z/x) } ) } ),

    "(_ / _ = ?)" -> Native(In(), "(_ / _)", { case LitVal(x) => Native(In(), "\\(_ / _)", { case LitVal(y) if y != 0 => LitVal(x/y) } ) } ),
    "(? / _ = _)" -> Native(Out(), "(? / _)", { case LitVal(y) if y != 0 => Native(In(), "\\(? / _)", { case LitVal(z) if (z*y)/y == z => LitVal(z*y) } ) } ),
    "(_ / ? = _)" -> Native(In(), "(_ / ?)", { case LitVal(x) if x != 0 => Native(Out(), "\\(_ / ?)", { case LitVal(z) if z != 0 && x/(x/z) == z => LitVal(x/z) } ) } ),

    "(_ == _ = ?)" -> Native(In(), "(_ == _)", { case v1 => Native(In(), "\\(_ == _)", { case v2 => if (v1 == v2) True else False } ) } ),
    "(? == _ = _)" -> Native(Out(), "(? == _)", { case v => Native(In(), "\\(? == _)", { case `True` => v } ) } ),
    "(_ == ? = _)" -> Native(In(), "(_ == ?)", { case v => Native(Out(), "\\(_ == ?)", { case `True` => v } ) } )
  )

  val ρ1 = ρ0 ++ Map(
    "(_ + _)" -> Union(ρ0("(_ + _ = ?)"), Union(ρ0("(? + _ = _)"), ρ0("(_ + ? = _)"))),
    "(_ - _)" -> Union(ρ0("(_ - _ = ?)"), Union(ρ0("(? - _ = _)"), ρ0("(_ - ? = _)"))),
    "(_ * _)" -> Union(ρ0("(_ * _ = ?)"), Union(ρ0("(? * _ = _)"), ρ0("(_ * ? = _)"))),
    "(_ / _)" -> Union(ρ0("(_ / _ = ?)"), Union(ρ0("(? / _ = _)"), ρ0("(_ / ? = _)"))),
    "(_ == _)" -> Union(ρ0("(_ == _ = ?)"), Union(ρ0("(? == _ = _)"), ρ0("(_ == ? = _)"))),
    "(- _)" -> Union(ρ0("(- _ = ?)"), ρ0("(- ? = _)"))
  )

  // type Eval[A] = ReaderWriterStateT[Free.Trampoline, MEnv, Unit, MState, ValidationNel[String, A]]
  type Eval[A] = ReaderWriterStateT[Free.Trampoline, MEnv, Unit, MState, A]

  val eval = ReaderWriterStateT.rwstMonad[Free.Trampoline, MEnv, Unit, MState]
  import eval._

  // The environment is stack of frame locations.
  type Env = List[Loc]

  // The store maps locations to values.
  type Store = Map[Loc, Value]

  // `self` is just the environment location.
  // but only exported variables should be returned.
  // so maybe we have a different operation to explicitly export variables and
  // return an indirect record

  // We also store in the environment the location of the current argument.
  // This is used for implementing call-by-value calls. If any alternative is call-by-value
  // we evaluate the argument and store it.
  // Otherwise, we just leave the location `nil`.
  // This implementation avoids re-evaluating the argument.
  // There is a small amount of overhead per call, but we can optimize calls
  // where the function is not a union.

  // Memoization works by looking up the current environment (and key, which is usually the function name)
  // in the memo table (in the store). If found, returns the value stored there.
  // The environment should be pruned to include only the free variables in the memoized expression.

  implicit class EnvOps(ρ: Record) {
    def union(ρ2: Record): Record = {
      val xvs2 = ρ.xes.foldLeft(ρ2.xes.toMap) {
        case (xvs, (x, v)) =>
          xvs.get(x) match {
            case Some(v2) => xvs + (x -> Ivo.union(v, v2))
            case None => xvs + (x -> v)
          }
      }
      Record(ρ.tag, xvs2.toList)
    }

    def intersect(ρ2: Record): Record = {
      val xvs2 = ρ.xes.foldLeft(ρ2.xes.toMap) {
        case (xvs, (x, v)) =>
          xvs.get(x) match {
            case Some(v2) if v == v2 => xvs + (x -> v)
            case _ => xvs - x
          }
      }
      Record(ρ.tag, xvs2.toList)
    }
  }

  val envLoc0 = Loc(0)

  val emptyStore: Store = Map(envLoc0 -> Record("Env", ρ1.toList))

  val emptyMEnv = MEnv(None, envLoc0::Nil)
  val emptyMState = MState(1, emptyStore)

  case class MState(nextLoc: Int, σ: Store)
  case class MEnv(arg: Option[Loc], frames: Env)

  val Empty = Record("Empty", List())
  val True = Record("True", List())
  val False = Record("False", List())

  def union(vs: List[Value]): Value = {
    require(vs.forall(_.isValue))

    vs match {
      case Nil => NilVal()
      case v::Nil => v
      case v::vs => union(v, union(vs))
    }
  }

  def collect(v: Value): List[Value] = {
    require(v.isValue)

    v match {
      case NilVal() => Nil
      case Union(v1, v2) => collect(v1) ++ collect(v2)
      case v if v.isValue => List(v)
    }
  }

  def union(v1: Value, v2: Value) = {
    require(v1.isValue)
    require(v2.isValue)

    collect(Union(v1, v2)).distinct match {
      case Nil => NilVal()
      case v::Nil => v
      case v::vs =>
        vs.foldLeft(v) {
          case (NilVal(), v2) => v2
          case (v1, NilVal()) => v1
          case (v1, v2) if v1 == v2 => v1
          case (v1, v2) => Union(v1, v2)
        }
    }
  }

  def atLeastAsSpecific(p1: Pat, p2: Pat): Boolean = {
    println(s"p1 = $p1")
    println(s"p2 = $p2")
    require(p1.isPattern)
    require(p2.isPattern)

    (p1, p2) match {
      case (ByNameMarker(p1), p2) => atLeastAsSpecific(p1, p2)
      case (p1, ByNameMarker(p2)) => atLeastAsSpecific(p1, p2)

      case (AnyVal(), AnyVal()) => true
      case (AnyVal(), Unknown(_)) => true
      case (Unknown(_), AnyVal()) => true
      case (Unknown(_), Unknown(_)) => true

      case (p1, Unknown(_)) => true
      case (Unknown(_), p2) => false

      case (p1, AnyVal()) => true
      case (AnyVal(), p2) => false

      case (LitVal(v1), LitVal(v2)) => true
      case (LitVal(v1), p2) => true
      case (p1, LitVal(v2)) => false

      case (NilVal(), NilVal()) => true

      case (Record(tag1, xps1), Record(tag2, xps2)) => true
      case (Record(tag1, xps1), p2) => true
      case (p1, Record(tag2, xps2)) => false

      case (App(e1, p1), App(e2, p2)) => true

      case (p, Union(p1, p2)) => atLeastAsSpecific(p, p1) && atLeastAsSpecific(p, p2)
      case (Union(p1, p2), p) => atLeastAsSpecific(p1, p) || atLeastAsSpecific(p2, p)
      case (p, Intersect(p1, p2)) => atLeastAsSpecific(p, p1) || atLeastAsSpecific(p, p2)
      case (Intersect(p1, p2), p) => atLeastAsSpecific(p1, p) && atLeastAsSpecific(p2, p)

      case (p1, p2) => false
    }
  }

  def findMaximalPatterns(pvs: DispatchResult): DispatchResult = pvs match {
    case Nil => Nil
    case (p, v)::pvs =>
      val filtered = findMaximalPatterns(pvs)
      filtered match {
        case Nil => (p, v)::Nil
        case pvs =>
          val f2 = pvs.filter { case (q, _) => atLeastAsSpecific(q, p) }
          if (f2.exists { case (q, _) => atLeastAsSpecific(p, q) })
            (p, v)::f2
          else
            f2
      }
  }

  def select(pvs: DispatchResult): Eval[List[Thunk]] = {
    point(findMaximalPatterns(pvs) map { _._2 })
  }

  def evalCall(fun: Value, arg: Exp): Eval[Value] = {
    require(fun.isValue)
    for {
      ς <- get
      env <- ask
      // allocate a new location for the argument and store it in the MEnv
      _ <- put(MState(ς.nextLoc+1, ς.σ))
      pvs <- local { env1 => MEnv(Some(Loc(ς.nextLoc)), env1.frames) } { dispatch(fun, arg) }
      thunks <- select(pvs)
      r <- evalThunks(thunks)
    } yield r
  }

  def evalThunks(thunks: List[Thunk]): Eval[Value] = {
    logger.debug(s"thunks = $thunks")
    thunks match {
      case Nil => point(NilVal())
      case Thunk(e, env)::Nil =>
        withEnv(env) { eval(e) }
      case Thunk(e, env)::thunks =>
        for {
          v1 <- withEnv(env) { eval(e) }
          v2 <- evalThunks(thunks)
        } yield union(v1, v2)
    }
  }

  def withEnv[A](newEnv: Env)(body: Eval[A]) = local { case MEnv(arg, frames) => MEnv(arg, newEnv) } (body)

  type DispatchResult = List[(Pat, Thunk)]

  def dispatch(fun: Value, arg: Exp): Eval[DispatchResult] = {
    require(fun.isValue)
    logger.debug(s"fun = $fun")
    logger.debug(s"arg = $arg")
    fun match {
      case Native(mode, name, fn) =>
        for {
          ς <- get
          env <- ask
          argv <- env match {
            case MEnv(Some(loc), ρFrames) =>
              ς match {
                case MState(nextLoc, σ) =>
                  σ.get(loc) match {
                    case Some(v) =>
                      logger.debug(s"already evaluated arg $v")
                      point(v)
                    case None =>
                      logger.debug(s"need to evaluate arg $arg")
                      for {
                        argv <- eval(arg)
                        _ <- put(MState(nextLoc, σ + (loc -> argv)))
                      } yield argv
                  }
              }
          }
          _ <- point(logger.debug(s"argv = $argv"))
          o <- point(fn.isDefinedAt(argv))
        } yield {
          logger.debug(s"o = $o")
          o match {
            // FIXME: the pattern should be the literal pattern in the Native
            case true => (NilVal(), Thunk(NativeThunk(fn, argv), envLoc0::Nil))::Nil
            case false => Nil
          }
          // o match {
          //   case env1::Nil => List((p, Thunk(e, env1)))
          //   case _ => Nil
          // }
        }

      case Closure(mode, ByNameMarker(p), e, ρ) =>
        for {
          argRho <- ask map { _.frames }
          o <- withEnv(ρ) { matchPattern(p, Thunk(arg, argRho)) }
        } yield {
          o map {
            ρ1 => (p, Thunk(e, ρ1))
          }
        }

      case Closure(mode, p, e, ρ) =>
        for {
          ς <- get
          env <- ask
          argv <- env match {
            case MEnv(Some(loc), ρFrames) =>
              ς match {
                case MState(nextLoc, σ) =>
                  σ.get(loc) match {
                    case Some(v) =>
                      logger.debug(s"already evaluated arg $v")
                      point(v)
                    case None =>
                      logger.debug(s"need to evaluate arg $arg")
                      for {
                        argv <- eval(arg)
                        _ <- put(MState(nextLoc, σ + (loc -> argv)))
                      } yield argv
                  }
              }
          }
          _ <- point(logger.debug(s"argv = $argv"))
          o <- withEnv (ρ) { matchPattern(p, argv) }
        } yield {
          logger.debug(s"o = $o")
          o map {
            ρ1 => (p, Thunk(e, ρ1))
          }
          // o match {
          //   case env1::Nil => List((p, Thunk(e, env1)))
          //   case _ => Nil
          // }
        }

      case Union(v1, v2) =>
        for {
          thunk1 <- dispatch(v1, arg)
          thunk2 <- dispatch(v2, arg)
        } yield thunk1 ++ thunk2

      case _ =>
        point(Nil)
    }
  }

  def eval(e: Exp): Eval[Value] = e match {
    case v if v.isValue =>
      point(v)

    case NativeThunk(f, v) =>
      point(f(v))

    case ModeSelect(mode, e) =>
      for {
        v <- eval(e)
      } yield {
        val vs = collect(v) collect {
          case v @ Native(cMode, tag, fn) if cMode == mode => v
          case v @ Closure(cMode, p, e, ρ) if cMode == mode => v
        }
        union(vs)
      }

    case Lambda(mode, ByNameMarker(p), e) =>
      for {
        ρ <- ask map { _.frames }
      } yield Closure(mode, ByNameMarker(p), e, ρ)

    case Lambda(mode, p, e) =>
      for {
        ρ <- ask map { _.frames }
      } yield Closure(mode, p, e, ρ)

    case Unknown(x) =>
      point(Native(In(), "env", { case v => Record("env", (x, v)::Nil) }))

    case AnyVal() =>
      point(Native(In(), "env", { case v => Record("env", Nil) }))

    case Var(x) =>
      ask map { _.frames } flatMap {
        case Nil => point(NilVal())
        case loc::frames =>
          get flatMap {
            case MState(_, σ) =>
              σ.get(loc) match {
                case Some(Record(_, xvs)) =>
                  xvs.toMap.get(x) match {
                    case Some(v) => v match {
                      case Thunk(e, env) =>
                        withEnv (env) { eval(e) }
                      case v =>
                        point(v)
                    }
                    case None => withEnv(frames) { eval(Var(x)) }
                  }
                case None =>
                  withEnv(frames) { eval(Var(x)) }
              }
          }
      }

    case Self() =>
      for {
        ρ <- ask map { _.frames }
      } yield {
          ρ match {
            case Nil => NilVal()
            case ρ::ρs => ρ
          }
        }

    case Union(e1, e2) =>
      for {
        v1 <- eval(e1)
        v2 <- eval(e2)
      } yield Union(v1, v2)

    case All(e) =>
      eval(e)

    // Select the first element of the union.
    case Any(e) =>
      for {
        v <- eval(e)
      } yield collect(v) match {
        case v::_ => v
        case Nil => NilVal()
      }

    // Select the unique element of the union
    case Single(e) =>
      for {
        v <- eval(e)
      } yield collect(v) match {
        case v::Nil => v
        case _ => NilVal()
      }

    // Random is the same as any, but selects
    // a random element of the union rather than the first.
    case Random(e) =>
      import scala.util.{Random => R}
      for {
        v <- eval(e)
      } yield R.shuffle(collect(v)) match {
        case v::_ => v
        case _ => NilVal()
      }

    case Select(e, x) =>
      for {
        ℓ <- eval(e)
        ς <- get
        v <- ς match {
          case MState(_, σ) =>
            ℓ match {
              case ℓ: Loc =>
                σ.get(ℓ) match {
                  case Some(Record(tag, xvs)) =>
                    xvs.toMap.get(x) match {
                      case Some(v) => point(v)
                      case _ => point(NilVal())
                    }
                  case _ => point(NilVal())
                }
              case _ => point(NilVal())
            }
        }
      } yield v

    case Record(tag, Nil) =>
      point(Record(tag, Nil))

    case Record(tag, (x, e)::xes) =>
      for {
        v <- eval(e)
        tail <- eval(Record(tag, xes))
      } yield {
        tail match {
          case Record(_, xvs) => Record(tag, (x, v)::xvs)
          case _ => NilVal()
        }
      }

    case App(e1, e2) =>
      logger.debug(s"e1 = $e1")
      logger.debug(s"e2 = $e2")
      for {
        v1 <- eval(e1)
        v2 <- evalCall(v1, e2)
      } yield v2

    case Bind(p, e) if p.isPattern =>
      for {
        v <- eval(e)
        ρs <- matchPattern(p, v)
      } yield True

    case For(p, e) if p.isPattern =>
      for {
        ρs <- matchPattern(p, True)
        _ <- point(println(s"$p = True --> $ρs"))
        vs <- mapM[Env, Value, Eval](ρs) { case ρ => withEnv (ρ) { eval(e) } }
      } yield union(vs)

    case _ =>
      point(NilVal())
  }

  // DISPATCH
  // Syntactic decision whether to treat as pattern or expression and select mode.
  // Add SelectMode(e, In::In::Out::Nil)

  def mapM[α, β, M[_]: Monad](xs: List[α])(f: α => M[β]): M[List[β]] = xs.map(f).sequence

  def matchPattern2(p: Pat, v: Value): Eval[List[Env]] = {
    require(v.isValue)
    // eval(App(ModeSelect(In(), p), v))
    point(Nil)
  }

  // match pattern should just eval the pattern, passing in the value
  // unknown(x) pattern is a function that returns an env with one field x
  // _ pattern is a function that returns an empty env for any argument

  // cons ? ?
  // returns two output values, which get merged in a crossproduct
  // {x1=v1} * {x2=v2} = {x1=v1, x2=v2}

  def matchPattern(p: Pat, v: Value): Eval[List[Env]] = {
    require(v.isValue)

    (p, v) match {
      case (Unknown(x), v) =>
        logger.debug(s"match pattern $p ~ $v")
        for {
          ρs0 <- ask map { _.frames }
          ς <- get
          ρs <- ς match {
            case MState(nextLoc, σ) =>
              val ρ::_ = ρs0
              val ρ1 = Loc(nextLoc)
              σ.get(ρ) match {
                case Some(Record(tag, xvs)) =>
                  for {
                    _ <- put(MState(nextLoc+1, σ + (ρ1 -> Record(tag, (x, v)::xvs))))
                  } yield List(ρ1::Nil)
                case _ =>
                  point(Nil)
              }
          }
        } yield ρs

      case (AnyVal(), v) =>
        logger.debug(s"match pattern $p ~ _")
        for {
          ρs <- ask map { _.frames }
        } yield List(ρs)

      case (LitVal(v1), LitVal(v2)) if v1 == v2 =>
        logger.debug(s"match pattern $p ~ $v")
        for {
          ρs <- ask map { _.frames }
        } yield List(ρs)

      case (App(e1, p2), v) if p2.isPattern =>
        logger.debug(s"match pattern $p ~ $v")
        for {
          f <- eval(e1)
          v1 <- evalCall(f, v)
          envs1 <- matchPattern(p2, v1)
        } yield envs1

      case (Union(p1, p2), v) =>
        logger.debug(s"match pattern $p ~ $v")
        for {
          envs1 <- matchPattern(p1, v)
          envs2 <- matchPattern(p2, v)
        } yield envs1 ++ envs2

      case (Intersect(p1, p2), v) =>
        logger.debug(s"match pattern $p ~ $v")
        for {
          envs1 <- matchPattern(p1, v)
          envs2 <- matchPattern(p2, v)
          ρs <- envs2.foldLeftM(envs1) {
            case (ρs, ρ) => mapM(ρs) { intersect(_, ρ) }
          }
        } yield ρs

      case (Record(tag1, xps), Record(tag2, xvs)) if tag1 == tag2 =>
        logger.debug(s"match pattern $p ~ $v")
        val m = xvs.toMap
        xps.foldLeftM(List[Env](envLoc0::Nil)) {
          case (Nil, _) => point(Nil)
          case (ρs1, (x, p)) =>
            m.get(x) match {
              case None => point(Nil)
              case Some(v) =>
                for {
                  ρs <- matchPattern(p, v)
                } yield {
                  ρs1 ++ ρs
                }
            }
        }

      case (Bind(p, e), `True`) if p.isPattern =>
        println(s"BIND $p = $e")
        for {
          v <- eval(e)
          ρs <- matchPattern(p, v)
        } yield ρs

      case (Bind(p, e), t) if p.isPattern =>
        println(s"BIND ($p = $e) = $t")
        for {
          v <- eval(e)
          ρs <- matchPattern(p, v)
        } yield ρs

      case (All(p), v) =>
        matchPattern(p, v)

      // Select the first element of the union.
      case (Any(p), v) =>
        for {
          ρs <- matchPattern(p, v)
        } yield ρs match {
          case ρ::_ => ρ::Nil
          case Nil => Nil
        }

      // Select the unique element of the union
      case (Single(p), v) =>
        for {
          ρs <- matchPattern(p, v)
        } yield ρs match {
          case ρ::Nil => ρ::Nil
          case _ => Nil
        }

      // Random is the same as any, but selects
      // a random element of the union rather than the first.
      case (Random(p), v) =>
        import scala.util.{Random => R}
        for {
          ρs <- matchPattern(p, v)
        } yield R.shuffle(ρs) match {
          case ρ::_ => ρ::Nil
          case _ => Nil
        }

      case (p, v) =>
        logger.debug(s"match pattern $p ~ $v FAILED")
        point(Nil)
    }
  }

  def intersect(ρ1s: Env, ρ2s: Env): Eval[Env] = {
    val ρ1::_ = ρ1s
    val ρ2::_ = ρ2s
    for {
      ς <- get
      ρ3 <- ς match {
        case MState(nextLoc, σ) =>
          σ.get(ρ1) match {
            case Some(v1: Record) =>
              σ.get(ρ2) match {
                case Some(v2: Record) =>
                  val v3 = v1 intersect v2
                  for {
                    _ <- put(MState(nextLoc+1, σ + (Loc(nextLoc) -> v3)))
                  } yield (Loc(nextLoc)::Nil)
              }
          }
      }
    } yield ρ3
  }

  // abstract machine
  // maintain an argument stack and a return stack (see ocaml)
  // this makes currying implementation more efficient

  // translation of tail calls into jumps

  // maintain typed stacks -- this allows unboxed values on the stack

  // the big semantic problem is unions for calls
  // we need to support both cbv and cbn, with cbv being the more
  // efficient implementation

  // we want singleton unions to be as efficient as a single value




}
