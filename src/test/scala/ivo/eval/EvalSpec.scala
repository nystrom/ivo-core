package fretta.eval

import com.typesafe.scalalogging._

import org.scalatest._

class EvalSpec extends FreeSpec with Matchers with Inside {
  import fretta.eval.Ivo._

  val logger = Logger("EvalSpec")

  def getLoc(p: (Unit, Value, MState)) = p match {
    case (_, v: Loc, MState(_, sigma)) =>
      sigma.get(v)
    case _ => None
  }

  "eval" - {
    "values" in {
      val input = LitVal(1)
      val v = eval(input).run(emptyMEnv, emptyMState)
      v.run._2 should be {
        LitVal(1)
      }
    }

    "arithmetic 1+2 == 3" in {
      val input = Binary("(_ + _ = ?)", LitVal(1), LitVal(2))
      val v = eval(input).run(emptyMEnv, emptyMState)
      v.run._2 should be {
        LitVal(3)
      }
    }

    "arithmetic 2*3 == 6" in {
      val input = Binary("(_ * _ = ?)", LitVal(2), LitVal(3))
      val v = eval(input).run(emptyMEnv, emptyMState)
      v.run._2 should be {
        LitVal(6)
      }
    }
  }

  "envs" - {
    "self" in {
      val input = Select(App(Lambda(In(), Unknown("x"), Self()), LitVal(1)), "x")
      val v = eval(input).run(emptyMEnv, emptyMState)
      v.run._2 should be {
        LitVal(1)
      }
    }
  }

  "matching" - {
    "calls" in {
      val input = App(Lambda(In(), Unknown("x"), Var("x")), LitVal(1))
      val v = eval(input).run(emptyMEnv, emptyMState)
      v.run._2 should be {
        LitVal(1)
      }
    }

    "by name calls" in {
      val input = App(Lambda(In(), ByNameMarker(Unknown("x")), Var("x")), LitVal(1))
      val v = eval(input).run(emptyMEnv, emptyMState)
      v.run._2 should be {
        LitVal(1)
      }
    }
  }

  "unions" - {
    "union calls no winner" in {
      val input = App(Union(Lambda(In(), Unknown("x"), Var("x")), Lambda(In(), Unknown("y"), LitVal(2))), LitVal(1))
      val v = eval(input).run(emptyMEnv, emptyMState)
      v.run._2 should be {
        Union(LitVal(1), LitVal(2))
      }
    }

    "union calls no winner with duplicates" in {
      val input = App(Union(Lambda(In(), Unknown("x"), Var("x")),
                            Union(Lambda(In(), Unknown("y"), LitVal(2)),
                                  Lambda(In(), Unknown("z"), Var("z")))), LitVal(1))
      val v = eval(input).run(emptyMEnv, emptyMState)
      v.run._2 should be {
        Union(LitVal(1), LitVal(2))
      }
    }

    "union calls one winner" in {
      val input = App(Union(Lambda(In(), Unknown("x"), Var("x")), Lambda(In(), LitVal(1), LitVal(2))), LitVal(1))
      val v = eval(input).run(emptyMEnv, emptyMState)
      v.run._2 should be {
        LitVal(2)
      }
    }

    "union calls same result" in {
      val input = App(Union(Lambda(In(), Unknown("x"), Var("x")), Lambda(In(), Unknown("y"), LitVal(1))), LitVal(1))
      val v = eval(input).run(emptyMEnv, emptyMState)
      v.run._2 should be {
        LitVal(1)
      }
    }
  }

  "for" - {
    "for" in {
      val input = For(Record("True", Nil), LitVal(4))
      val v = eval(input).run(emptyMEnv, emptyMState)
      v.run._2 should be {
        LitVal(4)
      }
    }

    "for bind" in {
      val input = For(Bind(Unknown("x"), LitVal(1)), Var("x"))
      val v = eval(input).run(emptyMEnv, emptyMState)
      v.run._2 should be {
        LitVal(1)
      }
    }

    "let" in {
      val input = For(Single(Bind(Unknown("x"), LitVal(1))), BinaryWithMode(In(), In(), "(_ + _)", LitVal(1), Var("x")))
      val v = eval(input).run(emptyMEnv, emptyMState)
      v.run._2 should be {
        LitVal(2)
      }
    }
  }

  "modes" - {
    "mode select _ + _" in {
      val input = BinaryWithMode(In(), In(), "(_ + _)", LitVal(2), LitVal(3))
      val v = eval(input).run(emptyMEnv, emptyMState)
      v.run._2 should be {
        LitVal(5)
      }
    }

    "mode select _ + ?" in {
      val input = BinaryWithMode(In(), Out(), "(_ + _)", LitVal(2), LitVal(3))
      val v = eval(input).run(emptyMEnv, emptyMState)
      v.run._2 should be {
        LitVal(1)
      }
    }

    "mode select ? + _" in {
      val input = BinaryWithMode(Out(), In(), "(_ + _)", LitVal(2), LitVal(3))
      val v = eval(input).run(emptyMEnv, emptyMState)
      v.run._2 should be {
        LitVal(1)
      }
    }
  }
}
