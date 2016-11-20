
object State {

  def unit[S,A](a: A): State[S,A] = State((a, _))

  def map2[S,A,B,C](ra: State[S,A], rb: State[S,B])(f: (A, B) => C): State[S,C] = {
    ra.flatMap(a => rb.flatMap(b => unit(f(a, b))))
  }

  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] = {
    fs.foldRight(State.unit[S, List[A]](List[A]())) {
      (a0, acc) => State.map2(a0, acc)(_ :: _)
    }
  }
}
case class State[S,+A](run: S => (A,S)) {

  def flatMap[B](g: A => State[S, B]): State[S, B] = {
    State { s: S =>
      val (a, s1) = run(s)
      g(a).run(s1)
    }
  }

  def map[B](g: A => B): State[S, B] = {
    flatMap(a => State.unit(g(a)))
  }



}

sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def insertCoin(): Machine = {
    if (locked && candies > 0) {
      this.copy(locked = false, coins = coins + 1)
    }
    else this
  }

  def turnKnob(): Machine = {
    if (!locked) {
      this.copy(candies = candies - 1, locked = true)
    } else this
  }
}
def simulateMachineSingleInput(input: Input): State[Machine, (Int, Int)] = {

  def run(s: Machine): ((Int, Int), Machine) = {
    input match {
      case Coin =>
        val m = s.insertCoin()
        ((m.candies, m.coins), m)
      case Turn =>
        val m = s.turnKnob()
//        println("Turn")
//        println(m)
//        println(s)
        ((m.candies, m.coins), m)
    }
  }
  State(run)
}
def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
  State.sequence(inputs.map(simulateMachineSingleInput)).map(_.last)
}
val inputs = Coin :: Turn :: Coin :: Turn :: Coin :: Turn :: Coin :: Turn :: Turn :: Nil
val aa = simulateMachine(inputs)
val gg = aa.run(Machine(locked=true, 5, 10))

