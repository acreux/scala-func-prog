trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

val rng = SimpleRNG(42)
val (n1, rng2) = rng.nextInt
val (n2, rng3) = rng2.nextInt
def nonNegativeInt(rng: RNG): (Int, RNG) = {
  val (int0, rng0) = rng.nextInt
  val int1 = if (int0 >= 0) int0 else -(int0 + 1)
  (int1, rng0)
}

def double(rng: RNG): (Double, RNG) = {
  val (int0, rng0) = nonNegativeInt(rng)
  (int0.toDouble / Int.MaxValue, rng0)
}

def intDouble(rng: RNG): ((Int,Double), RNG) = {
  val (int0, rng0) = rng.nextInt
  val (double0, rng1) = double(rng0)
  ((int0, double0), rng1)
}

def doubleInt(rng: RNG): ((Double,Int), RNG) = {
  val ((int0, double0), rng0) = intDouble(rng)
  ((double0, int0), rng0)
}

def double3(rng: RNG): ((Double,Double,Double), RNG) = {
  val (double0, rng0) = double(rng)
  val (double1, rng1) = double(rng0)
  val (double2, rng2) = double(rng1)
  ((double0, double1, double2), rng2)
}

def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
  if (count == 0) {
    (Nil, rng)
  } else {
    val (int0, rng0) = rng.nextInt
    val (list, rngEnd) = ints(count - 1)(rng0)
    (int0 ::list, rngEnd)
  }
}

type Rand[+A] = RNG => (A, RNG)

val int: Rand[Int] = _.nextInt

def unit[A](a: A): Rand[A] = rng => (a, rng)

def mapFirst[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
  val (a, rng2) = s(rng)
  (f(a), rng2)
}

def nonNegativeEven: Rand[Int] = {
  mapFirst(nonNegativeInt)(i => i - i % 2)
}


def mapDouble: Rand[Double] = {
  mapFirst(nonNegativeInt)(i => i.toDouble / Int.MaxValue)
}

mapDouble(rng)

def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
   rng0 => {
     val (a, rng1) = ra(rng0)
     val (b, rng2) = rb(rng1)
     (f(a, b), rng2)
   }
}

def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

val randIntDouble: Rand[(Int, Double)] = both(int, double)

val randDoubleInt: Rand[(Double, Int)] = both(double, int)

def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
  fs match {
    case head :: tail =>
      rng0 => {
        val (a, rng1) = head(rng0)
        val b = sequence(tail)
        val (b0, rng2) = b(rng1)
        (a :: b0, rng2)
      }
    case Nil => unit(Nil)
  }
}

def sequenceFoldLeft[A](fs: List[Rand[A]]): Rand[List[A]] = {
  fs.foldLeft(unit(List[A]())) {
    (acc, a0) => map2(a0, acc)(_ :: _)
  }
}


def intsSequence(count: Int): Rand[List[Int]] = {
  sequenceFoldLeft(List.fill(count)(int))
}


def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
  val (a, rng0) = f(rng)
  g(a)(rng0)
}

def mapUsingFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = {
  flatMap(s)(a => unit(f(a)))
}

def map2UsingFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
  flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
}


// type StateNew[S,+A] = S => (A,S)

object StateNew {

  def unit[S,A](a: A): StateNew[S,A] = StateNew((a, _))

  def map2[S,A,B,C](ra: StateNew[S,A], rb: StateNew[S,B])(f: (A, B) => C): StateNew[S,C] = {
    ra.flatMap(a => rb.flatMap(b => unit(f(a, b))))
  }

  def sequence[S,A](fs: List[StateNew[S,A]]): StateNew[S,List[A]] = {
    fs.foldLeft(StateNew.unit[S,List[A]](List[A]())) {
      (acc, a0) => StateNew.map2(a0, acc)(_ :: _)
    }
  }
}


case class StateNew[S,+A](run: S => (A,S)) {

  def flatMap[B](g: A => StateNew[S, B]): StateNew[S, B] = {
    StateNew { s: S =>
      val (a, s1) = run(s)
      g(a).run(s1)
    }
  }

  def map[B](g: A => B): StateNew[S, B] = {
    flatMap(a => StateNew.unit(g(a)))
  }

  def map2[B,C](f: A => B): StateNew[S, B] = {
    flatMap(a => StateNew.unit(f(a)))
  }

}


type RandNew[A] = StateNew[RNG, A]




