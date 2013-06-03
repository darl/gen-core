import com.github.darl.runtime._
import collection.mutable

random.randomize()
//random.init(500)

val questions = 6

val k = 10
val n = 5
val m = n * (n - 1)
val l = questions / 2
val q = questions - l

def genPairs[T](n: Int, f: Int => T): Seq[(T, T)] = {
  for {
    i <- 0 until n
    j <- 0 until n
    if i != j
  } yield (f(i), f(j))
}

val dims = random.shuffle(1 to k)

val matrices = genPairs(n, dims)

val pairs = genPairs(m, matrices)

val (right, wrong) = pairs.partition {
  case ((_, lw), (rh, _)) => lw == rh
}

validate(right.size >= l)
validate(wrong.size >= q)

val right2 = right.subset(l)
val wrong2 = wrong.subset(q)

val all = right2.map(_ -> true) ++ wrong2.map(_ -> false)
val valid = mutable.TreeSet.empty[Int]
val identifier = ('A' to 'Z').iterator

val variants =
  for {
    ((pair, isRight), i) <- all.shuffle zip (1 to all.size)
    a = identifier.next
    b = identifier.next
    ((leftH, leftW), (rightH, rightW)) = pair
  } yield {
    if (isRight) {
      valid += i
    }
    s"\\item $a[$leftH, $leftW]$b[$rightH, $rightW]"
  }

Problem(
  statement = "Укажите пары матриц, для которых произведение определено " +
    s"в указанном порядке: \\begin{enumerate}${variants.mkString(" ")}\\end{enumerate}",
  answer = valid.mkString(", ")
)
