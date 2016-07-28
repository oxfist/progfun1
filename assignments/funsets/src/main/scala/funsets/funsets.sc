import funsets.FunSets

type Set = Int => Boolean

def contains(s: Set, elem: Int): Boolean = s(elem)

def singletonSet(elem: Int): Set = { set => set == elem }

def union(s: Set, t: Set): Set = { num => s(num) || t(num) }

union(singletonSet(1), singletonSet(2))(1)
union(singletonSet(1), singletonSet(2))(3)

def intersect(s: Set, t: Set): Set = { num => s(num) && t(num) }
def diff(s: Set, t: Set): Set = { num => s(num) && !t(num) }

intersect(union(singletonSet(1), singletonSet(2)), singletonSet(2))(1)
diff(union(singletonSet(1), singletonSet(2)), singletonSet(1))(2)

def filter(s: Set, p: Int => Boolean): Set = { set => s(set) && p(set) }
filter(union(singletonSet(4), singletonSet(2)), { n => n % 2 == 0 })(4)

val bound = 1000

def forall(s: Set, p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean = {
    if (a < -bound) true
    else if (contains(s, a)) { if (p(a)) iter(a - 1) else false }
    else iter(a - 1)
  }
  iter(bound)
}

forall(union(singletonSet(11), singletonSet(101)), { n => n % 10 == 1 })
forall(union(singletonSet(2), singletonSet(3)), { n => n == 4 })

def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, { n => !p(n) })

exists(union(singletonSet(1), singletonSet(2)), { n => n % 2 == 0 })
exists(union(singletonSet(1), singletonSet(2)), { n => n == 3 })

def map(s: Set, f: Int => Int): Set = {
  def iter(a: Int, mappedSet: Set): Set = {
    if (a < -bound) mappedSet
    else if (contains(s, a)) iter(a - 1, union(singletonSet(f(a)), mappedSet))
    else iter(a - 1, mappedSet)
  }
  iter(bound, intersect(singletonSet(1), singletonSet(2)))
}

val s1 = union(union(singletonSet(1), singletonSet(2)), singletonSet(3))

def toString(s: Set): String = {
  val xs = for (i <- -bound to bound if contains(s, i)) yield i
  xs.mkString("{", ",", "}")
}

def map2(s: Set, f: Int => Int): Set = { b => exists(s, { a => f(a) == b }) }

println(toString(map(s1, { n => n * n })))
println(toString(map2(s1, { n => n * n })))

println(toString(map(s1, { n => n * n })))
