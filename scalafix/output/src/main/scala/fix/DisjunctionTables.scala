// format: off
package fix

object DisjunctionTables {
  class Cls { def derivesFrom(name: String): Boolean = name.nonEmpty }
  def requiredClass(name: String): String = name
  def keyword(name: String): Boolean = name.nonEmpty
  def pair(left: Int, right: String): Boolean = left > right.length
  def holds(flag: Boolean): Boolean = flag
  def check(value: String): Boolean = value.nonEmpty

  object Kind { val A = 1; val B = 2; val C = 3; val D = 4; val E = 5; val F = 6 }

  def names(name: String): Boolean =
    Set("a", "b", "c", "d", "e", "f").contains(name)

  def characters(character: Char): Boolean =
    Set(' ', '\t', '\n', '\r', '\f', 'x').contains(character)

  def numbers(code: Int): Boolean =
    Set(1, 2, 3, 4, 5, 6, 7).contains(code)

  def guarded(name: String, ok: Boolean): Boolean =
    ok && (name == "a" || name == "b" || name == "c" || name == "d" || name == "e" || name == "f")

  def untyped(value: Any): Boolean =
    value == "a" || value == "b" || value == "c" || value == "d" || value == "e" || value == "f"

  def wide(value: Long): Boolean =
    value == 1 || value == 2 || value == 3 || value == 4 || value == 5 || value == 6

  def mixed(name: String): Boolean =
    name == "a" || name == "b" || name == "c" || name == "d" || name == "e" || name.equals("f")

  def negated(name: String): Boolean =
    !(name == "a" || name == "b" || name == "c" || name == "d" || name == "e" || name == "f")

  def prefixes(path: String): Boolean =
    path.startsWith("a/") || path.startsWith("b/") || path.startsWith("c/") || path.startsWith("d/") || path.startsWith("e/") || path.startsWith("f/")

  def constants(kind: Int): Boolean =
    kind == Kind.A || kind == Kind.B || kind == Kind.C || Kind.D == kind || kind == Kind.E || kind == Kind.F

  def matrix(owner: String, name: String): Boolean =
    (owner == "java/util/List" && (name == "of" || name == "copyOf")) ||
      (owner == "java/util/Set" && (name == "of" || name == "copyOf")) ||
      (owner == "java/util/Map" && (name == "of" || name == "copyOf" || name == "ofEntries"))

  def derived(cls: Cls): Boolean =
    cls.derivesFrom(requiredClass("A")) || cls.derivesFrom(requiredClass("B")) || cls.derivesFrom(requiredClass("C")) ||
      cls.derivesFrom(requiredClass("D"))

  def keywords(word: String): Boolean =
    keyword("if") || keyword("else") || keyword("for") || keyword("while")

  def guardedKeywords(ok: Boolean): Boolean =
    ok && (keyword("if") || keyword("else") || keyword("for") || keyword("while"))

  def receivers(word: String): Boolean =
    word.contains("a") || word.contains("b") || word.contains("c") || word.contains("d")

  def constantArguments(cls: Cls): Boolean =
    cls.derivesFrom(Names.A) || cls.derivesFrom(Names.B) || cls.derivesFrom(Names.C) || cls.derivesFrom(Names.D)

  object Names { val A = "a"; val B = "b"; val C = "c"; val D = "d" }

  def fiveNames(name: String): Boolean =
    name == "a" || name == "b" || name == "c" || name == "d" || name == "e"

  def threeOperands(a: Int, b: Int, c: Int): Boolean =
    a == 1 || b == 2 || c == 3 || a == 4 || b == 5 || c == 6

  def variables(x: Int, a: Int, b: Int, c: Int, d: Int, e: Int, f: Int): Boolean =
    x == a || x == b || x == c || x == d || x == e || x == f

  def inequalities(name: String): Boolean =
    name != "a" && name != "b" && name != "c" && name != "d" && name != "e" && name != "f"

  def nested(x: Int, y: Int): Boolean =
    (x == 1 || x == 2 || x == 3) && holds(y == 4 || y == 5 || y == 6)

  def twoCalls(): Boolean = keyword("if") || keyword("else")

  def threeCalls(): Boolean = keyword("if") || keyword("else") || keyword("for")

  def twoPositions(): Boolean = pair(1, "a") || pair(2, "b") || pair(3, "c")

  def variableArguments(a: String, b: String, c: String): Boolean = check(a) || check(b) || check(c)

  def repeatedConstant(): Boolean = keyword("if") || keyword("if") || keyword("else")

  def conjunction(): Boolean = keyword("if") && keyword("else") && keyword("for")

  def receiversDiffer(a: Cls, b: Cls, c: Cls): Boolean = a.derivesFrom("x") || b.derivesFrom("x") || c.derivesFrom("x")

  def otherCall(): Boolean = keyword("if") || check("else") || keyword("for")

  def interrupted(other: Boolean): Boolean = keyword("if") || keyword("else") || other || keyword("for")

  def shortCalls(): Boolean = (keyword("if") || keyword("else")) && keyword("for")
}
