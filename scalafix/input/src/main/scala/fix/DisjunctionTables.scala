/*
rule = DisjunctionTables
 */
// format: off
package fix

object DisjunctionTables {
  class Cls { def derivesFrom(name: String): Boolean = name.nonEmpty }
  def requiredClass(name: String): String = name
  def keyword(name: String): Boolean = name.nonEmpty
  def pair(left: Int, right: String): Boolean = left > right.length
  def holds(flag: Boolean): Boolean = flag
  def check(value: String): Boolean = value.nonEmpty
  def wrap(value: Int): String = value.toString

  object Kind { val A = 1; val B = 2; val C = 3; val D = 4; val E = 5; val F = 6 }

  def names(name: String): Boolean =
    name == "a" || name == "b" || name == "c" || name == "d" || name == "e" || name == "f"// assert: DisjunctionTables.equality-disjunction

  def characters(character: Char): Boolean =
    character == ' ' || '\t' == character || character == '\n' || character == '\r' || character == '\f' || character == 'x'// assert: DisjunctionTables.equality-disjunction

  def numbers(code: Int): Boolean =
    code == 1 || code == 2 || code == 3 || code == 4 || code == 5 || code == 6 || code == 7// assert: DisjunctionTables.equality-disjunction

  def guarded(name: String, ok: Boolean): Boolean =
    ok && (name == "a" || name == "b" || name == "c" || name == "d" || name == "e" || name == "f")// assert: DisjunctionTables.equality-disjunction

  def untyped(value: Any): Boolean =
    value == "a" || value == "b" || value == "c" || value == "d" || value == "e" || value == "f"// assert: DisjunctionTables.equality-disjunction

  def wide(value: Long): Boolean =
    value == 1 || value == 2 || value == 3 || value == 4 || value == 5 || value == 6// assert: DisjunctionTables.equality-disjunction

  def mixed(name: String): Boolean =
    name == "a" || name == "b" || name == "c" || name == "d" || name == "e" || name.equals("f")// assert: DisjunctionTables.equality-disjunction

  def negated(name: String): Boolean =
    !(name == "a" || name == "b" || name == "c" || name == "d" || name == "e" || name == "f")// assert: DisjunctionTables.equality-disjunction

  def prefixes(path: String): Boolean =
    path.startsWith("a/") || path.startsWith("b/") || path.startsWith("c/") || path.startsWith("d/") || path.startsWith("e/") || path.startsWith("f/")// assert: DisjunctionTables.equality-disjunction

  def mixedPrefixes(n: String): Boolean = n == "a" || n == "b" || n == "c" || n.startsWith("d") || n.startsWith("e") || n.startsWith("f")// assert: DisjunctionTables.equality-disjunction

  def constants(kind: Int): Boolean =
    kind == Kind.A || kind == Kind.B || kind == Kind.C || Kind.D == kind || kind == Kind.E || kind == Kind.F// assert: DisjunctionTables.equality-disjunction

  def matrix(owner: String, name: String): Boolean =
    (owner == "java/util/List" && (name == "of" || name == "copyOf")) ||// assert: DisjunctionTables.equality-disjunction
      (owner == "java/util/Set" && (name == "of" || name == "copyOf")) ||
      (owner == "java/util/Map" && (name == "of" || name == "copyOf" || name == "ofEntries"))

  def derived(cls: Cls): Boolean =
    cls.derivesFrom(requiredClass("A")) || cls.derivesFrom(requiredClass("B")) || cls.derivesFrom(requiredClass("C")) ||// assert: DisjunctionTables.repeated-call-disjunction
      cls.derivesFrom(requiredClass("D"))

  def keywords(word: String): Boolean =
    keyword("if") || keyword("else") || keyword("for") || keyword("while")// assert: DisjunctionTables.repeated-call-disjunction

  def guardedKeywords(ok: Boolean): Boolean =
    ok && (keyword("if") || keyword("else") || keyword("for") || keyword("while"))// assert: DisjunctionTables.repeated-call-disjunction

  def receivers(word: String): Boolean =
    word.contains("a") || word.contains("b") || word.contains("c") || word.contains("d")// assert: DisjunctionTables.repeated-call-disjunction

  def constantArguments(cls: Cls): Boolean =
    cls.derivesFrom(Names.A) || cls.derivesFrom(Names.B) || cls.derivesFrom(Names.C) || cls.derivesFrom(Names.D)// assert: DisjunctionTables.repeated-call-disjunction

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

  def nullArgument(): Boolean = keyword(null) || keyword("if") || keyword("else") || keyword("for")

  def nestedArgument(): Boolean = check(wrap(1)) || check(wrap(2)) || check(wrap(3)) || check(wrap(4))// assert: DisjunctionTables.repeated-call-disjunction
}
