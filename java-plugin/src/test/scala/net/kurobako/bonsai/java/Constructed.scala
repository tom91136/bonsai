package net.kurobako.bonsai.java

/** Classes whose field `value` has no initialiser, which `ACCESS` is the access of: those the constructors all assign
  * once, and those that leave it open or assign it again.
  */
object Constructed {

  val assigned: Vector[(String, String)] = Vector(
    "a plain assignment"     -> "ACCESS int value; Case(int v) { value = v; }",
    "a qualified assignment" -> "ACCESS int value; Case(int v) { this.value = v; }",
    "both branches of an if" ->
      "ACCESS int value; Case(int v) { if (v > 0) { value = 1; } else { value = 2; } }",
    "nested branches" ->
      "ACCESS int value; Case(int v) { if (v > 0) { if (v > 5) value = 1; else value = 2; } else value = 3; }",
    "every case of a switch" ->
      """ACCESS int value;
        |Case(int v) {
        |  switch (v) {
        |    case 0: value = 1; break;
        |    case 1: value = 2; break;
        |    default: value = 3;
        |  }
        |}""".stripMargin,
    "every case of an arrow switch" ->
      "ACCESS int value; Case(int v) { switch (v) { case 0 -> value = 1; case 1 -> { value = 2; } default -> value = 3; } }",
    "a try whose catch throws" ->
      """ACCESS int value;
        |Case(String s) {
        |  try { value = Integer.parseInt(s); } catch (NumberFormatException e) { throw new IllegalArgumentException(e); }
        |}""".stripMargin,
    "a return after the assignment" ->
      "ACCESS int value; Case(int v) { if (v == 0) { value = 0; return; } value = v; }",
    "a throwing branch" ->
      "ACCESS int value; Case(int v) { if (v < 0) throw new IllegalArgumentException(); value = v; }",
    "two constructors"         -> "ACCESS int value; Case(int v) { value = v; } Case() { value = 0; }",
    "a delegating constructor" -> "ACCESS int value; Case(int v) { value = v; } Case() { this(0); }",
    "a chain of delegation"    ->
      "ACCESS int value; Case(int v) { value = v; } Case() { this(0); } Case(String s) { this(); }",
    "an explicit super call"               -> "ACCESS int value; Case(int v) { super(); value = v; }",
    "an assignment after other statements" ->
      "ACCESS int value; Case(int v) { int doubled = v * 2; System.out.println(doubled); value = doubled; }",
    "a synchronized block"                   -> "ACCESS int value; Case(int v) { synchronized (this) { value = v; } }",
    "a constant true condition"              -> "ACCESS int value; Case(int v) { if (true) { value = v; } }",
    "a constant true condition with an else" ->
      "ACCESS int value; Case(int v) { if (true) value = 1; else value = 2; }",
    "a constant false condition with an else" ->
      "ACCESS int value; Case(int v) { if (false) value = 1; else value = v; }",
    "a negated constant"     -> "ACCESS int value; Case(int v) { if (!false) value = v; }",
    "a constant conjunction" -> "ACCESS int value; Case(int v) { if (true && !false) value = v; }",
    "a constant field"       -> "static final boolean ON = true; ACCESS int value; Case(int v) { if (ON) value = v; }",
    "a constant local"       -> "ACCESS int value; Case(int v) { final boolean on = true; if (on) value = v; }",
    "a constant comparison"  -> "ACCESS int value; Case(int v) { if (1 < 2) value = v; }",
    "an endless while with a break"    -> "ACCESS int value; Case(int v) { while (true) { value = v; break; } }",
    "an endless for with a break"      -> "ACCESS int value; Case(int v) { for (;;) { value = v; break; } }",
    "an endless do with a break"       -> "ACCESS int value; Case(int v) { do { value = v; break; } while (true); }",
    "an endless loop left by a branch" ->
      "ACCESS int value; Case(int v) { while (true) { if (v > 0) { value = v; break; } v++; } }",
    "an endless loop with a continue" ->
      """ACCESS int value; Case(int v) { while (true) { if (v < 0) { v++; continue; } value = v;
        |break; } }""".stripMargin,
    "a labelled break out of nested loops" ->
      """ACCESS int value; Case(int v) { outer: while (true) { while (true) { value = v; break outer;
        |} } }""".stripMargin,
    "a break of an inner loop" ->
      """ACCESS int value; Case(int v) { while (true) { while (v > 0) { break; } value = v; break; }
        |}""".stripMargin,
    "an endless for with an update" ->
      """ACCESS int value; Case(int v) { for (int i = 0; ; i++) { if (i == v) { value = i; break; } }
        |}""".stripMargin,
    "a labelled continue" ->
      """ACCESS int value; Case(int v) { outer: while (true) { while (v > 0) { v--; continue outer; }
        |value = v; break; } }""".stripMargin,
    "a switch expression whose arms assign" ->
      """ACCESS int value; Case(int v) { int w = switch (v) { case 0 -> { value = 1; yield 1; }
        |default -> { value = 2; yield 2; } }; }""".stripMargin,
    "a switch expression with old style yields" ->
      """ACCESS int value; Case(int v) { int w = switch (v) { case 0: value = 1; yield 1; default:
        |value = 2; yield 2; }; }""".stripMargin,
    "a switch expression whose arms are assignments" ->
      """ACCESS int value; Case(int v) { int w = switch (v) { case 0 -> value = 1; default -> value =
        |2; }; }""".stripMargin,
    "a switch expression assigned to a variable" ->
      """ACCESS int value; Case(int v) { int w; w = switch (v) { case 0 -> { value = 1; yield 1; }
        |default -> { value = 2; yield 2; } }; }""".stripMargin,
    "a field assigned a switch expression" ->
      """ACCESS int value; Case(int v) { value = switch (v) { case 0 -> 1; default -> { yield 2; } };
        |}""".stripMargin,
    "an assignment in finally" ->
      "ACCESS int value; Case(int v) { try { System.out.println(); } finally { value = v; } }",
    "an assignment in finally after a catch" ->
      """ACCESS int value; Case(int v) { try { v++; } catch (RuntimeException e) {
        |System.out.println(); } finally { value = v; } }""".stripMargin,
    "a return in a try whose finally assigns" ->
      "ACCESS int value; Case(int v) { try { if (v < 0) return; v++; } finally { value = v; } }",
    "a return in a catch whose finally assigns" ->
      "ACCESS int value; Case(int v) { try { v++; } catch (RuntimeException e) { return; } finally { value = v; } }",
    "a try that always returns whose finally assigns" ->
      "ACCESS int value; Case(int v) { try { return; } finally { value = v; } }",
    "a break in a try whose finally assigns" ->
      "ACCESS int value; Case(int v) { switch (v) { default: try { break; } finally { value = v; } } }",
    "a labelled break in a try whose finally assigns" ->
      "ACCESS int value; Case(int v) { done: { try { if (v > 0) break done; v++; } finally { value = v; } } }",
    "an assignment in the first operand of a conjunction" ->
      "ACCESS int value; Case(int v) { if ((value = v) > 0 && v > 5) { v++; } }",
    "an assignment in the first operand of a disjunction" ->
      "ACCESS int value; Case(int v) { if ((value = v) > 0 || v > 5) { v++; } else { v--; } }",
    "a conjunction whose failure throws" ->
      "ACCESS int value; Case(int v) { if (v > 0 && (value = v) > 1) { v++; } else { throw new IllegalStateException(); } }",
    "a negated conjunction that throws" ->
      "ACCESS int value; Case(int v) { if (!(v > 0 && (value = v) > 1)) throw new IllegalStateException(); }",
    "a disjunction whose success throws" ->
      "ACCESS int value; Case(int v) { if (v > 0 || (value = v) > 1) { throw new IllegalStateException(); } }",
    "a conditional whose arms assign" ->
      "ACCESS int value; Case(int v) { if (v > 0 ? (value = 1) > 0 : (value = 2) > 0) { v++; } }",
    "a conditional value whose arms assign" ->
      "ACCESS int value; Case(int v) { int w = v > 0 ? (value = 1) : (value = 2); }",
    "a conditional inside a conjunction" ->
      "ACCESS int value; Case(int v) { if ((v > 0 ? (value = 1) : (value = 2)) > 0 && v > 5) { v++; } }",
    "a do loop that runs once"            -> "ACCESS int value; Case(int v) { do { value = v; } while (false); }",
    "a do loop that runs once and breaks" ->
      "ACCESS int value; Case(int v) { do { value = v; if (v > 0) break; v++; } while (false); }",
    "a do loop that runs once and continues" ->
      "ACCESS int value; Case(int v) { do { value = v; if (v > 0) continue; v++; } while (false); }",
    "a do loop with a negated constant"    -> "ACCESS int value; Case(int v) { do { value = v; } while (!true); }",
    "a do loop with a constant comparison" -> "ACCESS int value; Case(int v) { do { value = v; } while (1 > 2); }",
    "a labelled do loop that runs once"    ->
      "ACCESS int value; Case(int v) { loop: do { value = v; break loop; } while (false); }",
    "a loop that does not touch the field" ->
      "ACCESS int value; Case(int v) { while (v > 0) { v--; } for (int i = 0; i < v; i++) { v--; } value = v; }",
    "a loop that returns after assigning" ->
      "ACCESS int value; Case(int v) { while (v > 0) { if (v > 5) { value = 1; return; } v--; } value = 2; }",
    "a nested assignment" -> "ACCESS int value; Case(int v) { int w = (value = v) + 1; }",
    "a finally that throws before it assigns on one path" ->
      "ACCESS int value; Case(int v) { try { v++; } finally { if (v > 0) throw new IllegalStateException(); value = v; } }",
    "a conditional statement expression whose arms assign" ->
      "ACCESS int value; Case(int v) { boolean b = v > 0 ? (value = 1) > 0 : (value = 2) > 0; }",
    "a do loop that runs once inside a try" ->
      "ACCESS int value; Case(int v) { try { do { value = v; break; } while (false); } finally { v++; } }"
  )

  val rejected: Vector[(String, String)] = Vector(
    "one branch only"                          -> "ACCESS int value; Case(int v) { if (v > 0) { value = v; } }",
    "a constructor that skips"                 -> "ACCESS int value; Case(int v) { value = v; } Case() { }",
    "no constructor"                           -> "ACCESS int value;",
    "a delegation to a constructor that skips" -> "ACCESS int value; Case(int v) { } Case() { this(0); }",
    "a switch without a default"               ->
      "ACCESS int value; Case(int v) { switch (v) { case 0: value = 1; break; case 1: value = 2; break; } }",
    "a switch that breaks before the assignment" ->
      "ACCESS int value; Case(int v) { switch (v) { case 0: if (v > 5) break; value = 1; break; default: value = 2; } }",
    "a try whose catch falls through" ->
      "ACCESS int value; Case(String s) { try { value = Integer.parseInt(s); } catch (NumberFormatException e) { } }",
    "a return before the assignment" -> "ACCESS int value; Case(int v) { if (v == 0) { return; } value = v; }",
    "a while loop"                   -> "ACCESS int value; Case(int v) { while (v > 0) { value = v; v--; } }",
    "a for loop"                     -> "ACCESS int value; Case(int v) { for (int i = 0; i < v; i++) { value = i; } }",
    "a constant false condition"     -> "ACCESS int value; Case(int v) { if (false) value = v; }",
    "a constant true condition with the assignment in the dead else" ->
      "ACCESS int value; Case(int v) { if (true) { } else value = v; }",
    "a condition that is not constant"  -> "ACCESS int value; Case(int v) { boolean on = true; if (on) value = v; }",
    "an endless loop that breaks first" ->
      "ACCESS int value; Case(int v) { while (true) { if (v > 0) break; value = v; break; } }",
    "a switch expression arm that skips" ->
      """ACCESS int value; Case(int v) { int w = switch (v) { case 0 -> { value = 1; yield 1; }
        |default -> 2; }; }""".stripMargin,
    "a yield before the assignment" ->
      """ACCESS int value; Case(int v) { int w = switch (v) { case 0 -> { if (v > 5) yield 3; value =
        |1; yield 1; } default -> { value = 2; yield 2; } }; }""".stripMargin,
    "a finally that assigns on one path" ->
      "ACCESS int value; Case(int v) { try { v++; } finally { if (v > 0) value = v; } }",
    "a read in finally of a field the try assigns" ->
      "ACCESS int value; Case(int v) { try { value = v; } finally { System.out.println(value); } }",
    "a return in a try whose finally assigns on one path" ->
      "ACCESS int value; Case(int v) { try { return; } finally { if (v > 0) value = v; } }",
    "a return in a try whose finally leaves the field" ->
      "ACCESS int value; Case(int v) { try { if (v < 0) return; } finally { v++; } value = v; }",
    "a break in a try whose finally leaves the field" ->
      "ACCESS int value; Case(int v) { switch (v) { case 0: try { break; } finally { v++; } default: value = 2; } }",
    "a conjunction with the assignment in the second operand" ->
      "ACCESS int value; Case(int v) { if (v > 0 && (value = v) > 1) { v++; } }",
    "a disjunction with the assignment in the second operand" ->
      "ACCESS int value; Case(int v) { if (v > 0 || (value = v) > 1) { v++; } }",
    "a conditional with one assigning arm" ->
      "ACCESS int value; Case(int v) { int w = v > 0 ? (value = 1) : 2; }",
    "a conditional condition with one assigning arm" ->
      "ACCESS int value; Case(int v) { if (v > 0 ? (value = 1) > 0 : v > 5) { v++; } }",
    "a conjunction whose result is tested later" ->
      "ACCESS int value; Case(int v) { boolean ok = v > 0 && (value = v) > 1; if (!ok) { throw new IllegalStateException(); } }",
    "a do loop that breaks before the assignment" ->
      "ACCESS int value; Case(int v) { do { if (v > 0) break; value = v; } while (false); }",
    "a do loop that continues before the assignment" ->
      "ACCESS int value; Case(int v) { do { if (v > 0) continue; value = v; } while (false); }",
    "a finally that returns, which the try may skip before assigning" ->
      "ACCESS int value; Case(int v) { try { value = v; } finally { if (v > 0) return; } }",
    "a finally that returns before it assigns" ->
      "ACCESS int value; Case(int v) { try { return; } finally { if (v > 0) return; value = v; } }",
    "a do loop that continues before the assignment of a repeating loop" ->
      "ACCESS int value; Case(int v) { do { if (v > 0) continue; value = v; } while (v < 5); }",
    "a disjunction assigning only in the second operand" ->
      "ACCESS int value; Case(int v) { boolean b = v > 0 || (value = v) > 0; }",
    "a do loop that does not assign"       -> "ACCESS int value; Case(int v) { do { v++; } while (false); }",
    "a do loop that repeats"               -> "ACCESS int value; Case(int v) { do { value = v; } while (v > 0); }",
    "a loop that returns before assigning" ->
      "ACCESS int value; Case(int v) { while (v > 0) { if (v > 5) { return; } v--; } value = 2; }"
  )

  val repeated: Vector[(String, String)] = Vector(
    "two assignments"       -> "ACCESS int value; Case(int v) { value = 1; value = v; }",
    "a compound assignment" -> "ACCESS int value; Case(int v) { value = 1; value += v; }",
    "an increment"          -> "ACCESS int value; Case(int v) { value = 1; value++; }",
    "both try and catch"    ->
      """ACCESS int value;
        |Case(String s) {
        |  try { value = Integer.parseInt(s); } catch (NumberFormatException e) { value = 0; }
        |}""".stripMargin,
    "an assignment after a try" ->
      "ACCESS int value; Case(int v) { try { value = v; } finally { System.out.println(); } value = 1; }",
    "an assignment after the delegation" ->
      "ACCESS int value; Case(int v) { value = v; } Case() { this(0); value = 1; }",
    "a lambda" -> "ACCESS int value; Case(int v) { Runnable r = () -> value = 1; value = v; }",
    "an assignment through another instance" ->
      "ACCESS int value; Case(Case other) { other.value = 1; value = 2; }",
    "a read before the assignment"              -> "ACCESS int value; Case(int v) { int w = value + 1; value = v; }",
    "a read through this before the assignment" ->
      "ACCESS int value; Case(int v) { int w = this.value + 1; value = v; }",
    "an endless loop that retries a try" ->
      """ACCESS int value; Case(String s) { while (true) { try { value = Integer.parseInt(s); break;
        |} catch (NumberFormatException e) { s = "0"; } } }""".stripMargin,
    "an assignment after a constant true condition" ->
      "ACCESS int value; Case(int v) { if (true) { value = 1; } value = v; }",
    "an endless loop that assigns without a break" ->
      "ACCESS int value; Case(int v) { while (true) { value = v; if (v > 0) break; } }",
    "an assignment before a continue" ->
      "ACCESS int value; Case(int v) { while (true) { value = v; if (v > 0) continue; break; } }",
    "an assignment in both try and finally" ->
      "ACCESS int value; Case(int v) { try { value = 1; } finally { value = 2; } }",
    "an assignment in both catch and finally" ->
      """ACCESS int value; Case(int v) { try { v++; } catch (RuntimeException e) { value = 1; }
        |finally { value = 2; } }""".stripMargin,
    "a catch that assigns after a try that left the loop" ->
      """ACCESS int value; Case(int v) { while (true) { try { value = 1; break; } catch
        |(RuntimeException e) { value = 2; break; } } }""".stripMargin,
    "an assignment after a switch expression" ->
      """ACCESS int value; Case(int v) { int w = switch (v) { case 0 -> { value = 1; yield 1; }
        |default -> { value = 2; yield 2; } }; value = 3; }""".stripMargin,
    "a catch that assigns after a try that returned" ->
      """ACCESS int value; Case(int v) { try { value = 1; return; } catch (RuntimeException e) {
        |value = 2; } }""".stripMargin,
    "a return in a try that assigned before a finally that assigns" ->
      "ACCESS int value; Case(int v) { try { if (v > 0) { value = 1; return; } } finally { value = v; } }",
    "an assignment after a try whose finally assigns" ->
      "ACCESS int value; Case(int v) { try { if (v < 0) return; } finally { value = v; } value = 1; }",
    "a break in a try whose finally assigns again" ->
      "ACCESS int value; Case(int v) { switch (v) { default: value = 1; try { break; } finally { value = 2; } } }",
    "an endless loop left by a try whose finally assigns" ->
      "ACCESS int value; Case(int v) { while (true) { try { break; } finally { value = v; } } }",
    "an assignment after a switch whose break passes a finally that assigns" ->
      "ACCESS int value; Case(int v) { switch (v) { default: try { break; } finally { value = v; } } value = 1; }",
    "an assignment in the else of a conjunction" ->
      "ACCESS int value; Case(int v) { if (v > 0 && (value = v) > 1) { v++; } else { value = 2; } }",
    "an assignment in the then of a disjunction" ->
      "ACCESS int value; Case(int v) { if (v > 0 || (value = v) > 1) { value = 2; } }",
    "an assignment in both operands" ->
      "ACCESS int value; Case(int v) { if ((value = v) > 0 && (value = 2) > 0) { v++; } }",
    "an assignment in the arm of a conditional after another" ->
      "ACCESS int value; Case(int v) { value = 1; int w = v > 0 ? (value = 1) : 2; }",
    "a read before the assignment in a conjunction" ->
      "ACCESS int value; Case(int v) { if (value > 0 && (value = v) > 1) { v++; } }",
    "an assignment after a do loop that runs once" ->
      "ACCESS int value; Case(int v) { do { value = v; } while (false); value = 1; }",
    "an assignment in the condition of a do loop" ->
      "ACCESS int value; Case(int v) { do { v++; } while ((value = v) > 0); }",
    "an assignment in the condition of a while loop" ->
      "ACCESS int value; Case(int v) { while ((value = v) > 0) { v--; } }",
    "an assignment skipped by a constant conjunction before a conditional" ->
      "ACCESS int value; Case(int v) { int w = (false && (value = v) > 0) ? (value = 1) : (value = 2); }",
    "an assignment skipped by a constant disjunction before another" ->
      "ACCESS int value; Case(int v) { if (v > 0 || true) { v++; } else { value = 1; } if ((true || (value = v) > 0)) { value = 2; } }",
    "a read before the assignment hidden by a constant condition" ->
      "ACCESS int value; Case(int v) { int w = value + 1; if (v > 0 || true) throw new IllegalStateException(); value = v; }",
    "an assignment in a loop whose exit is constant" ->
      "ACCESS int value; Case(int v) { for (int i = 0; (true || (value = v) > 0); i++) { value = v; if (false) continue; else throw new IllegalStateException(); } }",
    "an assignment in a dead branch followed by another" ->
      "ACCESS int value; Case(int v) { if (false) { value = 1; } value = 2; }"
  )

  /** Classes whose constructors assign the field once and which other code writes as well. */
  val written: Vector[(String, String)] = Vector(
    "an initialiser block" -> "ACCESS int value; { value = 1; } Case(int v) { value = v; }",
    "a method"             -> "ACCESS int value; Case(int v) { value = v; } void set(int v) { value = v; }",
    "a nested class"       ->
      "ACCESS int value; Case(int v) { value = v; } static class Inner { void set(Case c) { c.value = 1; } }"
  )

  val excluded: Vector[(String, String)] = Vector(
    "a static field"   -> "ACCESS static int value; Case(int v) { value = v; }",
    "a final field"    -> "ACCESS final int value; Case(int v) { value = v; }",
    "a volatile field" -> "ACCESS volatile int value; Case(int v) { value = v; }"
  )

  val annotated: (String, String) = "an annotated field" -> "@Deprecated ACCESS int value; Case(int v) { value = v; }"

  /** The source of the class around the members, with the access of the field. */
  def source(members: String, access: String): String =
    s"package demo;\n\npublic class Case {\n${members.replace("ACCESS", access)}\n}\n"
}
