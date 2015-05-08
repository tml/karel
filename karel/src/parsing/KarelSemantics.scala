package parsing

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.LinkedHashSet

class KarelSemantics(program: Program, entryPoint: String, val targetLevel: Int) {

  val defs = program.defs.map(def_ => (def_.identifier, def_)).toMap

  val calleesOf = program.defs.map(def_ => (def_, defsCalledBy(def_))).toMap

  private def defsCalledBy(node: Node) = {
    callsFrom(node).map(x => defs.get(x.identifier)).flatten
  }

  private def callsFrom(node: Node) = {
    val udcs = new ArrayBuffer[Call]()
    val walker = new TreeWalker({ case udc: Call => udcs += udc })
    walker.walk(node)
    udcs.toVector
  }

  val reachableDefs = defs.get(entryPoint) match {
    case Some(deF) => reachableFrom(deF)
    case None => Vector.empty
  }

  private def reachableFrom(start: Def) = {
    // entry point must come first for code generation!
    val visited = new LinkedHashSet[Def]

    def visit(current: Def) {
      visited += current
      calleesOf(current).filter(!visited.contains(_)).foreach(visit)
    }
    visit(start)
    visited.toVector
  }

  lazy val errors: List[Diagnostic] = {
    val allErrors = duplicateDefinitions ++ undefinedCommands ++ illegalWhileLoops ++ illegalRecursion
    // maintenance note: sortBy(_.pos) does not work here,
    // because even though Position offers a < method,
    // it does not implement the Ordered trait.
    allErrors.toList.sortWith(_.pos < _.pos)
  }

  private def duplicateDefinitions = {
    for ((identifier, firstOccurence :: secondOccurence :: _) <- program.defs.groupBy(_.identifier))
      yield Diagnostic(secondOccurence.pos, "command " + identifier + " was already defined on line " + firstOccurence.pos.line)
  }

  private def undefinedCommands = {
    for {
      call <- callsFrom(program)
      if (!defs.contains(call.identifier) && !vm.Instruction.builtinCommands.contains(call.identifier))
    } yield Diagnostic(call.pos, "undefined command " + call.identifier)
  }

  private def illegalWhileLoops = {
    if (targetLevel < 2)
      whileLoops.map(w => Diagnostic(w.pos, "iteration is not allowed yet"))
    else Vector.empty
  }

  private def whileLoops = {
    val whiles = new ArrayBuffer[While]
    val walker = new TreeWalker({ case w: While => whiles += w })
    reachableDefs.foreach(walker.walk)
    whiles.toVector
  }

  private def illegalRecursion = {
    if (targetLevel < 3)
      recursiveDefs.map(r => Diagnostic(r.pos, "recursion is not allowed yet"))
    else Vector.empty
  }

  private def recursiveDefs = {
    reachableDefs.filter(deF => calleesOf(deF).flatMap(reachableFrom).contains(deF))
  }
}
