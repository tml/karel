package detective

class Knowledge(private val formula: Long) extends AnyVal {
  import Knowledge._

  def &&(that: Knowledge) = Knowledge(this.formula & that.formula)
  def ||(that: Knowledge) = Knowledge(this.formula | that.formula)
  def unary_! = Knowledge(~formula)

  def implies(that: Knowledge) = ((!this || that) == TAUTOLOGY)

  def frontIsClear = implies(FRONT_IS_CLEAR)
  def frontIsBlocked = implies(!FRONT_IS_CLEAR)

  def leftIsClear = implies(LEFT_IS_CLEAR)
  def leftIsBlocked = implies(!LEFT_IS_CLEAR)

  def backIsClear = implies(BACK_IS_CLEAR)
  def backIsBlocked = implies(!BACK_IS_CLEAR)

  def rightIsClear = implies(RIGHT_IS_CLEAR)
  def rightIsBlocked = implies(!RIGHT_IS_CLEAR)

  def onBeeper = implies(ON_BEEPER)
  def noBeeper = implies(!ON_BEEPER)

  def beeperAhead = implies(BEEPER_AHEAD)
  def nothingAhead = implies(!BEEPER_AHEAD)

  // TODO Can this be improved with bit twiddling?
  def moveForward = {
    val temp = this && FRONT_IS_CLEAR

    if (temp.beeperAhead) BACK_IS_CLEAR && ON_BEEPER
    else if (temp.nothingAhead) BACK_IS_CLEAR && NO_BEEPER
    else BACK_IS_CLEAR
  }

  // see http://programming.sirrida.de/calcperm.php
  private def bit_permute_step(x: Long, mask: Long, shift: Int) = {
    val t = ((x >>> shift) ^ x) & mask
    (x ^ t) ^ (t << shift)
  }

  def turnLeft = {
    var x = forgetAhead
    x = bit_permute_step(x, 0x2222222222222222L, 1)
    x = bit_permute_step(x, 0x0c0c0c0c0c0c0c0cL, 2)
    x = bit_permute_step(x, 0x00f000f000f000f0L, 4)
    Knowledge(x)
  }

  def turnRight = {
    var x = forgetAhead
    x = bit_permute_step(x, 0x00aa00aa00aa00aaL, 7)
    x = bit_permute_step(x, 0x00cc00cc00cc00ccL, 6)
    x = bit_permute_step(x, 0x00f000f000f000f0L, 4)
    Knowledge(x)
  }

  def turnAround = {
    var x = forgetAhead
    x = bit_permute_step(x, 0x0a0a0a0a0a0a0a0aL, 3)
    x = bit_permute_step(x, 0x00cc00cc00cc00ccL, 6)
    Knowledge(x)
  }

  def pickBeeper = Knowledge(formula >>> 32)

  def dropBeeper = Knowledge(formula << 32)

  // see http://programming.sirrida.de/calcperm.php
  private def bit_permute_step_simple(x: Long, mask: Long, shift: Long) = {
    val t = (x >>> shift) & mask
    ((x & mask) << shift) | t
  }

  def forgetAhead = formula | bit_permute_step_simple(formula, 0x0000ffff0000ffffL, 16)

  override def toString = "%016x".format(formula)
}

object Knowledge {
  private def apply(x: Long) = new Knowledge(x)

  val /*  */ FRONT_IS_CLEAR = apply(0xaaaaaaaaaaaaaaaaL) // 01...
  val /**/ FRONT_IS_BLOCKED = !FRONT_IS_CLEAR
  val /*   */ LEFT_IS_CLEAR = apply(0xccccccccccccccccL) // 0011...
  val /* */ LEFT_IS_BLOCKED = !LEFT_IS_CLEAR
  val /*   */ BACK_IS_CLEAR = apply(0xf0f0f0f0f0f0f0f0L) // 00001111...
  val /* */ BACK_IS_BLOCKED = !BACK_IS_CLEAR
  val /*  */ RIGHT_IS_CLEAR = apply(0xff00ff00ff00ff00L) // 0000000011111111...
  val /**/ RIGHT_IS_BLOCKED = !RIGHT_IS_CLEAR
  val /*    */ BEEPER_AHEAD = apply(0xffff0000ffff0000L) // 00000000000000001111111111111111...
  val /*   */ NOTHING_AHEAD = !BEEPER_AHEAD
  val /*       */ ON_BEEPER = apply(0xffffffff00000000L) // 0000000000000000000000000000000011111111111111111111111111111111...
  val /*       */ NO_BEEPER = !ON_BEEPER

  val /*     */ TAUTOLOGY = apply(0xffffffffffffffffL)
  val /* */ CONTRADICTION = apply(0x0000000000000000L)
}
