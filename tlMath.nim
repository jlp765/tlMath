############################################################
# genMath
############################################################
#
# math type-independent (string, int, float)
# similar to how awk can handle multiple types
#
# This is NOT as fast as handling the types yourself
# but is a compromise that makes it easier for
# situations like a DSL has dynamic types
# or where you just want an easy solution to
# doing maths across multiple types (a lazy
# approach :-) )
#
# Behind the scenes, numbers are floats, and integers
# are tracked, so that conversion to strings display
# integer values as non-floats.
# ANY division causes the result to be a float answer
#
# Most math functions "lifted" from the system math library
# are float results.
# If a proc in the math lib returns a float,
# then it will be a float result in genMath lib also.
#
# Conversion of non-numeric strings to a generic number
# results in a 0.0 float result, so doing operations
# using a non-numeric string is the same as doing that
# operation with a zero float value
#
###########################################################

import math, strutils

type
  gmObj* = object
    ## base generic Math object
    val*: float    ## actual value used in calculations
    isInt*: bool   ## track if number is actually an integer

proc `=`*(a: var gmObj, b: gmObj) =
  ## `a` is assigned the value of b
  ## keeping track of whether b was an integer value
  a.val = b.val
  a.isInt = b.isInt

proc newGM*(s: string): gmObj =
  result.val = 0.0
  result.isInt = false
  try:
    result.val = s.parseFloat
    result.isInt = (not s.contains("."))
  except: discard

proc newGM(i: int): gmObj =
  result.val = i.toFloat
  result.isInt = true

proc newGM(f: float): gmObj =
  result.val = f
  result.isInt = false

proc toGM*(s: string): gmObj = newGM(s)
  ## return a string as a generic Math object
  ## Non-numeric strings produce a zero float number
proc toGM*(i: SomeInteger): gmObj = newGM(i)
  ## return an integer as a generic Math object
proc toGM*(f: float): gmObj = newGM(f)
  ## return a float as a generic Math object

proc toInt*(g: gmObj): int =
  ## return  the generic Math object rounded to an integer
  result = g.val.toInt
proc toFloat*(g: gmObj): float =
  ## return the generic Math object as a float
  result = g.val

proc `$`*[T: gmObj](a: T): string =
  ## return the string representation of a
  ## (as an integer if a was an integer number,
  ## else as a float representation)
  if a.isInt:
    result = $(a.val.toInt)
  else:
    result = $a.val

template doCmp2NewAnyResult*(Op) =      # Comparison procs
  proc Op*[T: gmObj](a, b: T): bool =
    if a.isInt and b.isInt:
      result = Op(a.toInt, b.toInt)
    else:
      result = Op(a.val, b.val)
  proc Op*[T: gmObj](a: T, b: int): bool =
    if a.isInt:
      result = Op(a.toInt, b)
    else:
      result = Op(a.val, b.float)
  proc Op*[T: gmObj](a: T, b: float): bool =
    result = Op(a.val,  b)
  proc Op*[T: gmObj](a: T, b: string): bool =
    result = Op(a, toGM(b))

template doOp2NewAnyResult*(fname) =
  proc fname*[T: gmObj](a, b: T): T =
    result.val = fname(a.val,  b.val)
    if a.isInt and b.isInt:
      result.isInt = true
    else:
      result.isInt = false
  proc fname*[T: gmObj](a: T, b: int): T =
    result.val = fname(a.val,  b.float)
    if a.isInt:
      result.isInt = true
    else:
      result.isInt = false
  proc fname*[T: gmObj](a: T, b: float): T =
    result.val = fname(a.val,  b)
    result.isInt = false
  proc fname*[T: gmObj](a: T, b: string): T =
    result = fname(a, toGM(b))

template doOp2NewFloatResult*(fname) =
  proc fname*[T: gmObj](a, b: T): T =
    result.val = fname[float](a.val, b.val)
    result.isInt = false
  proc fname*[T: gmObj](a: T, b: int): T =
    result.val = fname[float](a.val, b.float)
    result.isInt = false
  proc fname*[T: gmObj](a: T, b: float): T =
    result.val = fname[float](a.val, b)
    result.isInt = false
  proc fname*[T: gmObj](a: T, s: string): T =
    let z = toGM(s)
    result.val = fname(a.val, z.val)
    result.isInt = false

template doOp2UpdateAnyResult*(fname) =
  proc fname*[T: gmObj](a: var T, b: T) =
    fname(a.val,  b.val)
    if a.isInt and b.isInt:
      a.isInt = true
    else:
      a.isInt = false
  proc fname*[T: gmObj](a: var T, b: int) =
    fname(a.val,  b.float)
    if a.isInt:
      a.isInt = true
    else:
      a.isInt = false
  proc fname*[T: gmObj](a: var T, b: float) =
    fname(a.val,  b)
    a.isInt = false
  proc fname*[T: gmObj](a: var T, b: string) =
    fname(a, toGM(b))

template doOp2UpdateFloatResult*(fname) =
  proc fname*[T: gmObj](a: var T, b: T) =
    fname(a.val,  b.val)
    a.isInt = false
  proc fname*[T: gmObj](a: var T, b: int) =
    fname(a.val,  b.float)
    a.isInt = false
  proc fname*[T: gmObj](a: var T, b: float) =
    fname(a.val,  b)
    a.isInt = false
  proc fname*[T: gmObj](a: var T, s: string) =
    let z = toGM(s)
    fname(a.val, z.val)
    a.isInt = false

template doFunc1NewAnyResult*(fname) =
  proc fname*[T: gmObj](a: T): T =
    result = newGM(fname(a.val))
    if a.isInt:
      result.isInt = true

template doFunc1NewFloatResult*(fname) =
  proc fname*[T: gmObj](a: T): T =
    result.val = fname(a.val)
    result.isInt = false

template doFunc2NewAnyResult*(fname) =
  proc fname*[T: gmObj](a, b: T): T =
    result = newGM(fname(a.val, b.val))
    if a.isInt and b.isInt:
      result.isInt = true

template doFunc2NewFloatResult*(fname) =
  proc fname*[T: gmObj](a, b: T): T =
    result.val = fname(a.val, b.val)
    result.isInt = false

doCmp2NewAnyResult(`==`)
doCmp2NewAnyResult(`!=`)
doCmp2NewAnyResult(`>`)
doCmp2NewAnyResult(`<`)
proc `>=`*(a, b: gmObj): bool =
  if a.isInt and b.isInt:
    result = (a.toInt >= b.toInt)
  else:
    result = `>`(a,b) or `==`(a,b)

proc `<=`*(a, b: gmObj): bool =
  if a.isInt and b.isInt:
    result = (a.toInt <= b.toInt)
  else:
    result = `<`(a,b) or `==`(a,b)

doOp2NewAnyResult(`+`)
doOp2NewAnyResult(`-`)
doOp2NewAnyResult(`*`)
doOp2NewFloatResult(`/`)

doOp2UpdateAnyResult(`+=`)
doOp2UpdateAnyResult(`-=`)
doOp2UpdateAnyResult(`*=`)
doOp2UpdateFloatResult(`/=`)

doFunc1NewAnyResult(abs)
doFunc1NewFloatResult(arccos)
doFunc1NewFloatResult(arcsin)
doFunc2NewFloatResult(arctan2)
doFunc1NewFloatResult(arctan)
proc binom*(a, b: gmObj): int = binom(a.toInt, b.toInt)
doFunc1NewFloatResult(cbrt)
doFunc1NewFloatResult(ceil)
proc classify*(x: gmObj): FloatClass = classify(x.val)
doFunc1NewFloatResult(cosh)
doFunc1NewFloatResult(cos)
proc countBits32*(x: gmObj): int = countBits32(int32(x.toInt))
doFunc1NewFloatResult(degToRad)
proc `div`*(a, b: gmObj): gmObj = toGM(`div`(a.toInt, b.toInt))
proc `div`*(a: gmObj, b: int): gmObj = toGM(`div`(a.toInt, b))
proc `div`*(a: gmObj, b: float): gmObj = toGM(`div`(a.toInt, b.toInt))
proc `div`*(a: gmObj, s: string): gmObj =
  var b = toGM(s)
  #result = toGM(`div`(a.toInt, b.toInt))
  result = `div`(a, b)
doFunc1NewFloatResult(`div`)
doFunc1NewFloatResult(erfc)
doFunc1NewFloatResult(erf)
doFunc1NewFloatResult(exp)
proc fac*(x: gmObj): int = fac(x.toInt)
doFunc1NewFloatResult(floor)
doFunc2NewFloatResult(fmod)
proc frexp*(x: gmObj, exponent: var int): float = frexp(x.val, exponent)

proc gcd*(a, b: gmObj): gmObj =
  ## Computes the greatest common divisor of ``x`` and ``y``.
  ## Note that for floats, the result cannot always be interpreted as
  ## "greatest decimal `z` such that ``z*N == x and z*M == y``
  ## where N and M are positive integers."
  var
    x = a.toInt
    y = b.toInt
  while y != 0:
    x = x mod y
    swap x, y
  result = toGM(abs(x))
doFunc2NewFloatResult(hypot)
proc isPowerOfTwo*(x: gmObj): bool = isPowerOfTwo(x.toInt)

proc lcm*[T: gmObj](x, y: T): gmObj =
  ## Computes the least common multiple of ``x`` and ``y``.
  result = toGM(`div`(x.toInt,  toInt(gcd[float](x.val, y.val))) * y.toInt)

doFunc1NewFloatResult(lgamma)
doFunc1NewFloatResult(ln)
doFunc1NewFloatResult(log10)
doFunc1NewFloatResult(log2)
doFunc2NewFloatResult(`mod`)
proc nextPowerOfTwo*(x: gmObj): int = nextPowerOfTwo(x.toInt)
doFunc2NewFloatResult(pow)
doFunc1NewFloatResult(radToDeg)

proc round0(x: float32): float32 {.importc: "roundf", header: "<math.h>".}
proc round0(x: float64): float64 {.importc: "round", header: "<math.h>".}

doFunc1NewAnyResult(round0)
proc round*(x: gmObj, places: int = 0): float = round(x.val, places)
doFunc1NewFloatResult(sinh)
doFunc1NewFloatResult(sin)
proc splitDecimal*(x: gmObj): tuple[intpart, floatpart: float] = splitDecimal(x.val)
doFunc1NewFloatResult(sqrt)
doFunc1NewFloatResult(tanh)
doFunc1NewFloatResult(tan)
doFunc1NewFloatResult(tgamma)
doFunc1NewFloatResult(trunc)
#proc `^`*(a, b: gmObj): gmObj = pow(a,b)

proc `==~`(x, y: gmObj; prec: float = 1e-9): bool =
  ## Inexact comparison of float values
  ## return true if absolute difference is < 1e-9
  result = (abs(x.val - y.val) < prec)

############# TESTING ################
when isMainModule:
  var
    a, b, c: gmObj
  a = 2.toGM
  assert(a.isInt == true)
  assert(a.val == 2.0)
  assert(a.val.int == 2)

  b = "222".toGM
  assert("222".parseFloat == b.val)
  assert(b.val == 222.0)
  assert(b.isInt == true)

  block:   # test: + - * /
    c = b / a
    assert( c.isInt == false)
    assert(c.val == 222/2)
    assert($c == "111.0")
    c = a + b
    assert(c.isInt == true)
    assert(c.val == (2+222).float)
    assert($c == "224")
    c = a - b
    assert(c.isInt == true)
    assert(c.val == (2-222).float)
    assert($c == "-220")
    c = b * a
    assert(c.isInt == true)
    assert(c.val == 222*2)
    assert($c == "444")

    assert(b / "2" == toGM(111.0))
    assert(b / "2.0" == toGM(111.0))
    assert(b / 2 == toGM(111.0))
    assert(b / 2.0  == toGM(111.0))
    assert(b * "2" == toGM(444))
    assert(b * "2.0" == toGM(444.0))
    assert(b * 2 == toGM(444))
    assert(b * 2.0  == toGM(444.0))

  block:  # # test:  += -= *= /=
    b += a
    assert(b.isInt == true)
    assert(b.val == (224).float)
    assert($b == "224")
    b -= a
    assert(b.isInt == true)
    assert(b.val == (222).float)
    assert($b == "222")
    b *= a
    assert(b.isInt == true)
    assert(b.val == (444).float)
    assert($b == "444")
    b /= a
    assert(b.isInt == false)
    assert(b.val == (222).float)
    assert($b == "222.0")

    c = b
    assert(c == b)
    b /= "2"
    assert(b == toGM(111.0))
    b = c
    b /= "2.0"
    assert(b == toGM(111.0))
    b = c
    b /= 2
    assert(b == toGM(111.0))
    b = c
    b /= 2.0
    assert(b == toGM(111.0))
    b = c
    b *= "2"
    assert(b == toGM(444))
    b = c
    b *= "2.0"
    assert(b == toGM(444.0))
    b = c
    b *= 2
    assert(b == toGM(444))
    b = c
    b *= 2.0
    assert(b == toGM(444.0))

  block:  # test:  == != < > <= >=
    assert(a != b)
    assert(c != b)
    assert(c == c)
    assert(a < b)
    assert(b > a)
    assert(a <= b)
    assert(b >= a)
    assert(toGM(2) >= toGM("2"))
    assert(toGM(2.0) >= toGM("2.0"))
    assert(toGM(2) <= toGM("2"))
    assert(toGM(2.0) <= toGM("2.0"))
    assert(toGM(2.0) == toGM("2.0"))
    assert(toGM(2.12345678) == toGM("2.12345678"))
    assert(toGM(1/3) !=  toGM("0.3333333333"))
    assert(toGM(1/3) ==~  toGM("0.333333333"))

  block:  # test: invalid string conversions
    assert(toGM("wow") == 0.0)
    assert(toGM(2) + toGM("oops") == 2.0)
    b = toGM(2) + toGM("oops")
    assert(b == 2)
    b = toGM("oops")
    assert(b == 0.0)
    b = toGM(2)
    assert(b / toGM("undefined") == 2.0/0.0)

  block:   # test: math functions from system math lib
    b = toGM(-0.2)
    c = b
    assert(abs(b) == abs(-0.2))
    assert(arccos(b) == arccos(-0.2))
    assert(arcsin(b) == arcsin(-0.2))
    assert(arctan2(b,c) == arctan2(-0.2,-0.2))
    assert(arctan(b) == arctan(-0.2))
    assert(binom(b,c) == binom(0,0))
    assert(cbrt(b) == cbrt(-0.2))
    assert(ceil(b) == ceil(-0.2))
    assert(classify(b) == classify(-0.2))
    assert(cosh(b) == cosh(-0.2))
    assert(cos(b) == cos(-0.2))
    assert(countBits32(b) == countBits32(0))
    assert(degToRad(b) == degToRad(-0.2))
    assert(erfc(b) == erfc(-0.2))
    assert(erf(b) == erf(-0.2))
    assert(exp(b) == exp(-0.2))
    assert(fac(b) == fac(0))
    assert(floor(b) == floor(-0.2))
    assert(fmod(b,c) == fmod(-0.2,-0.2))
    var
      d = 0
      e = 0
    assert(frexp(b,d) == frexp(-0.2,e))
    assert(gcd(b,c) == gcd(0,0))
    assert(hypot(b,c) == hypot(-0.2,-0.2))
    assert(isPowerOfTwo(b) == isPowerOfTwo(0))
    b = toGM(2)
    c = toGM(3)
    d = 2
    e = 3
    assert(lcm(b,c) == lcm(d, e))
    b = toGM(-0.2)
    assert(lgamma(b) == lgamma(-0.2))
    assert(ln(abs(b)) == ln(0.2))
    assert(log10(abs(b)) == log10(0.2))
    assert(log2(abs(b)) == log2(0.2))

    b = toGM(2)
    c = toGM(3)
    d = 2
    e = 3
    assert(`mod`(b,c) == `mod`(d,e))
    assert(b mod c == d mod e)
    assert(nextPowerOfTwo(b) == nextPowerOfTwo(d))
    assert(pow(b,c) == pow(2.0,3.0))
    assert(radToDeg(b) == radToDeg(2.0))
    b = toGM(2.34567)
    assert(round0(b) == 2.0)
    assert(round0(b * -1) == -2)
    assert(round(b,3) == 2.346)

    b = toGM(-0.2)
    assert(sinh(b) == sinh(-0.2))
    assert(sin(b) == sin(-0.2))
    assert(tanh(b) == tanh(-0.2))
    assert(tan(b) == tan(-0.2))
    assert(tgamma(b) == tgamma(-0.2))
    assert(trunc(b) == trunc(-0.2))

    assert(sqrt(toGM(43)) == sqrt(43.0))
    assert(splitDecimal(toGM(43.21)) == splitDecimal(43.21))
    #assert((b ^ c) == (d ^ e))
