############################################################
# tlMath
############################################################
#
# Type-Less math
# or type-independent (string, int, float)
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
  TLMObj* = object
    ## base generic Math object
    val*: float    ## actual value used in calculations
    isInt*: bool   ## track if number is actually an integer

proc `=`*(a: var TLMObj, b: TLMObj) =
  ## `a` is assigned the value of b
  ## keeping track of whether b was an integer value
  a.val = b.val
  a.isInt = b.isInt

proc newTLM*(s: string): TLMObj =
  result.val = 0.0
  result.isInt = false
  try:
    result.val = s.parseFloat
    result.isInt = (not s.contains("."))
  except: discard

proc newTLM(i: int): TLMObj =
  result.val = i.toFloat
  result.isInt = true

proc newTLM(f: float): TLMObj =
  result.val = f
  result.isInt = false

proc toTLM*(s: string): TLMObj = newTLM(s)
  ## return a string as a generic Math object
  ## Non-numeric strings produce a zero float number
proc toTLM*(i: SomeInteger): TLMObj = newTLM(i)
  ## return an integer as a generic Math object
proc toTLM*(f: float): TLMObj = newTLM(f)
  ## return a float as a generic Math object

proc toInt*(g: TLMObj): int =
  ## return  the generic Math object rounded to an integer
  result = g.val.toInt
proc toFloat*(g: TLMObj): float =
  ## return the generic Math object as a float
  result = g.val

proc `$`*[T: TLMObj](a: T): string =
  ## return the string representation of a
  ## (as an integer if a was an integer number,
  ## else as a float representation)
  if a.isInt:
    result = $(a.val.toInt)
  else:
    result = $a.val

template doCmp2NewAnyResult*(Op) =      # Comparison procs
  proc Op*[T: TLMObj](a, b: T): bool =
    if a.isInt and b.isInt:
      result = Op(a.toInt, b.toInt)
    else:
      result = Op(a.val, b.val)
  proc Op*[T: TLMObj](a: T, b: int): bool =
    if a.isInt:
      result = Op(a.toInt, b)
    else:
      result = Op(a.val, b.float)
  proc Op*[T: TLMObj](a: T, b: float): bool =
    result = Op(a.val,  b)
  proc Op*[T: TLMObj](a: T, b: string): bool =
    result = Op(a, toTLM(b))

template doOp2NewAnyResult*(fname) =
  proc fname*[T: TLMObj](a, b: T): T =
    result.val = fname(a.val,  b.val)
    result.isInt = (a.isInt and b.isInt)
  proc fname*[T: TLMObj](a: T, b: int): T =
    result.val = fname(a.val,  b.float)
    result.isInt = a.isInt
  proc fname*[T: TLMObj](a: T, b: float): T =
    result.val = fname(a.val,  b)
    result.isInt = false
  proc fname*[T: TLMObj](a: T, b: string): T =
    result = fname(a, toTLM(b))

template doOp2NewFloatResult*(fname) =
  proc fname*[T: TLMObj](a, b: T): T =
    result.val = fname[float](a.val, b.val)
    result.isInt = false
  proc fname*[T: TLMObj](a: T, b: int): T =
    result.val = fname[float](a.val, b.float)
    result.isInt = false
  proc fname*[T: TLMObj](a: T, b: float): T =
    result.val = fname[float](a.val, b)
    result.isInt = false
  proc fname*[T: TLMObj](a: T, s: string): T =
    let z = toTLM(s)
    result.val = fname(a.val, z.val)
    result.isInt = false

template doOp2UpdateAnyResult*(fname) =
  proc fname*[T: TLMObj](a: var T, b: T) =
    fname(a.val,  b.val)
    a.isInt = (a.isInt and b.isInt)
  proc fname*[T: TLMObj](a: var T, b: int) =
    fname(a.val,  b.float)
  proc fname*[T: TLMObj](a: var T, b: float) =
    fname(a.val,  b)
    a.isInt = false
  proc fname*[T: TLMObj](a: var T, b: string) =
    fname(a, toTLM(b))

template doOp2UpdateFloatResult*(fname) =
  proc fname*[T: TLMObj](a: var T, b: T) =
    fname(a.val,  b.val)
    a.isInt = false
  proc fname*[T: TLMObj](a: var T, b: int) =
    fname(a.val,  b.float)
    a.isInt = false
  proc fname*[T: TLMObj](a: var T, b: float) =
    fname(a.val,  b)
    a.isInt = false
  proc fname*[T: TLMObj](a: var T, s: string) =
    let z = toTLM(s)
    fname(a.val, z.val)
    a.isInt = false

template doFunc1NewAnyResult*(fname) =
  proc fname*[T: TLMObj](a: T): T =
    result = newTLM(fname(a.val))
    result.isInt = a.isInt

template doFunc1NewFloatResult*(fname) =
  proc fname*[T: TLMObj](a: T): T =
    result.val = fname(a.val)
    result.isInt = false

template doFunc2NewAnyResult*(fname) =
  proc fname*[T: TLMObj](a, b: T): T =
    result = newTLM(fname(a.val, b.val))
    result.isInt = (a.isInt and b.isInt)

template doFunc2NewFloatResult*(fname) =
  proc fname*[T: TLMObj](a, b: T): T =
    result.val = fname(a.val, b.val)
    result.isInt = false

doCmp2NewAnyResult(`==`)
doCmp2NewAnyResult(`!=`)
doCmp2NewAnyResult(`>`)
doCmp2NewAnyResult(`<`)
proc `>=`*(a, b: TLMObj): bool =
  if a.isInt and b.isInt:
    result = (a.toInt >= b.toInt)
  else:
    result = `>`(a,b) or `==`(a,b)

proc `<=`*(a, b: TLMObj): bool =
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
proc binom*(a, b: TLMObj): int = binom(a.toInt, b.toInt)
doFunc1NewFloatResult(cbrt)
doFunc1NewFloatResult(ceil)
proc classify*(x: TLMObj): FloatClass = classify(x.val)
doFunc1NewFloatResult(cosh)
doFunc1NewFloatResult(cos)
proc countBits32*(x: TLMObj): int = countBits32(int32(x.toInt))
doFunc1NewFloatResult(degToRad)
proc `div`*(a, b: TLMObj): TLMObj = toTLM(`div`(a.toInt, b.toInt))
proc `div`*(a: TLMObj, b: int): TLMObj = toTLM(`div`(a.toInt, b))
proc `div`*(a: TLMObj, b: float): TLMObj = toTLM(`div`(a.toInt, b.toInt))
proc `div`*(a: TLMObj, s: string): TLMObj =
  var b = toTLM(s)
  result = `div`(a, b)
doFunc1NewFloatResult(`div`)
doFunc1NewFloatResult(erfc)
doFunc1NewFloatResult(erf)
doFunc1NewFloatResult(exp)
proc fac*(x: TLMObj): int = fac(x.toInt)
doFunc1NewFloatResult(floor)
#doFunc2NewFloatResult(fmod)
proc `mod`*(a, b: TLMObj): TLMObj = toTLM(`mod`(a.val, b.val))
proc `mod`*(a: TLMObj, b: int): TLMObj = toTLM(`mod`(a.val, b.float))
proc `mod`*(a: TLMObj, b: float): TLMObj = toTLM(`mod`(a.val, b))
proc `mod`*(a: TLMObj, s: string): TLMObj =
  var b = toTLM(s)
  result = `mod`(a, b)
proc frexp*(x: TLMObj, exponent: var int): float = frexp(x.val, exponent)

proc gcd*(a, b: TLMObj): TLMObj =
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
  result = toTLM(abs(x))
doFunc2NewFloatResult(hypot)
proc isPowerOfTwo*(x: TLMObj): bool = isPowerOfTwo(x.toInt)

proc lcm*[T: TLMObj](x, y: T): TLMObj =
  ## Computes the least common multiple of ``x`` and ``y``.
  result = toTLM(`div`(x.toInt, toInt(gcd(x.toFloat, y.toFloat))) * y.toInt)

doFunc1NewFloatResult(lgamma)
doFunc1NewFloatResult(ln)
doFunc1NewFloatResult(log10)
doFunc1NewFloatResult(log2)
doFunc2NewFloatResult(`mod`)
proc nextPowerOfTwo*(x: TLMObj): int = nextPowerOfTwo(x.toInt)
doFunc2NewFloatResult(pow)
doFunc1NewFloatResult(radToDeg)

proc round0(x: float32): float32 {.importc: "roundf", header: "<math.h>".}
proc round0(x: float64): float64 {.importc: "round", header: "<math.h>".}

doFunc1NewAnyResult(round0)
proc round*(x: TLMObj, places: int = 0): float = round(x.val, places)
doFunc1NewFloatResult(sinh)
doFunc1NewFloatResult(sin)
proc splitDecimal*(x: TLMObj): tuple[intpart, floatpart: float] = splitDecimal(x.val)
doFunc1NewFloatResult(sqrt)
doFunc1NewFloatResult(tanh)
doFunc1NewFloatResult(tan)
doFunc1NewFloatResult(gamma)
doFunc1NewFloatResult(trunc)
#proc `^`*(a, b: TLMObj): TLMObj = pow(a,b)

proc `==~`(x, y: TLMObj; prec: float = 1e-9): bool =
  ## Inexact comparison of float values
  ## return true if absolute difference is < 1e-9
  result = (abs(x.val - y.val) < prec)

############# TESTING ################
when isMainModule:
  var
    a, b, c: TLMObj
    f1 = 2.0
    i1 = 2
  a = 2.toTLM
  assert(a.isInt == true)
  assert(a.val == 2.0)
  assert(a.val.int == 2)
  assert(a / f1 == 1.0)
  assert(a / i1.float == 1.0)
  assert(a / i1 == 1.0)
  assert(a * f1 == 4.0)
  assert(a * i1.float == 4.0)
  assert(a * i1 == 4.0)

  b = "222".toTLM
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

    assert(b / "2" == toTLM(111.0))
    assert(b / "2.0" == toTLM(111.0))
    assert(b / 2 == toTLM(111.0))
    assert(b / 2.0 == toTLM(111.0))
    assert(b * "2" == toTLM(444))
    assert(b * "2.0" == toTLM(444.0))
    assert(b * 2 == toTLM(444))
    assert(b * 2.0 == toTLM(444.0))

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
    assert(b == toTLM(111.0))
    b = c
    b /= "2.0"
    assert(b == toTLM(111.0))
    b = c
    b /= 2
    assert(b == toTLM(111.0))
    b = c
    b /= 2.0
    assert(b == toTLM(111.0))
    b = c
    b *= "2"
    assert(b == toTLM(444))
    b = c
    b *= "2.0"
    assert(b == toTLM(444.0))
    b = c
    b *= 2
    assert(b == toTLM(444))
    b = c
    b *= 2.0
    assert(b == toTLM(444.0))

  block:  # test:  == != < > <= >=
    assert(a != b)
    assert(c != b)
    assert(c == c)
    assert(a < b)
    assert(b > a)
    assert(a <= b)
    assert(b >= a)
    assert(toTLM(2) >= toTLM("2"))
    assert(toTLM(2.0) >= toTLM("2.0"))
    assert(toTLM(2) <= toTLM("2"))
    assert(toTLM(2.0) <= toTLM("2.0"))
    assert(toTLM(2.0) == toTLM("2.0"))
    assert(toTLM(2.12345678) == toTLM("2.12345678"))
    assert(toTLM(1/3) != toTLM("0.3333333333"))
    assert(toTLM(1/3) ==~ toTLM("0.333333333"))

  block:  # test: invalid string conversions
    assert(toTLM("wow") == 0.0)
    assert(toTLM(2) + toTLM("oops") == 2.0)
    b = toTLM(2) + toTLM("oops")
    assert(b == 2)
    b = toTLM("oops")
    assert(b == 0.0)
    b = toTLM(2)
    assert(b / toTLM("undefined") == 2.0/0.0)

  block:   # test: math functions from system math lib
    b = toTLM(-0.2)
    c = b
    var x = b.val
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
    assert(erfc(b) == erfc(x))
    assert(erf(b) == erf(x))
    assert(exp(b) == exp(x))
    assert(fac(b) == fac(0))
    assert(floor(b) == floor(-0.2))
    assert(`mod`(b,c) == `mod`(x,x))
    var
      d = 0
      e = 0
    assert(frexp(b,d) == frexp(-0.2,e))
    assert(gcd(b,c) == gcd(0,0))
    assert(hypot(b,c) == hypot(x,x))
    assert(isPowerOfTwo(b) == isPowerOfTwo(0))
    b = toTLM(2)
    c = toTLM(3)
    d = 2
    e = 3
    assert(lcm(b,c) == lcm(d, e))
    b = toTLM(-0.2)
    assert(lgamma(b) == lgamma(-0.2))
    assert(ln(abs(b)) == ln(0.2))
    assert(log10(abs(b)) == log10(0.2))
    assert(log2(abs(b)) == log2(0.2))

    b = toTLM(2)
    c = toTLM(3)
    d = 2
    e = 3
    assert(`mod`(b,c) == `mod`(d,e))
    assert(b mod c == d mod e)
    assert(nextPowerOfTwo(b) == nextPowerOfTwo(d))
    assert(pow(b,c) == pow(2.0, 3.0))
    assert(radToDeg(b) == radToDeg(2.0))
    b = toTLM(2.34567)
    assert(round0(b) == 2.0)
    assert(round0(b * -1) == -2)
    assert(round(b,3) == 2.346)

    b = toTLM(-0.2)
    x = b.val
    assert(sinh(b) == sinh(x))
    assert(sin(b) == sin(x))
    assert(tanh(b) == tanh(x))
    assert(tan(b) == tan(x))
    assert(gamma(b) == gamma(x))
    assert(trunc(b) == trunc(x))

    assert(sqrt(toTLM(43)) == sqrt(43.0))
    assert(splitDecimal(toTLM(43.21)) == splitDecimal(43.21))
    #assert((b ^ c) == (d ^ e))
