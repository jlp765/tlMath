# tlMath
A typeless Math library (awk-like math, handling int,float and string)
similar to how awk can handle multiple types

This is NOT as fast as handling the types yourself
but is a compromise that makes it easier for
situations like where a DSL has dynamic types
or where you just want an easy solution to
doing maths across multiple types (a lazy
approach :-) )

Behind the scenes, numbers are floats, and integers
are tracked, so that conversion to strings display
integer values as non-floats.
ANY division causes the result to be a float answer

Most math functions "lifted" from the system math library
are float results.
If a proc in the math lib returns a float,
then it will be a float result in genMath lib also.

Conversion of non-numeric strings to a generic number
results in a 0.0 float result, so doing operations
using a non-numeric string is the same as doing that
operation with a zero float value

## Examples
for more examples, view the assertions at the end of the tlMath.nim file

```Nim
  var
    a, b, c: TLObj
    f1 = 2.0
    i1 = 2
  a = 2.toTL
  assert( a.isInt      == true )
  assert( a.val        == 2.0  )
  assert( a.val.int    == 2    )
  assert( a / f1       == 1.0  )
  assert( a / i1.float == 1.0  )
  assert( a / i1       == 1.0  )
  assert( a * f1       == 3.0  )
  assert( a * i1.float == 4.0  )
  assert( a * i1       == 4.0  )
```
