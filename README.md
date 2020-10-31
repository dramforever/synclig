# synclig

*Synchronous Logic from Imperative Generators*

*(Work in progress. No usable demo yet.)*

## Example

```scala
val foo = reg("foo")
val bar = reg("bar")

bar := 0.U

whilst(foo):
  bar := bar + 1.U
  skip

loop:
  skip
```
