# nim-attoparsec

[Attoparsec](https://github.com/haskell/attoparsec)-like collection of parser
combinators for Nim.

## Example

This is a snippet from the JSON tokenizer in the tests/ folder.

```nim
let jsonSpace = {' ', '\t', '\r', '\n'}
let skipSpace = takeWhile(jsonSpace)
let jsonStops = jsonSpace + {',', ']', '}'}
# eof counts as a JSON stop too
let expectJsonStop = peekCharP.validate("expected JSON stop", (mbc) => mbc.get(' ') in jsonStops)

let parseIntRep = optional(stringp("-"), "") &> orElse(
  stringp("0"),
  charp({'1'..'9'}) &> takeWhile(Digits)
)
let parseInt = (parseIntRep <* expectJsonStop).map(parseBiggestInt)
```

`parseIntRep` combines a sign parser and magnitude parser with `&>`, meaning its result is
the concatenation of the two. To combine two parsers sequentially, the other quick options are
`>>` (take second result), `<*` (take first result). `parser1.andCombine(parser2, (T1,T2) -> R)` is a more flexible form.

`expectJsonStop` is there so the parser recognizes these types of inputs as invalid: `10/3, -99!`.

The map operation makes `parseInt` a `Parser[int64]`.

```nim
let parseFracRep1 = charp('.') &> takeWhile1(Digits)
let parseFracRep2 = charp({'e', 'E'}) &> optional(stringp("-") | stringp("+"), "") &> takeWhile1(Digits)
# 4 cases, first 3 are floats: 1.23e4, 1.23, 123e4, 123
let parseFracRep = (parseFracRep1 &> optional(parseFracRep2, "")) | parseFracRep2
let parseFloatRep = parseIntRep &> parseFracRep
let parseFloat = (parseFloatRep <* expectJsonStop).tryMap(proc (s: string): Result[float64] =
  var f: float64
  let took = parseBiggestFloat(s, f)
  if took < s.len:
    return Fail[float64](fmt"Invalid float value {s}")
  return Ok(f)
)
```

Parsing floats is similar but slightly more complex. The `|` operator does the same thing
as `orElse` from the `parseIntRep` - on the commented line, it's applied to a complex
pattern on the left-hand side and not just a `stringp`. This still works without any issues,
because `orElse` always gives the second branch the same input as the first, regardless
of how much the first consumes before failing.

The map operation makes `parseFloat` a `Parser[float64]`, which means `parseInt | parseFloat`
is not valid code; however, they can be mapped to variants of the same sum type/tagged union,
and then combined with an `orElse`.

## Performance

It's about 5-6x slower than the standard library for JSON. It speeds up with `--mm:arc`.
My guess is that the bottleneck has something to do with the Result[T] allocation, because
[nom](https://github.com/rust-bakery/nom) for Rust does a lot better with a
similar design.

```
apache_builds.json: 100 iters
        171.559984 it/s
        20.823926 MB/s
stdlib: 100 iters
        1016.475306 it/s
        123.379623 MB/s
canada.json: 10 iters
        5.004890 it/s
        10.744346 MB/s
stdlib: 10 iters
        24.519834 it/s
        52.638432 MB/s
data.json: 1000 iters
        2156.740625 it/s
        19.025660 MB/s
stdlib: 1000 iters
        12508.025931 it/s
        110.339393 MB/s
```
