# nim-attoparsec

[Attoparsec](https://github.com/haskell/attoparsec)-like collection of parser
combinators for Nim.

## Example

This is a snippet from the JSON tokenizer in the main source file.

```nim
when isMainModule:
  # ...
  let jsonSpace = {' ', '\t', '\r', '\n'}
  let skipSpace = takeWhile((c) => c in jsonSpace)
  let jsonStops = jsonSpace + {',', ']', '}'}
  # eof counts as a JSON stop too
  let expectJsonStop = peekCharP.validate("expected JSON stop", (mbc) => mbc.get(' ') in jsonStops)

  let parseIntRep = optional(stringp("-"), "") &> orElse(
    stringp("0"),
    charp({'1'..'9'}) &> takeWhile((c) => c in Digits)
  )
  let parseInt = (parseIntRep <* expectJsonStop).map(parseBiggestInt)
```

`parseIntRep` combines a sign parser and magnitude parser with `&>`, meaning its result is
the concatenation of the two. To combine two parsers sequentially, the other quick options are
`>>` (take second result), `<*` (take first result). `parser1.andThen(T -> parser2)` is the more
flexible way, but the `std/sugar` module doesn't seem to work well with it so its clunkier to
write.

`expectJsonStop` is there so the parser recognizes these types of inputs as invalid: `10/3, -99!`.

The map operation makes `parseInt` a `Parser[int64]`.

```nim
  let parseFracRep1 = charp('.') &> takeWhile1((c) => c in Digits)
  let parseFracRep2 = charp({'e', 'E'}) &> optional(stringp("-") | stringp("+"), "") &> takeWhile1((c) => c in Digits)
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
