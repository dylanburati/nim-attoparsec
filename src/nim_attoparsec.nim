# nim_attoparsec
# Copyright dylanburati

# forked from nim_parsec
# Copyright xmonader

import std/options
import std/strformat
import std/strutils
import std/sequtils
import std/sugar

type
  ResultKind* = enum
    rkLeft, rkRight
  Result*[T] = ref object
    case kind*: ResultKind
    of rkLeft: msg*: string
    of rkRight: val*: T

proc Ok*[T](val: T): Result[T] =
  return Result[T](kind: rkRight, val: val)

proc Fail*[T](msg: string): Result[T] =
  return Result[T](kind: rkLeft, msg: msg)

proc map*[T, R](this: Result[T], f: T -> R): Result[R] =
  ## Maps a Result[T] to Result[R] by applying a function to a contained
  ## Ok value, leaving an Err value untouched.
  case this.kind
  of rkLeft:
    return Fail[R](this.msg)
  of rkRight:
    return Ok(f(this.val))

proc tryMap*[T, R](this: Result[T], f: T -> Result[R]): Result[R] =
  ## Calls op if the result is Ok, otherwise returns the Err value of this.
  case this.kind
  of rkLeft:
    return Fail[R](this.msg)
  of rkRight:
    return f(this.val)

proc validate*[T](this: Result[T], msg: string, f: T -> bool): Result[T] =
  ## Returns Err(msg) if the result is Ok but the value inside of it does not match
  ## the predicate.
  case this.kind
  of rkLeft:
    return Fail[T](this.msg)
  of rkRight:
    if not f(this.val):
      return Fail[T](msg)
    else:
      return this

proc `$`*[T](this: Result[T]): string =
  case this.kind
  of rkLeft: return fmt"<Left {this.msg}>"
  of rkRight: return fmt"<Right {this.val}>"

proc `==`*[T](this: Result[T], other: Result[T]): bool =
  case this.kind
  of rkLeft:
    return other.kind == rkLeft
  of rkRight:
    return other.kind == rkRight and other.val == this.val

type
  State = tuple
    str: ref string
    pos: int
  Parser*[T] = ref object
    f: State -> (State, Result[T])

proc advance(state: State, n: int): State =
  return (str: state.str, pos: state.pos + n)

proc newParser[T](f: State -> (State, Result[T])): Parser[T] =
  new(result)
  result.f = f

proc `$`*(this: Parser): string =
  return fmt("<Parser:>")

proc runParser*[T](this: Parser[T], s: string, debug: bool = false): (State, Result[T]) =
  var reference: ref[string]
  new(reference)
  reference[] = s
  let state0: State = (reference, 0)
  if debug:
    let (last, res) = this.f(state0)
    if res.kind == rkRight:
      return (last, res)
    else:
      var i = 0
      var line = 1
      var col = -1
      while i >= 0 and i < s.len:
        let i2 = s.find('\n', i)
        if i2 < 0 or last.pos <= i2:
          col = last.pos - i
          break
        i = i2 + 1
        line += 1
      return (last, Fail[T](fmt"line {line}:{col} {res.msg}"))
  return this.f(state0)

proc parse*[T](this: Parser[T], s: string, debug: bool = false): Result[T] =
  ## Run a parser.
  return this.runParser(s, debug)[1]

proc map*[T, R](this: Parser[T], transformer: T -> R): Parser[R] =
  ## Succeeds with `transformer(val)` if the base parser succeeds with `valT`.
  proc inner(s: State): (State, Result[R]) =
    let (nxt, res) = this.f(s)
    return (nxt, res.map(transformer))
  return newParser(inner)

proc tryMap*[T, R](this: Parser[T], transformer: T -> Result[R]): Parser[R] =
  ## Succeeds with `valR` if the base parser succeeds with `valT` and the
  ## transformer returns `Ok(valR)`.
  proc inner(s: State): (State, Result[R]) =
    let (nxt, res) = this.f(s)
    let res2 = res.tryMap(transformer)
    return (nxt, res2)
  return newParser(inner)

proc validate*[T](this: Parser[T], msg: string, check: T -> bool): Parser[T] =
  ## Succeeds if the base parser succeeds and the value it outputs passes a
  ## check.
  proc inner(s: State): (State, Result[T]) =
    let (nxt, res) = this.f(s)
    let res2 = res.validate(msg, check)
    return (nxt, res2)
  return newParser(inner)

proc andThen*[T1, T2](p1: Parser[T1], p2gen: T1 -> Parser[T2]): Parser[T2] =
  ## Composes the first parser with a function that makes a second parser from
  ## the first's output. Like Haskell Monad `>>=`.
  proc inner(s: State): (State, Result[T2]) =
    let (nxt1, res1) = p1.f(s)
    case res1.kind
    of rkLeft:
      return (nxt1, Fail[T2](res1.msg))
    of rkRight:
      let p2 = p2gen(res1.val)
      let (nxt2, res2) = p2.f(nxt1)
      case res2.kind
      of rkLeft:
        return (nxt2, Fail[T2](fmt"andThen[{s.pos},{nxt1.pos}] {res2.msg}"))
      of rkRight:
        return (nxt2, res2)
  return newParser(inner)

proc andAdd*[T](p1: Parser[T], p2: Parser[T]): Parser[T] =
  ## Succeeds with the value `val1 & val2` if the first parser succeeds with
  ## `val1` and the second parser succeeds with `val2`.
  proc gen(parsed1: T): Parser[T] =
    return p2.map((parsed2) => parsed1 & parsed2)
  return p1.andThen(gen)

proc andAdd*(p1: Parser[string], p2: Parser[char]): Parser[string] =
  ## Succeeds with the value `val1 & val2` if the first parser succeeds with
  ## `val1` and the second parser succeeds with `val2`.
  proc gen(parsed1: string): Parser[string] =
    return p2.map((parsed2) => parsed1 & parsed2)
  return p1.andThen(gen)

proc andAdd*(p1: Parser[char], p2: Parser[string]): Parser[string] =
  ## Succeeds with the value `val1 & val2` if the first parser succeeds with
  ## `val1` and the second parser succeeds with `val2`.
  proc gen(parsed1: char): Parser[string] =
    return p2.map((parsed2) => parsed1 & parsed2)
  return p1.andThen(gen)

proc orElse*[T](p1, p2: Parser[T]): Parser[T] =
  ## Succeeds with the value of the first of the two given parsers that
  ## succeeds. Like Haskell Alternative `<|>`.
  proc inner(s: State): (State, Result[T]) =
    let (nxt, res) = p1.f(s)
    case res.kind
    of rkRight:
      return (nxt, res)
    of rkLeft:
      let (nxt, res) = p2.f(s)
      case res.kind
      of rkLeft:
        return (nxt, Fail[T](fmt"orElse {res.msg}"))
      of rkRight:
        return (nxt, res)
  return newParser(inner)

proc `<*`*[T1, T2](this: Parser[T1], rparser: Parser[T2]): Parser[T1] =
  ## Succeeds with the value `val1` if the first parser succeeds with `val1`
  ## and the second parser succeeds with any value. Like Haskell Applicative
  ## `<*`.
  return andThen(this, proc(val: T1): Parser[T1] =
    rparser.map(proc(_: T2): T1 = val)
  )

proc `>>`*[T1, T2](this: Parser[T1], rparser: Parser[T2]): Parser[T2] =
  ## Succeeds with the value `val2` if the first parser succeeds with any value
  ## and the second parser succeeds with `val2`. Like Haskell Applicative `<*`.
  return andThen(this, proc(_: T1): Parser[T2] = rparser)

proc `&>`*[T](this: Parser[T], rparser: Parser[T]): Parser[T] =
  ## Operator form of `andAdd`.
  return andAdd(this, rparser)

proc `&>`*(this: Parser[char], rparser: Parser[string]): Parser[string] =
  ## Operator form of `andAdd`.
  return andAdd(this, rparser)

proc `&>`*(this: Parser[string], rparser: Parser[char]): Parser[string] =
  ## Operator form of `andAdd`.
  return andAdd(this, rparser)

proc `|`*[T](this: Parser[T], rparser: Parser[T]): Parser[T] =
  ## Operator form of `orElse`.
  return orElse(this, rparser)

proc choice*[T](parsers: varargs[Parser[T]]): Parser[T] =
  ## Succeeds with the value of the first of the given parsers that succeeds.
  return foldl(toSeq(parsers), a | b)

proc constp*[T](val: T): Parser[T] =
  ## Succeeds with the given value. Like Haskell Monad `return`.
  proc inner(s: State): (State, Result[T]) =
    return (s, Ok(val))
  return newParser(inner)

proc failp*[T](msg: T): Parser[T] =
  ## Fails with the given value.
  proc inner(s: State): (State, Result[T]) =
    return (s, Fail[T](msg))
  return newParser(inner)

proc endOfInputImpl(s: State): (State, Result[void]) =
  if s.pos >= s.str[].len:
    return (s, Result[void](kind: rkRight))
  else:
    return (s, Fail[void]("endOfInput"))

let endOfInput*: Parser[void] = newParser(endOfInputImpl)
## Succeeds only if all input has been consumed.

proc optional*[T](parser: Parser[T], default: T): Parser[T] =
  ## Succeeds with a fallback if the base parser fails.
  return parser | constp(default)

proc many*[T](parser: Parser[T]): Parser[seq[T]] =
  ## Applies the base parser zero or more times. If the base parser can succeed
  ## without consuming input, the returned parser can infinitely loop.
  proc inner(s: State): (State, Result[seq[T]]) =
    var mys = s
    var fullparsed: seq[T]
    while true:
      let (nxt, res) = parser.f(mys)
      mys = nxt
      case res.kind
      of rkLeft:
        return (mys, Ok(fullparsed))
      of rkRight:
        fullparsed.add(res.val)
  return newParser(inner)

proc count*[T](parser: Parser[T], n: int): Parser[seq[T]] =
  ## Returns a parser which applies the base parser exactly `n` times, and
  ## fails if any of the `n` applications fails.
  proc inner(s: State): (State, Result[seq[T]]) =
    var mys = s
    var fullparsed: seq[T]
    for i in 1 .. n:
      let (nxt, res) = parser.f(mys)
      mys = nxt
      case res.kind
      of rkLeft:
        return (mys, Fail[seq[T]](fmt"count[{s.pos}] {res.msg}"))
      of rkRight:
        fullparsed.add(res.val)
    return (mys, Ok(fullparsed))
  return newParser(inner)

proc many1*[T](parser: Parser[T]): Parser[seq[T]] =
  ## Returns a parser which applies the base parser one or more times.
  return count(parser, 1) &> many(parser)

proc `*`*[T](this: Parser[T], n: int): Parser[seq[T]] =
  ## Operator form of `count`.
  return count(this, n)

proc manyTill*[T, Any](parser: Parser[T], endParser: Parser[Any]): Parser[seq[T]] =
  ## Returns a parser which applies the first parser zero or more times in a
  ## loop which exits when `endParser` succeeds. The `endParser` can consume input
  ## if it succeeds, but its output won't be saved.
  proc inner(s: State): (State, Result[seq[T]]) =
    var mys = s
    var fullparsed: seq[T]
    while true:
      let (post, endRes) = endParser.f(mys)
      if endRes.kind == rkRight:
        return (post, Ok(fullparsed))
      let (nxt, res) = parser.f(mys)
      mys = nxt
      case res.kind
      of rkLeft:
        return (nxt, Fail[seq[T]](fmt"manyTill[{s.pos}] {res.msg}"))
      of rkRight:
        fullparsed.add(res.val)
  return newParser(inner)

proc anyCharImpl(s: State): (State, Result[char]) =
  if s.pos < s.str[].len:
    return (s.advance(1), Ok(s.str[s.pos]))
  else:
    return (s, Fail[char]("anyChar"))

let anyChar*: Parser[char] = newParser(anyCharImpl)
## Matches any character.

proc satisfy*(test: (char) -> bool): Parser[char] =
  ## Matches a character with a predicate.
  proc inner(s: State): (State, Result[char]) =
    if s.pos < s.str[].len and test(s.str[s.pos]):
      return (s.advance(1), Ok(s.str[s.pos]))
    else:
      return (s, Fail[char]("satisfy"))
  return newParser(inner)

proc charp*(c: char): Parser[char] =
  ## Matches a specific character.
  proc inner(s: State): (State, Result[char]) =
    if s.pos < s.str[].len and c == s.str[s.pos]:
      return (s.advance(1), Ok(s.str[s.pos]))
    else:
      return (s, Fail[char]("charp"))
  return newParser(inner)

proc charp*(cs: set[char]): Parser[char] =
  ## Matches any character in the given set.
  proc inner(s: State): (State, Result[char]) =
    if s.pos < s.str[].len and s.str[s.pos] in cs:
      return (s.advance(1), Ok(s.str[s.pos]))
    else:
      return (s, Fail[char]("charp"))
  return newParser(inner)

proc peekImpl(s: State): (State, Result[Option[char]]) =
  if s.pos >= s.str[].len:
    return (s, Ok(none[char]()))
  else:
    return (s, Ok(some(s.str[s.pos])))

let peekCharP*: Parser[Option[char]] = newParser(peekImpl)
## Succeeds with `some(c)` if a character `c` is left in the input. Succeeds
## with none[char]() if there is no more input.

proc stringp*(toMatch: string): Parser[string] =
  ## Matches the given string exactly.
  proc inner(s: State): (State, Result[string]) =
    let matchEnd = s.pos + toMatch.len
    if matchEnd > s.str[].len:
      return (s, Fail[string]("stringp"))
    if s.str[s.pos..<matchEnd] != toMatch:
      return (s, Fail[string]("stringp"))
    return (s.advance(toMatch.len), Ok(s.str[s.pos..<matchEnd]))
  return newParser(inner)

proc take*(n: int): Parser[string] =
  ## Matches the next `n` characters, or fails if there are less than `n`
  ## remaining in the input.
  proc inner(s: State): (State, Result[string]) =
    let maxSize = s.str[].len - s.pos
    if n > maxSize:
      return (s, Fail[string]("stringp"))
    return (s.advance(n), Ok(s.str[s.pos..<s.pos + n]))
  return newParser(inner)

proc takeWhileImpl(test: char -> bool, minSize: int): Parser[string] =
  proc inner(s: State): (State, Result[string]) =
    let maxSize = s.str[].len - s.pos
    for i in 0 ..< maxSize:
      if not test(s.str[s.pos + i]):
        if i < minSize:
          return (s, Fail[string]("takeWhile1"))
        # note: does not consume the char which fails the test
        return (s.advance(i), Ok(s.str[s.pos..<s.pos + i]))
    return (s.advance(maxSize), Ok(s.str[s.pos..^1]))
  return newParser(inner)

proc takeWhile*(test: char -> bool): Parser[string] =
  ## Takes zero or more characters from the input while the predicate succeeds.
  return takeWhileImpl(test, 0)

proc takeWhile1*(test: char -> bool): Parser[string] =
  ## Takes one or more characters from the input while the predicate succeeds.
  return takeWhileImpl(test, 1)

# Not ported yet
# proc sepBy1*(sep: Parser, parser: Parser): Parser =
#   let sep_then_parser = sep >> parser
#   return (parser >> many(sep_then_parser))

# proc sepBy*(sep: Parser, parser: Parser): Parser =
#   let myparsed = @[""]
#   let noneparser = newParser(f=(_) => Result(kind: rkRight, val: (parsed: myparsed, remaining: "")))
#   return sepBy1(sep, parser) | noneparser

# proc sepBy*(sep: Parser, comb: proc(): Parser): Parser =
#   proc curried(s: string): Result =
#     return sepBy(sep, comb()).parse(s)
#   return newParser(f=curried)

proc runScanner*[S](scanner: (S, char) -> Option[S], first: S): Parser[(string, S)] =
  ## A stateful scanner. The parser consumes characters one-by-one and repeatedly
  ## updates the state as long as the function returns a `some()`. This parser does not
  ## fail, and does not consume the character passed to the last invocation. The output
  ## includes the last state.
  proc inner(s: State): (State, Result[(string, S)]) =
    let maxSize = s.str[].len - s.pos
    var scanVal = first
    for i in 0 ..< maxSize:
      let maybeScanVal = scanner(scanVal, s.str[s.pos + i])
      if isNone(maybeScanVal):
        # note: does not consume the char for which scanner returns None
        return (s.advance(i), Ok((s.str[][s.pos..<s.pos + i], scanVal)))
      else:
        scanVal = maybeScanVal.get
    return (s.advance(maxSize), Ok((s.str[][s.pos..^1], scanVal)))
  return newParser(inner)

proc foldWhile*[S](scanner: (S, char) -> Option[S], first: S): Parser[S] =
  ## A stateful scanner. The parser consumes characters one-by-one and repeatedly
  ## updates the state as long as the function returns a `some()`. This parser does not
  ## fail, and does not consume the character passed to the last invocation. The output
  ## is the last state. See also `scan`, `runScanner`.
  return runScanner(scanner, first).map((pair) => pair[1])

proc scan*[S](scanner: (S, char) -> Option[S], first: S): Parser[string] =
  ## A stateful scanner. The parser consumes characters one-by-one and repeatedly
  ## updates the state as long as the function returns a `some()`. This parser does not
  ## fail, and does not consume the character passed to the last invocation. The output
  ## is the consumed input. See also `foldWhile`, `runScanner`.
  return runScanner(scanner, first).map((pair) => pair[1])
