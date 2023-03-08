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
  ResultKind = enum
    ekLeft, ekRight
  Result[T] = ref object
    case kind*: ResultKind 
    of ekLeft: msg*: string
    of ekRight: val*: T
  
proc Ok[T](val: T): Result[T] =
  return Result[T](kind: ekRight, val: val)

proc Fail[T](msg: string): Result[T] =
  return Result[T](kind: ekLeft, msg: msg)

proc map*[T, R](this: Result[T], f: T -> R): Result[R] =
  ## Maps a Result[T] to Result[R] by applying a function to a contained
  ## Ok value, leaving an Err value untouched.
  case this.kind
  of ekLeft:
    return Fail[R](this.msg)
  of ekRight: 
    return Ok(f(this.val))

proc tryMap*[T, R](this: Result[T], f: T -> Result[R]): Result[R] =
  ## Calls op if the result is Ok, otherwise returns the Err value of this.
  case this.kind
  of ekLeft:
    return Fail[R](this.msg)
  of ekRight: 
    return f(this.val)

proc validate*[T](this: Result[T], msg: string, f: T -> bool): Result[T] =
  ## Returns Err(msg) if the result is Ok but the value inside of it does not match
  ## the predicate.
  case this.kind
  of ekLeft:
    return Fail[T](this.msg)
  of ekRight: 
    if not f(this.val):
      return Fail[T](msg)
    else:
      return this

proc `$`*[T](this: Result[T]): string =
  case this.kind
  of ekLeft: return fmt"<Left {this.msg}>"
  of ekRight: return fmt"<Right {this.val}>"

proc `==`*[T](this: Result[T], other: Result[T]): bool =
  case this.kind
  of ekLeft:
    return other.kind == ekLeft
  of ekRight:
    return other.kind == ekRight and other.val == this.val

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

proc runParser[T](this: Parser[T], s: string): (State, Result[T]) =
  var reference: ref[string]
  new(reference)
  reference[] = s
  let state0: State = (reference, 0)
  return this.f(state0)

proc parse*[T](this: Parser[T], s: string): Result[T] =
  ## Run a parser.
  return this.runParser(s)[1]

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
    of ekLeft:
      return (nxt1, Fail[T2](res1.msg))
    of ekRight:
      let p2 = p2gen(res1.val)
      let (nxt2, res2) = p2.f(nxt1)
      case res2.kind
      of ekLeft:
        return (nxt2, Fail[T2](fmt"andThen[{s.pos},{nxt1.pos}] {res2.msg}"))
      of ekRight:
        return (nxt2, res2)
  return newParser(inner)

proc andAdd*[T](p1: Parser[T], p2: Parser[T]): Parser[T] =
  ## Succeeds with the value `val1 & val2` if the first parser succeeds with
  ## `val1` and the second parser succeeds with `val2`. Like Haskell Semigroup
  ## `<>`.
  proc gen(parsed1: T): Parser[T] =
    return p2.map((parsed2) => parsed1 & parsed2)
  return p1.andThen(gen)

proc andAdd*(p1: Parser[string], p2: Parser[char]): Parser[string] =
  ## Succeeds with the value `val1 & val2` if the first parser succeeds with
  ## `val1` and the second parser succeeds with `val2`. Like Haskell Semigroup
  ## `<>`.
  proc gen(parsed1: string): Parser[string] =
    return p2.map((parsed2) => parsed1 & parsed2)
  return p1.andThen(gen)

proc andAdd*(p1: Parser[char], p2: Parser[string]): Parser[string] =
  ## Succeeds with the value `val1 & val2` if the first parser succeeds with
  ## `val1` and the second parser succeeds with `val2`.  Like Haskell Semigroup
  ## `<>`.
  proc gen(parsed1: char): Parser[string] =
    return p2.map((parsed2) => parsed1 & parsed2)
  return p1.andThen(gen)

proc orElse*[T](p1, p2: Parser[T]): Parser[T] =
  ## Succeeds with the value of the first of the two given parsers that
  ## succeeds. Like Haskell Alternative `<|>`.
  proc inner(s: State): (State, Result[T]) =
    let (nxt, res) = p1.f(s)
    case res.kind
    of ekRight:
      return (nxt, res)
    of ekLeft:
      let (nxt, res) = p2.f(s)
      case res.kind
      of ekLeft:
        return (nxt, Fail[T](fmt"orElse {res.msg}"))
      of ekRight:
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
    return (s, Result[void](kind: ekRight))
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
      of ekLeft:
        return (mys, Ok(fullparsed))
      of ekRight:
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
      of ekLeft:
        return (mys, Fail[seq[T]](fmt"count[{s.pos}] {res.msg}"))
      of ekRight:
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
      if endRes.kind == ekRight:
        return (post, Ok(fullparsed))
      let (nxt, res) = parser.f(mys)
      mys = nxt
      case res.kind
      of ekLeft:
        return (nxt, Fail[seq[T]](fmt"manyTill[{s.pos}] {res.msg}"))
      of ekRight:
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

let peek*: Parser[Option[char]] = newParser(peekImpl)
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
#   let noneparser = newParser(f=(_) => Result(kind: ekRight, val: (parsed: myparsed, remaining: "")))
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

when isMainModule:
  import std/parsejson
  import std/parseutils
  import std/unicode
  type
    Token = object
      case kind: TokKind
      of tkError, tkEof, tkTrue, tkFalse, tkNull, tkCurlyLe, tkCurlyRi, tkBracketLe, tkBracketRi, tkColon, tkComma:
        discard
      of tkString:
        strVal: string
      of tkInt:
        intVal: int64
      of tkFloat:
        floatVal: float64

  proc newJsonString(val: string): Token =
    return Token(kind: tkString, strVal: val)
  proc newJsonInt(val: int64): Token =
    return Token(kind: tkInt, intVal: val)
  proc newJsonFloat(val: float64): Token =
    return Token(kind: tkFloat, floatVal: val)
  proc newJsonTrue(): Token =
    return Token(kind: tkTrue)
  proc newJsonFalse(): Token =
    return Token(kind: tkFalse)
  proc newJsonNull(): Token =
    return Token(kind: tkNull)
  proc newJsonObjectStart(): Token =
    return Token(kind: tkCurlyLe)
  proc newJsonObjectEnd(): Token =
    return Token(kind: tkCurlyRi)
  proc newJsonArrayStart(): Token =
    return Token(kind: tkBracketLe)
  proc newJsonArrayEnd(): Token =
    return Token(kind: tkBracketRi)
  proc newJsonColon(): Token =
    return Token(kind: tkColon)
  proc newJsonComma(): Token =
    return Token(kind: tkComma)

  proc `$`(tok: Token): string =
    case tok.kind
    of tkError:
      raise newException(ValueError, "parsec doesn't use error token")
    of tkEof:
      raise newException(ValueError, "parsec doesn't use eof")
    of tkTrue:
      return "#t"
    of tkFalse:
      return "#f"
    of tkNull:
      return "#nil"
    of tkCurlyLe:
      return "(hash "
    of tkCurlyRi:
      return ")"
    of tkBracketLe:
      return "'("
    of tkBracketRi:
      return ")"
    of tkColon:
      return " "
    of tkComma:
      return " "
    of tkString:
      let runes = cast[seq[int32]](toRunes(tok.strVal))
      if runes.len > 0 and max(runes) > 1237:
        return "(utf8 " & $runes & ")"
      return '"' & tok.strVal & '"'
    of tkInt:
      return $tok.intVal
    of tkFloat:
      return $tok.floatVal

  let jsonSpace = {' ', '\t', '\r', '\n'}
  let skipSpace = takeWhile((c) => c in jsonSpace)
  let jsonStops = jsonSpace + {',', ']', '}'}
  let parseIntRep = optional(stringp("-"), "") &> orElse(
    stringp("0"),
    charp({'1'..'9'}) &> takeWhile((c) => c in Digits)
  )
  # eof counts as a JSON stop too
  let expectJsonStop = peek.validate("expected JSON stop", (mbc) => mbc.get(' ') in jsonStops)
  let parseInt = (parseIntRep <* expectJsonStop).map(parseBiggestInt)
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

  # st: (chars remaining after this, hex value left of this)
  proc fourHexScanner(st: (int, int), c: char): Option[(int, int)] =
    if st[0] <= 0: return none[(int, int)]()
    case c
    of '0'..'9': return some((st[0] - 1, st[1]*16 + c.ord - '0'.ord))
    of 'a'..'f': return some((st[0] - 1, st[1]*16 + 10 + c.ord - 'a'.ord))
    of 'A'..'F': return some((st[0] - 1, st[1]*16 + 10 + c.ord - 'A'.ord))
    else: return none[(int, int)]()
    
  let parseEscape = charp('\\') >> anyChar.andThen(proc (c: char): Parser[string] =
    case c
    of '"', '\\', '/': return constp($c)
    of 'b': return constp("\b")
    of 'f': return constp("\f")
    of 'n': return constp("\n")
    of 'r': return constp("\r")
    of 't': return constp("\t")
    of 'u':
      return foldWhile(fourHexScanner, (4, 0)).tryMap(
        proc (st: (int, int)): Result[string] =
          return if st[0] > 0: Fail[string]("Unicode escape ended early") else: Ok($Rune(st[1]))
      )
    else:
      return failp(fmt"(\{c} is not a valid escape)")
  )
  proc parseStringRecur(): Parser[seq[string]] =
    return takeWhile((c) => c notin {'"', '\\'}).andThen(proc(parsed1: string): Parser[seq[string]] =
      return orElse(
        charp('"') >> constp(@[parsed1]),
        constp(@[parsed1]) &> count(parseEscape, 1) &> parseStringRecur()
      )
    )
  let parseString = parseStringRecur().map(l => l.join(""))
  let parseToken = skipSpace >> peek.andThen(proc (mbc: Option[char]): Parser[Token] =
    if mbc == some('{'):
      return charp('{') >> constp(newJsonObjectStart())
    elif mbc == some('}'):
      return charp('}') >> constp(newJsonObjectEnd())
    elif mbc == some('['):
      return charp('[') >> constp(newJsonArrayStart())
    elif mbc == some(']'):
      return charp(']') >> constp(newJsonArrayEnd())
    elif mbc == some(':'):
      return charp(':') >> constp(newJsonColon())
    elif mbc == some(','):
      return charp(',') >> constp(newJsonComma())
    elif mbc == some('n'):
      return stringp("null") >> constp(newJsonNull())
    elif mbc == some('t'):
      return stringp("true") >> constp(newJsonTrue())
    elif mbc == some('f'):
      return stringp("false") >> constp(newJsonFalse())
    elif mbc == some('"'):
      return charp('"') >> parseString.map(newJsonString)
    else:
      # order is important; could be solved by forcing both to peek and expect one of ",]} "
      return parseFloat.map(newJsonFloat) | parseInt.map(newJsonInt)
  )
  let tokenizer = manyTill(parseToken <* skipSpace, endOfInput)
  proc showParse(inp: string) =
    try:
      echo fmt"input: {inp}"
      let (st, res) = runParser(tokenizer, inp)
      case res.kind
      of ekLeft:
        echo fmt"""Left "{res.msg}" @ {st.pos}"""
      of ekRight:
        echo "Right " & mapIt(res.val, $it).join("")
    except Exception as e:
      echo getStackTrace(e)
  
  import std/os
  import std/streams
  for path in walkFiles("/home/dylan/Downloads/4cs/JSONTestSuite/test_parsing/*.json"):
    if path.splitPath().tail.startsWith("y_"):
      let strm = openFileStream(path, fmRead)
      let text = strm.readAll()
      strm.close()
      echo ""
      echo path
      showParse(text)

let ignored = """

proc smashTransformer(l: seq[string]): seq[string] =
  @[join(l, "")]

let letter = anyOf(strutils.Letters)
let lletter = anyOf({'a'..'z'})
let uletter = anyOf({'A'..'Z'})
let digit = anyOf(strutils.Digits)
let hexdigit, hexstr = anyOf(strutils.HexDigits)
let ws = anyOf(strutils.Whitespace)
let letters = many1(letter)
let digits = many1(digit)
let word = letters.map(smashtransformer)
let alphanumword = many1(letter >> (letters|digits)).map(smashtransformer)
let between = proc(p1, p2, p3: Parser): Parser =
  return p1 >> p2 >> p3
let surroundedBy = proc(surparser, contentparser: Parser): Parser =
  return surparser >> contentparser >> surparser


when isMainModule:
  let aParser = charp('a')
  let bParser = charp('b')
  echo $aParser.parse("abc")
  echo $bParser.parse("bca")

  let abParser = charp('a') >> charp('b')
  echo $abParser.parse("abc")

  let aorbParser = charp('a') | charp('b')
  echo $aorbParser.parse("acd")
  echo $aorbParser.parse("bcd")

  let abcParser = parseString("abc")
  echo $abcParser.parse("abcdef")

  let manyA = many(charp('a'))
  echo $manyA.parse("aaab")
  echo $manyA.parse("bbb")

  let manyA1 = many1(charp('a'))
  echo $manyA1.parse("aaab")
  echo $manyA1.parse("bbb")

  let manyDigits = many1(digit)
  echo $manyDigits.parse("1234")

  let commaseparatednums = sep_by(charp(',').suppress(), digit)
  echo $commaseparatednums.parse("1,2,4")

  #let greetparser = letters >> charp(',') >> many(ws) >> letters
  let greetparser = word >> charp(',').suppress() >> many(ws).suppress() >> word >> optionally(charp('!'))
  echo $greetparser.parse("Hello,   World")
  echo $greetparser.parse("Hello,   World!")


  echo $(letter*3).parse("abc")

  let uuidsample = "db9674c4-72a9-4ab9-9ddd-1d641a37cde4"
  let uuidparser =(hexstr*8).map(smashtransformer) >> charp('-') >> (hexstr*4).map(smashtransformer) >> charp('-') >>  (hexstr*4).map(smashtransformer) >> charp('-') >> (hexstr*4).map(smashtransformer) >> charp('-') >> (hexstr*12).map(smashtransformer)
  echo $uuidparser.parse(uuidsample)
  

  let sur3pipe = surroundedBy(charp('|'), charp('3'))
  echo $sur3pipe.parse("|3|")

  let paren3 = between(charp('('), charp('3'), charp(')') )
  echo paren3.parse("(3)")

  # recursive lang ints and list of ints or lists
  type 
    LangElemKind = enum
      leChr, leList
    LangElem = ref object
      case kind*: LangElemKind 
      of leChr: c*: char
      of leList: l*: seq[LangElem]
  
  proc `$`*(this: LangElem): string =
    case this.kind
    of leChr: return fmt"<Char {this.c}>"
    of leList: return fmt("<List: {this.l}>")

  proc `==`*(this: LangElem, other: LangElem): bool =
    return this.kind == other.kind

  proc parseToNimData(data: seq[string]) : LangElem =
    result = LangElem(kind: leList, l: @[])
    let dataIsList = data[0][0] == '['
    for el in data:
      var firstchr = el[0]
      if firstchr.isAlphaAscii():
        var elem = LangElem(kind: leChr, c: firstchr)
        if dataIsList == false:
          return elem
        else:
          result.l[result.l.len-1].l.add(LangElem(kind: leChr, c: firstchr))

      elif firstchr == '[':
        result.l.add(LangElem(kind: leList, l: @[]))
      

  var listp: Parser
  var valref = (proc(): Parser =letters|listp)

  listp = charp('[') >> sep_by(charp(',').suppress(), many(valref)) >> charp(']')
  var valp = valref()

  var inps = @["a", "[a,b]", "[a,[b,c]]"]
  for inp in inps:
    echo &"inp : {inp}"
    let parsed = valp.parse(inp)
    if parsed.kind == ekRight:
      let data = parsed.val.parsed
      echo "parsed data: " & data
      echo inp, " => ", $parseToNimData(data)
"""
