import std/json
import std/math
import std/options
import std/os
import std/parsejson
import std/parseutils
import std/sequtils
import std/streams
import std/strformat
import std/strutils
import std/sugar
import std/times
import std/unicode
import unittest
import nim_attoparsec

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
    var parts: seq[string]
    for rune in tok.strVal.runes():
      let rv = cast[int32](rune)
      if rv >= 32 and rv < 128 and rv != '"'.ord:
        parts &= $rune
      else:
        parts &= fmt"\({cast[int32](rune)})"
    return '"' & parts.join("") & '"'
  of tkInt:
    return $tok.intVal
  of tkFloat:
    return $tok.floatVal

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
let parseFracRep1 = charp('.') &> takeWhile1(Digits)
let parseFracRep2 = charp({'e', 'E'}) &> optional(stringp("-") | stringp("+"), "") &> takeWhile1(Digits)
# 4 cases, first 3 are floats: 1.23e4, 1.23, 123e4, 123
let parseFracRep = (parseFracRep1 &> optional(parseFracRep2, "")) | parseFracRep2
let parseFloatRep = parseIntRep &> parseFracRep
let parseFloat = (parseFloatRep <* expectJsonStop).tryMap(proc (s: string): Result[float64] =
  var f: float64
  let took = parseBiggestFloat(s, f)
  if took < s.len:
    return Err[float64](fmt"Invalid float value {s}")
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
        return if st[0] > 0: Err[string]("Unicode escape ended early") else: Ok($Rune(st[1]))
    )
  else:
    return failp[string](fmt"(\{c} is not a valid escape)")
)
let parseString = recursiveParser(proc(self: Parser[string]): Parser[string] =
  takeTill({'"', '\\'}) &> orElse(
    charp('"') >> constp(""),
    parseEscape &> self
  )
)
let parseToken = skipSpace >> peekCharP.andThen(proc (mbc: Option[char]): Parser[Token] =
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
    return parseFloat.map(newJsonFloat) | parseInt.map(newJsonInt)
)

let tokenizer = manyTill(parseToken <* skipSpace, endOfInput)

proc fromRight[T](res: Result[T]): T =
  case res.kind
  of rkindErr:
    raise newException(ValueError, res.msg)
  of rkindOk:
    return res.val

proc jsonToLisp(inp: string): string =
  let tokens: seq[Token] = fromRight(tokenizer.parse(inp, debug=true))
  return mapIt(tokens, $it).join("")

# random valid JSON values from nst/JSONTestSuite

test "can handle arrays_with_spaces":
  check jsonToLisp("[[]   ]") == "'('())"

test "can handle object_extreme_numbers":
  check jsonToLisp("""{ "min": -1.0e+28, "max": 1.0e+28 }""") == """(hash "min" -1e+28 "max" 1e+28)"""

test "can handle string_comments":
  check jsonToLisp("""["a/*b*/c/*d//e"]""") == """'("a/*b*/c/*d//e")"""

test "can handle string_backslash_doublequotes":
  check jsonToLisp("""["\""]""") == """'("\(34)")"""

test "can handle string_nbsp_uescaped":
  check jsonToLisp("""["new\u00A0line"]""") == """'("new\(160)line")"""

test "can handle object_duplicated_key_and_value":
  check jsonToLisp("""{"a":"b","a":"b"}""") == """(hash "a" "b" "a" "b")"""

test "can handle string_accepted_surrogate_pair":
  check jsonToLisp("""["\uD801\udc37"]""") == """'("\(55297)\(56375)")"""

test "can handle array_with_trailing_space":
  check jsonToLisp("[2] ") == "'(2)"

test "can handle number_negative_zero":
  check jsonToLisp("[-0]") == "'(0)"

test "can handle number_after_space":
  check jsonToLisp("[ 4]") == "'(4)"

test "can handle number_simple_real":
  check jsonToLisp("[123.456789]") == "'(123.456789)"

# random invalid values from nst/JSONTestSuite
# todo assert position or name of failing parser?

test "can reject number_1_000":
  let res = tokenizer.parse("[1 000.0]", debug=true)
  check res.kind == rkindErr

test "can reject number_with_alpha_char":
  let res = tokenizer.parse("[1.8011670033376514H-308]", debug=true)
  check res.kind == rkindErr

test "can reject number_with_leading_zero":
  let res = tokenizer.parse("[012]", debug=true)
  check res.kind == rkindErr

test "can reject number_-2.":
  let res = tokenizer.parse("[-2.]", debug=true)
  check res.kind == rkindErr

test "can reject number_infinity":
  let res = tokenizer.parse("[Infinity]", debug=true)
  check res.kind == rkindErr

test "can reject string_1_surrogate_then_escape_u":
  let res = tokenizer.parse("""["\uD800\u"]""", debug=true)
  check res.kind == rkindErr

test "can reject structure_uescaped_LF_before_string":
  let res = tokenizer.parse("""[\u000A""]""", debug=true)
  check res.kind == rkindErr

test "can reject object_trailing_comment_slash_open":
  let res = tokenizer.parse("""{"a":"b"}//""", debug=true)
  check res.kind == rkindErr

template benchmark(benchmarkName: string, iters: Natural,
                   scales: openArray[(string, float)], code: untyped) =
  block:
    let t0 = cpuTime()
    var currIter = 1
    while currIter <= iters:
      code
      currIter += 1
    let rate = float(iters) / (cpuTime() - t0)
    let rateStr = formatFloat(rate, format = ffDecimal, precision = 6)
    echo benchmarkName, ": ", iters, " iters"
    echo "        ", rateStr, " it/s"
    for (desc, v) in scales:
      let scaledStr = formatFloat(v * rate, format = ffDecimal, precision = 6)
      echo "        ", scaledStr, " ", desc, "/s"

test "speed test":
  for fname in walkFiles("tests/json/*.json"):
    let strm = openFileStream(fname, fmRead)
    let text = strm.readAll()
    strm.close()
    let iters = pow(10.0, round(7 - log10(float(text.len)))).int
    let mb = float(text.len) / 1024.0 / 1024.0
    benchmark(fname.splitFile().name & ".json", iters, [("MB", mb)]):
      assert tokenizer.parse(text).kind == rkindOk
    benchmark("stdlib", iters, [("MB", mb)]):
      discard parseJson(text)
