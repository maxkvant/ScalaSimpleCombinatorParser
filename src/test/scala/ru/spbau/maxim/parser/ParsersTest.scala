package ru.spbau.maxim.parser

import org.scalatest._

class ParsersTest extends FlatSpec with Matchers {
  "EndString" should
    "parse it" in {
    EndString.parse("") should be (Some(((), "")))
    EndString.parse("a") should be (None)
  }

  "ConstString" should
    "parse it" in {
    ConstString("aba").parse("") should be (None)
    ConstString("aba").parse("abacaba") should be (Some(("aba", "caba")))
    ConstString("+").^^(_ => " ++ ").takeWhile.parse("+++a") should be (Some((List(" ++ ", " ++ ", " ++ "), "a")))
    ConstString("+").^^(_ => " ++ ").takeWhile.parse("") should be (Some((List[String](), "")))
  }

  "IntNumber" should
    "parse it" in {
    IntNumber.parse("10") should be (Some((10, "")))
    IntNumber.parse("-10") should be (Some((-10, "")))
    IntNumber.parse("1234567890 zba") should be (Some((1234567890, " zba")))
    IntNumber.parse("-10a") should be (Some((-10, "a")))
    IntNumber.parse("-a") should be (None)
    IntNumber.parse("-1-1") should be (Some((-1, "-1")))
    IntNumber.parse("--1") should be (None)
  }

  "DoubleNumber" should
    "parse it" in {
    DoubleNumber.parse("10") should be (Some((10.0, "")))
    DoubleNumber.parse("-10") should be (Some((-10.0, "")))
    DoubleNumber.parse("12345678.90 zba") should be (Some((12345678.9, " zba")))
    DoubleNumber.parse("-10.5a") should be (Some((-10.5, "a")))
    DoubleNumber.parse("-a") should be (None)
    DoubleNumber.parse("-1-1") should be (Some((-1, "-1")))
    DoubleNumber.parse("--1") should be (None)
  }
}
