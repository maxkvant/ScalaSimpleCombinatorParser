package ru.spbau.maxim.example
import ru.spbau.maxim.parser._

object ExprParser {
  private[this] def funCall: Parser[Expr] = ((ConstString("exp") | ConstString("log") | ConstString("sin") | ConstString("cos")) ~ (
    ConstString("(") ~> expr <~ ConstString(")"))).^^[Expr] {
      case (name: String, expr: Expr) => FunCall(name, expr)
    }

  private[this] def value: Parser[Expr] = DoubleNumber.^^[Expr](x => Value(x)) | IntNumber.^^[Expr](x => Value(x))

  private[this] def exprSimple: Parser[Expr] =
    funCall.^^[Expr](fun => fun) |
    (ConstString("(") ~> expr <~ ConstString(")")) |
    value.^^[Expr](fun => fun)

  private[this] def multSeq: Parser[Expr] = (exprSimple ~ ((ConstString("*") | ConstString("/")) ~ exprSimple).takeWhile) ^^ {
      case (value, lst)
        => (value /: lst) ((expr: Expr, token) => BinOp(expr, token._1, token._2))
  }

    private[this] def sumSeq: Parser[Expr] = multSeq ~ ((ConstString("+") | ConstString("-")) ~ multSeq).takeWhile ^^ {
      case (value, lst)
        => (value /: lst) ((expr: Expr, token) => BinOp(expr, token._1, token._2))
    }

    private[this] def expr: Parser[Expr] = sumSeq | (ConstString("-") ~> sumSeq).^^[Expr](expr => BinOp(Value(0), "-", expr))

    def parsedString(str: String): Expr = {
      (expr<~EndString).parse(str.replace(" ", "")) match {
        case Some((expr, "")) => expr
        case _ => throw new RuntimeException("parsing failed")
      }
    }

    def evaluate(s: String): Double = parsedString(s).evaluate
  }