package ru.spbau.maxim.parser

trait Parser[U] {
  def parse(s: String): Option[(U, String)]

  def ~[V](other: => Parser[V]): Parser[(U, V)] = {
      (s: String) => {
        for {
          (u, s1) <- parse(s)
          (v, s2) <- other.parse(s1)
        } yield ((u, v), s2)
      }
  }

  def |(other: => Parser[U]): Parser[U] = {
    val this_ = this
    (s: String) => {
      this_.parse(s) match {
        case Some(p) => Some(p)
        case _ => other.parse(s)
      }
    }
  }

  def ^^[V](f: U => V): Parser[V] = {
    val this_ = this
    (s: String) => {
      for {
        (u, s1) <- this_.parse(s)
      } yield (f(u), s1)
    }
  }

  def ~>[V](other: => Parser[V]): Parser[V] = (this ~ other) ^^ (_._2)

  def <~[V](other: => Parser[V]): Parser[U] = (this ~ other) ^^ (_._1)

  def takeWhile: Parser[List[U]] = ((this ~ takeWhile) ^^ (p => p._1 :: p._2)) | ConstVal(List[U]())
}

object EndString extends Parser[Any] {
  override def parse(s: String): Option[(Unit, String)] = {
    s match {
      case "" => Some(((),""))
      case _ => None
    }
  }
}

case class ConstString(prefix: String) extends Parser[String] {
  override def parse(s: String): Option[(String, String)] = {
    if (s.startsWith(prefix)) {
      Some((prefix, s.substring(prefix.length, s.length)))
    } else {
      None
    }
  }
}

case class ConstVal[U](u: U) extends Parser[U] {
  override def parse(s: String): Option[(U, String)] = Some((u, s))
}

object IntNumber extends Parser[Int] {
  override def parse(s: String): Option[(Int, String)] = {
    for {
      res <- "^(-|)[0-9]+".r.findFirstIn(s)
    } yield (res.toInt, s.substring(res.length(), s.length()))
  }
}

object DoubleNumber extends Parser[Double] {
  override def parse(s: String): Option[(Double, String)] = {
    for {
      res <- "^(-|)([0-9]+\\.[0-9]+|[0-9]+)+".r.findFirstIn(s)
    } yield (res.toDouble, s.substring(res.length(), s.length()))
  }
}