package io.getquill.idiom

import io.getquill.ast.ScalarLift

sealed trait Token
case class StringToken(string: String) extends Token
case class LiftingToken(lift: ScalarLift) extends Token
case class Statement(tokens: List[Token]) extends Token
