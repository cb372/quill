package io.getquill.context

import scala.reflect.macros.whitebox.{ Context => MacroContext }
import io.getquill.ast.Ident
import io.getquill.norm.select.SelectResultExtraction
import io.getquill.ast._
import io.getquill.quotation.ReifyLiftings
import io.getquill.util.Messages._
import io.getquill.norm.BetaReduction

class ActionMacro(val c: MacroContext)
  extends ContextMacro
  with EncodingMacro
  with SelectResultExtraction
  with ReifyLiftings {
  import c.universe.{ Ident => _, Function => _, _ }

  def runAction(quoted: Tree): Tree =
    q"""
      val expanded = ${expand(extractAst(quoted))}
      ${c.prefix}.executeAction(
        expanded.string,
        expanded.bind
      )
    """

  def runActionReturning[T](quoted: Tree)(implicit t: WeakTypeTag[T]): Tree =
    q"""
      val expanded = ${expand(extractAst(quoted))}
      ${c.prefix}.executeActionReturning(
        expanded.string,
        expanded.bind,
        ${returningExtractor(t.tpe)},
        expanded.returningColumn.get
      )
    """

  def runBatchAction(quoted: Tree): Tree =
    expandBatchAction(quoted) {
      case (batchItemType, batch, param, expanded) =>
        q"""
          ${c.prefix}.executeBatchAction[$batchItemType](
            $batch,
            $param => {
              val expanded = $expanded
              (expanded.string, expanded.bind)
            }
          )
        """
    }

  def runBatchActionReturning[T](quoted: Tree)(implicit t: WeakTypeTag[T]): Tree =
    expandBatchAction(quoted) {
      case (batchItemType, batch, param, expanded) =>
        q"""
          ${c.prefix}.executeBatchActionReturning[$batchItemType, $t](
            $batch,
            $param => {
              val expanded = $expanded
              (expanded.string, expanded.bind, expanded.returningColumn.get)
            },
            ${returningExtractor(t.tpe)}
          )
        """
    }

  def expandBatchAction(quoted: Tree)(call: (Type, Tree, Tree, Tree) => Tree): Tree =
    BetaReduction(extractAst(quoted)) match {
      case ast @ Foreach(lift: Lift, alias, body) =>
        val batch = lift.value.asInstanceOf[Tree]
        val batchItemType = batch.tpe.typeArgs.head
        c.typecheck(q"(value: $batchItemType) => value") match {
          case q"($param) => $value" =>
            val nestedLift =
              lift match {
                case ScalarBatchLift(name, batch: Tree, encoder: Tree) =>
                  ScalarLift("value", value, encoder)
                case CaseClassBatchLift(name, batch: Tree) =>
                  CaseClassLift("value", value)
              }
            val (ast, _) = reifyLiftings(BetaReduction(body, alias -> nestedLift))
            c.untypecheck {
              call(batchItemType, batch, param, expand(ast))
            }
        }
      case other =>
        c.fail(s"Batch actions must be static quotations. Found: '$other'")
    }

  private def returningExtractor(returnType: c.Type) = {
    val selectValues = encoding(Ident("X"), returnType, Encoding.inferDecoder(c))
    selectResultExtractor(selectValues)
  }
}
