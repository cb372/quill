package io.getquill

import io.getquill.ast._
import io.getquill.idiom.Idiom
import io.getquill.idiom.Statement
import io.getquill.idiom.StatementInterpolator._
import io.getquill.norm.Normalize
import io.getquill.util.Interleave

object MirrorIdiom extends MirrorIdiom

class MirrorIdiom extends Idiom {

  override def prepareForProbing(string: String) = string

  override def liftingPlaceholder(index: Int): String = "?"

  override def emptyQuery = ""

  override def translate(ast: Ast)(implicit naming: NamingStrategy): (Ast, Statement) = {
    val normalizedAst = Normalize(ast)
    (normalizedAst, stmt"${normalizedAst.token}")
  }

  implicit val astTokenizer: Tokenizer[Ast] = Tokenizer[Ast] {
    case ast: Query           => ast.token
    case ast: Function        => ast.token
    case ast: Value           => ast.token
    case ast: Operation       => ast.token
    case ast: Action          => ast.token
    case ast: Ident           => ast.token
    case ast: Property        => ast.token
    case ast: Infix           => ast.token
    case ast: OptionOperation => ast.token
    case ast: Dynamic         => ast.token
    case ast: If              => ast.token
    case ast: Block           => ast.token
    case ast: Val             => ast.token
    case ast: Ordering        => ast.token
    case ast: QuotedReference => ast.ast.token
    case ast: Lift            => stmt"?"
    case ast: Assignment      => ast.token
  }

  implicit val ifTokenizer: Tokenizer[If] = Tokenizer[If] {
    case If(a, b, c) => stmt"if(${a.token}) ${b.token} else ${c.token}"
  }

  implicit val dynamicTokenizer: Tokenizer[Dynamic] = Tokenizer[Dynamic] {
    case Dynamic(tree) => stmt"${tree.toString.token}"
  }

  implicit val blockTokenizer: Tokenizer[Block] = Tokenizer[Block] {
    case Block(statements) => stmt"{ ${statements.map(_.token).mkStmt("; ")} }"
  }

  implicit val valTokenizer: Tokenizer[Val] = Tokenizer[Val] {
    case Val(name, body) => stmt"val ${name.token} = ${body.token}"
  }

  implicit val queryTokenizer: Tokenizer[Query] = Tokenizer[Query] {

    case e: Entity => e.token

    case Filter(source, alias, body) =>
      stmt"${source.token}.filter(${alias.token} => ${body.token})"

    case Map(source, alias, body) =>
      stmt"${source.token}.map(${alias.token} => ${body.token})"

    case FlatMap(source, alias, body) =>
      stmt"${source.token}.flatMap(${alias.token} => ${body.token})"

    case SortBy(source, alias, body, ordering) =>
      stmt"${source.token}.sortBy(${alias.token} => ${body.token})(${ordering.token})"

    case GroupBy(source, alias, body) =>
      stmt"${source.token}.groupBy(${alias.token} => ${body.token})"

    case Aggregation(op, ast) =>
      stmt"${scopedTokenizer(ast)}.${op.token}"

    case Take(source, n) =>
      stmt"${source.token}.take(${n.token})"

    case Drop(source, n) =>
      stmt"${source.token}.drop(${n.token})"

    case Union(a, b) =>
      stmt"${a.token}.union(${b.token})"

    case UnionAll(a, b) =>
      stmt"${a.token}.unionAll(${b.token})"

    case Join(t, a, b, iA, iB, on) =>
      stmt"${a.token}.${t.token}(${b.token}).on((${iA.token}, ${iB.token}) => ${on.token})"

    case Distinct(a) =>
      stmt"${a.token}.distinct"
  }

  implicit val entityTokenizer: Tokenizer[Entity] = Tokenizer[Entity] {
    case SimpleEntity(name) => stmt"query[${name.token}]"
    case c: ConfiguredEntity =>
      val alias = c.alias.map(s => stmt""".entity("${s.token}")""")
      val properties = c.properties.map(p => stmt"""_.${p.property.token} -> "${p.alias.token}"""")
      val columns = if (properties.isEmpty) None else Some(stmt""".columns(${properties.mkStmt()})""")
      val params = alias.toList ::: columns.toList
      stmt"${c.source.token}.schema(_${params.mkStmt("")})"
  }

  implicit val orderingTokenizer: Tokenizer[Ordering] = Tokenizer[Ordering] {
    case TupleOrdering(elems) => stmt"Ord(${elems.token})"
    case Asc                  => stmt"Ord.asc"
    case Desc                 => stmt"Ord.desc"
    case AscNullsFirst        => stmt"Ord.ascNullsFirst"
    case DescNullsFirst       => stmt"Ord.descNullsFirst"
    case AscNullsLast         => stmt"Ord.ascNullsLast"
    case DescNullsLast        => stmt"Ord.descNullsLast"
  }

  implicit val optionOperationTokenizer: Tokenizer[OptionOperation] = Tokenizer[OptionOperation] {
    case q: OptionOperation =>
      val method = q.t match {
        case OptionMap    => "map"
        case OptionForall => "forall"
        case OptionExists => "exists"
      }
      stmt"${q.ast.token}.${method.token}((${q.alias.token}) => ${q.body.token})"
  }

  implicit val joinTypeTokenizer: Tokenizer[JoinType] = Tokenizer[JoinType] {
    case InnerJoin => stmt"join"
    case LeftJoin  => stmt"leftJoin"
    case RightJoin => stmt"rightJoin"
    case FullJoin  => stmt"fullJoin"
  }

  implicit val functionTokenizer: Tokenizer[Function] = Tokenizer[Function] {
    case Function(params, body) => stmt"(${params.token}) => ${body.token}"
  }

  implicit val operationTokenizer: Tokenizer[Operation] = Tokenizer[Operation] {
    case UnaryOperation(op: PrefixUnaryOperator, ast)  => stmt"${op.token}${scopedTokenizer(ast)}"
    case UnaryOperation(op: PostfixUnaryOperator, ast) => stmt"${scopedTokenizer(ast)}.${op.token}"
    case BinaryOperation(a, op, b)                     => stmt"${scopedTokenizer(a)} ${op.token} ${scopedTokenizer(b)}"
    case FunctionApply(function, values)               => stmt"${scopedTokenizer(function)}.apply(${values.token})"
  }

  implicit def operatorTokenizer[T <: Operator]: Tokenizer[T] = Tokenizer[T] {
    case o => stmt"${o.toString.token}"
  }

  implicit val propertyTokenizer: Tokenizer[Property] = Tokenizer[Property] {
    case Property(ref, name) => stmt"${scopedTokenizer(ref)}.${name.token}"
  }

  implicit val valueTokenizer: Tokenizer[Value] = Tokenizer[Value] {
    case Constant(v: String) => stmt""""${v.token}""""
    case Constant(())        => stmt"{}"
    case Constant(v)         => stmt"${v.toString.token}"
    case NullValue           => stmt"null"
    case Tuple(values)       => stmt"(${values.token})"
  }

  implicit val identTokenizer: Tokenizer[Ident] = Tokenizer[Ident] {
    case e => stmt"${e.name.token}"
  }

  implicit val actionTokenizer: Tokenizer[Action] = Tokenizer[Action] {
    case Update(query, assignments)    => stmt"${query.token}.update(${assignments.token})"
    case Insert(query, assignments)    => stmt"${query.token}.insert(${assignments.token})"
    case Delete(query)                 => stmt"${query.token}.delete"
    case Returning(query, alias, body) => stmt"${query.token}.returning((${alias.token}) => ${body.token})"
    case Foreach(query, alias, body)   => stmt"${query.token}.forach((${alias.token}) => ${body.token})"
  }

  implicit val assignmentTokenizer: Tokenizer[Assignment] = Tokenizer[Assignment] {
    case Assignment(ident, property, value) => stmt"${ident.token} => ${property.token} -> ${value.token}"
  }

  implicit val infixTokenizer: Tokenizer[Infix] = Tokenizer[Infix] {
    case Infix(parts, params) =>
      def tokenParam(ast: Ast) =
        ast match {
          case ast: Ident => stmt"$$${ast.token}"
          case other      => stmt"$${${ast.token}}"
        }

      val pt = parts.map(_.token)
      val pr = params.map(tokenParam)
      val body = Statement(Interleave(pt, pr))
      stmt"""infix"${body.token}""""
  }

  private def scopedTokenizer(ast: Ast) =
    ast match {
      case _: Function        => stmt"(${ast.token})"
      case _: BinaryOperation => stmt"(${ast.token})"
      case other              => ast.token
    }
}
