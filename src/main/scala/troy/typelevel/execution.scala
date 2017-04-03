/*
 * Copyright 2016 Tamer AbdulRadi
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package troy.typelevel

import scala.collection.JavaConverters._
import com.datastax.driver.core._
import shapeless._

trait StatementBinder[GenericInput <: HList, BindMarkerTypes <: HList] {
  protected def bindMarkerCount: Int

  def bind(statement: BoundStatement, input: GenericInput): BoundStatement

  def bind(statement: PreparedStatement, input: GenericInput): BoundStatement =
    bind(statement.bind(), input)
}

object StatementBinder {
  def instance[I <: HList, BM <: HList](bmCount: Int)(f: I => BoundStatement => BoundStatement) = new StatementBinder[I, BM] {
    override def bind(statement: BoundStatement, input: I): BoundStatement = f(input)(statement)
    override protected val bindMarkerCount: Int = bmCount
  }

  implicit def hNilInstance = new StatementBinder[HNil, HNil] {
    override protected val bindMarkerCount: Int = 0
    override def bind(statement: BoundStatement, input: HNil): BoundStatement = statement
  }

  implicit def hConsInstance[IH, IT <: HList, BH <: ColumnType, BT <: HList](
    implicit
    headCodec: TroyCodec[BH, IH],
    tailBinder: StatementBinder[IT, BT]
  ) = new StatementBinder[IH :: IT, BH :: BT] {
    override protected val bindMarkerCount: Int = tailBinder.bindMarkerCount + 1
    override def bind(statement: BoundStatement, input: IH :: IT): BoundStatement = {
      val index = bindMarkerCount - 1
      val tailBoundStatement = tailBinder.bind(statement, input.tail)
      headCodec.set(tailBoundStatement, index, input.head)
    }
  }

}


trait RowParser[GenericOutput <: HList, SelectionTypes <: HList] {
  def parse(row: Row): GenericOutput
}


trait TroyCodec[C <: ColumnType, S] {
  def get(gettable: Row, i: Int): S
  def set(statement: BoundStatement, i: Int, value: S): BoundStatement
}

trait TroyCodecWrapper[C <: ColumnType, S] extends TroyCodec[C, S] {
  def codec: TypeCodec[S]
  override def get(row: Row, i: Int) = row.get(i, codec)
  override def set(statement: BoundStatement, i: Int, value: S) = statement.set(i, value, codec)
}

object TroyCodec {
  def instance[C <: ColumnType, S](getter: (Row, Int) => S, setter: (BoundStatement, Int, S) => BoundStatement) =
    new TroyCodec[C, S] {
      override def get(row: Row, i: Int) = getter(row, i)
      override def set(statement: BoundStatement, i: Int, value: S) = setter(statement, i, value)
    }

  def wrapper[C <: ColumnType, S](inner: TypeCodec[S]) =
    new TroyCodecWrapper[C, S] {
      override def codec: TypeCodec[S] = inner
    }

  implicit val intAsInt = instance[ColumnType.Int, Int](_.getInt(_), _.setInt(_, _))
  implicit val intAsInteger = wrapper[ColumnType.Int, Integer](TypeCodec.cint)
  implicit val textAsString = wrapper[ColumnType.Text, String](TypeCodec.ascii())

  implicit def listAsJList[C <: ColumnType.Native, S](implicit itemCodec: TroyCodecWrapper[C, S]) =
    wrapper[ColumnType.List[C], java.util.List[S]](TypeCodec.list(itemCodec.codec))

  implicit def listAsSeq[C <: ColumnType.Native, S](implicit jCodec: TroyCodecWrapper[ColumnType.List[C], java.util.List[S]]) =
    instance[ColumnType.List[C], Seq[S]](
      (r, i) => jCodec.get(r, i).asScala,
      (s, i, v) => jCodec.set(s, i, v.asJava)
    )

  // workaround Java Generics problems with Primitives
  implicit def listOfIntAsSeqInt(implicit c: TroyCodec[ColumnType.List[ColumnType.Int], Seq[Integer]]) =
    instance[ColumnType.List[ColumnType.Int], Seq[Int]](
      c.get(_, _).map(_.toInt),
      (s, i, v) => c.set(s, i, v.map(Integer.valueOf))
    )
}