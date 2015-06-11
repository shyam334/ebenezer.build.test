//   Copyright 2014 Commonwealth Bank of Australia
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

package au.com.cba.omnia.ebenezer.cli

import java.util.UUID

import argonaut._, Argonaut._

import com.twitter.scalding.typed.IterablePipe
import com.twitter.scrooge.ThriftStruct

import org.scalacheck.Arbitrary

import org.specs2.execute.Result

import au.com.cba.omnia.ebenezer.ParquetLogging
import au.com.cba.omnia.ebenezer.cli.CatAsJson._
import au.com.cba.omnia.ebenezer.introspect._
import au.com.cba.omnia.ebenezer.scrooge.ParquetScroogeSource
import au.com.cba.omnia.ebenezer.test._, ThriftArbitraries._ , JavaArbitraries._

import au.com.cba.omnia.thermometer.context.Context
import au.com.cba.omnia.thermometer.core.Thermometer._
import au.com.cba.omnia.thermometer.core._

object CatAsJsonSpec extends ThermometerSpec with ParquetLogging { def is = s2"""

CatAsJson usage
================
  Read some parquet as records and cat it as JSON                 $usage

  Read some parquet as records, extract fields and cat as a flattened JSON $checkFlattenedFields

Types
======
  Read booleans                                                   ${check("boolean", fromBoolish)}
  Read doubles                                                    ${check("double", fromDoublish)}
  Read bytes                                                      ${check("byte", fromBytish)}
  Read shorts                                                     ${check("short", fromShortish)}
  Read ints                                                       ${check("int", fromIntish)}
  Read longs                                                      ${check("long", fromLongish)}
  Read strings                                                    ${check("string", fromStringish)}
  Read nested                                                     ${check("nested", fromNestedish)}
  Read lists                                                      ${check("list", fromListish)}
  Read maps                                                       ${check("map", fromMapish)}
  Read maps with numeric key                                      ${check("map", fromMapish2)}
  Read maps with list as key                                      ${check("map", fromMapish3)}
  Read enums                                                      ${check("enum", fromEnumish)}
  """

  val data = List(
    Customer("CUSTOMER-1", "Fred", "Bedrock",   40),
    Customer("CUSTOMER-2", "Wilma", "Bedrock",  40),
    Customer("CUSTOMER-3", "Barney", "Bedrock", 39),
    Customer("CUSTOMER-4", "BamBam", "Bedrock",  2)
  )

  implicit def CustomerEncode: EncodeJson[Customer] =
    jencode4L((c: Customer) => (c.id, c.name, c.address, c.age))(
      Customer.IdField.name,
      Customer.NameField.name,
      Customer.AddressField.name,
      Customer.AgeField.name
    )

  implicit def CustomerDecode: DecodeJson[Customer] =
    jdecode4L(Customer.apply)(
      Customer.IdField.name,
      Customer.NameField.name,
      Customer.AddressField.name,
      Customer.AgeField.name
    )

  def customerToJsonKV(c: Customer) = List(
      Map(Customer.IdField.name       -> c.id     ).asJson,
      Map(Customer.NameField.name     -> c.name   ).asJson,
      Map(Customer.AddressField.name  -> c.address).asJson,
      Map(Customer.AgeField.name      -> c.age    ).asJson
    )

  def usage =
    withData(name, data)(context => {
      val result = context.glob(name </> "*.parquet").flatMap(
        ParquetIntrospectTools.listFromPath(jobConf, _)
      )
        .map(CatAsJson.recordToJson(_))
        .flatMap(x => Parse.decodeOption[Customer](x.toString))

      result must_== data
    })

  def checkFlattenedFields =
    withData(name, data)(context => {
      val result = context.glob(name </> "*.parquet").flatMap(
        ParquetIntrospectTools.listFromPath(jobConf, _)
      )
        .flatMap(x => x.data.map(CatAsJson.fieldToJson(_)))

      result.toSet must_== data.map(customerToJsonKV(_)).flatten.toSet
    })

  def fromBoolish: Boolish => Json = d => Record(List(
    Field("value", BooleanValue(d.value))
  )).asJson

  def fromDoublish: Doublish => Json = d => Record(List(
    Field("value", DoubleValue(d.value))
  )).asJson

  def fromBytish: Bytish => Json = d => Record(List(
    Field("value", IntValue(d.value))
  )).asJson

  def fromShortish: Shortish => Json = d => Record(List(
    Field("value", IntValue(d.value))
  )).asJson

  def fromIntish: Intish => Json = d => Record(List(
    Field("value", IntValue(d.value))
  )).asJson

  def fromLongish: Longish => Json = d => Record(List(
    Field("value", LongValue(d.value))
  )).asJson

  def fromStringish: Stringish => Json = d => Record(List(
    Field("value", StringValue(d.value))
  )).asJson

  def fromNestedish: Nestedish => Json = d => Record(List(
    Field("value", RecordValue(
      Record(List(
        Field("value", StringValue(d.value.value))
      ))
    ))
  )).asJson

  def fromListish: Listish => Json = d => Record(List(
    Field("values", ListValue(d.values.toList.map(StringValue)))
  )).asJson

  def fromMapish: Mapish => Json = d => Record(List(
    Field("values", MapValue(Map(d.values.toList.map({
      case (k, v) => StringValue(k) -> StringValue(v)
    }):_*)))
  )).asJson

  def fromMapish2: Mapish2 => Json = d => Record(List(
    Field("values", MapValue(Map(d.values.toList.map({
      case (k, v) => LongValue(k) -> StringValue(v)
    }):_*)))
  )).asJson

  def fromMapish3: Mapish3 => Json = d => Record(List(
    Field("values", MapValue(Map(d.values.toList.map({
      case (k, v) => ListValue(k.map(StringValue(_)).toList) -> StringValue(v)
    }):_*)))
  )).asJson

  def fromEnumish: Enumish => Json = d => Record(List(
    Field("value", EnumValue(d.value.name.toUpperCase))
  )).asJson

  def fromCustomer: Customer => Json = customer => Record(List(
    Field("id", StringValue(customer.id)),
    Field("name", StringValue(customer.name)),
    Field("address", StringValue(customer.address)),
    Field("age", IntValue(customer.age))
  )).asJson

  def withData[A <: ThriftStruct](name: String, data: List[A])(check: Context => Result)(implicit M: Manifest[A]) = {
    executesOk(IterablePipe(data).writeExecution(ParquetScroogeSource[A](name)))
    expectations(check)
  }

  def check[A <: ThriftStruct](name: String, converter: A => Json)(implicit A: Arbitrary[A], M: Manifest[A]) =
    prop { (data: List[A], uuid: UUID) =>
      withData(name, data)(context => {
        val result = context.glob(name </> "*.parquet").flatMap(
          ParquetIntrospectTools.listFromPath(jobConf, _)
        ).map(CatAsJson.recordToJson(_))

        result.toSet must_== data.map(converter).toSet
      })
    }.set(minTestsOk = 10)
}
