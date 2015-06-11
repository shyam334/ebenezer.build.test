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

import org.apache.hadoop.conf.Configuration

import argonaut._, Argonaut._

import au.com.cba.omnia.ebenezer.fs.Glob
import au.com.cba.omnia.ebenezer.introspect._

/**
 * Cli tool to read parquet file contents as Json.
 * It expects a path or path glob in HDFS with parquet files.
 * It will output to stdout.
 */
object CatAsJson {
  def run(patterns: List[String]): Unit = {
    val conf    = new Configuration
    val paths   = Glob.patterns(conf, patterns)
    val records = paths.foldLeft(Iterator[Record]())((iter, path) =>
      iter ++ ParquetIntrospectTools.streamFromPath(conf, path)
    )
    records.map(_.asJson).foreach(println)
  }

  implicit def RecordEncodeJson: EncodeJson[Record] =
    EncodeJson((record: Record) => recordToJson(record))

  def flattenJson(l: List[Json]): Json =
    l.foldLeft(Json())((a: Json, b: Json) => a.deepmerge(b))

  def recordToJson(record: Record): Json =
    flattenJson(record.data.map(fieldToJson(_)))

  def fieldToJson(field: Field): Json =
    Json(field.name -> valueToJson(field.value))

  def valueToJson(value: Value): Json = {
    value match {
      case BooleanValue(v)   => jBool(v)
      case EnumValue(v)      => Json.array(jString(v))
      case IntValue(v)       => jNumber(v)
      case LongValue(v)      => jNumber(v)
      case FloatValue(v)     => jNumberOrString(v)
      case DoubleValue(v)    => jNumber(v)
      case StringValue(v)    => jString(v)
      case ListValue(v)      => jArray(v.map(valueToJson(_)))
      case MapValue(v)       => mapValueToJson(v)
      case RecordValue(v)    => jArray(v.data.map(fieldToJson(_)))
      case null              => jNull
    }
  }

  def mapValueToJson(mapValue: Map[Value, Value]): Json = {
    if(mapValue.keys.forall(_.isInstanceOf[StringValue])) {
      jObjectAssocList(
        mapValue.map { case (k, v)  =>
          (String.valueOf(k), valueToJson(v))
        }.toList)
    } else {
      jArray(
        mapValue.map { case (k, v)  =>
          jObjectFields(("key", valueToJson(k)), ("value", valueToJson(v)))
         }.toList
      )
    }
  }

  def main(args: Array[String]): Unit =
    run(args.toList)

}
