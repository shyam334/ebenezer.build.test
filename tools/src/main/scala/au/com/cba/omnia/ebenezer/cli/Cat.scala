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

package au.com.cba.omnia.ebenezer
package cli

import au.com.cba.omnia.ebenezer.introspect._
import au.com.cba.omnia.ebenezer.fs.Glob

import org.apache.hadoop.fs.Path
import org.apache.hadoop.fs.FileSystem
import org.apache.hadoop.conf.Configuration

object Cat {
  def run(patterns: List[String]): Unit = {
    val conf = new Configuration
    val paths = Glob.patterns(conf, patterns)
    val records = paths.foldLeft(Iterator[Record]())((iter, path) =>
      iter ++ ParquetIntrospectTools.iteratorFromPath(conf, path))
    records.foreach(println)
  }

  def main(args: Array[String]): Unit =
    run(args.toList)
}
