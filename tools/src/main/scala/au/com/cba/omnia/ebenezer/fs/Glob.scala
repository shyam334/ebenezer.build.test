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
package fs

import org.apache.hadoop.fs.{PathNotFoundException, Path, FileSystem}
import org.apache.hadoop.conf.Configuration

object Glob {
  def patterns(conf: Configuration, patterns: List[String]): List[Path] =
    paths(conf, patterns.map(new Path(_)))

  def paths(conf: Configuration, paths: List[Path]): List[Path] = {
    val fs = FileSystem.get(conf)
    paths.flatMap(path => Option(fs.globStatus(path))
        .getOrElse(throw new PathNotFoundException(String.valueOf(path)))
        .toList
        .map(_.getPath)
      )
  }
}
