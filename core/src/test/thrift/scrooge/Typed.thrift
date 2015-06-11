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

#@namespace scala au.com.cba.omnia.ebenezer.test

struct Nested {
  1: string value
}

enum SomethingOrOther {
  SOME, OTHER
}

struct Boolish {
  1: bool value
}

struct Doublish {
  1: double value
}

struct Bytish {
  1: byte value
}

struct Shortish {
  1: i16 value
}

struct Intish {
  1: i32 value
}

struct Longish {
  1: i64 value
}

struct Stringish {
  1: string value
}

struct Nestedish {
  1: Nested value
}

struct Listish {
  1: list<string> values
}

struct Mapish {
  1: map<string, string> values
}

struct Enumish {
  1: SomethingOrOther value
}

struct Listish2 {
  1: list<list<string>> values
}

struct Mapish2 {
  1: map<i64, string> values
}

struct Mapish3 {
  1: map<list<string>, string> values
}