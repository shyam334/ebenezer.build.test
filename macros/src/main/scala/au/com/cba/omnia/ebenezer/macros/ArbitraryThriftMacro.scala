//   Copyright 2015 Commonwealth Bank of Australia
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

package au.com.cba.omnia.ebenezer.macros

import language.experimental.macros

import reflect.macros.Context

/**
  * TODO: This macro creates Arbitrary instances for Thrift structs
  */
object ArbitraryThriftMacro {
  def debug1(param: Any): Unit = macro debug_impl

  def debug_impl(c: Context)(param: c.Expr[Any]): c.Expr[Unit] = {
    import c.universe._
    val paramRep = show(param.tree)
    val paramRepTree = Literal(Constant(paramRep))
    val paramRepExpr = c.Expr[String](paramRepTree)

    reify { println(paramRepExpr.splice + " = " + param.splice) }
  }

  def genArbitrary(typ: String, fields: List[(String, String)]): String = {
    (List("implicit def " + typ + "Arbitrary: Arbitrary[" + typ + "] =",
         "  Arbitrary(for {") ++
     fields.map { case (f,t) => s"    $f <- arbitrary[$t]" } ++
     List("  } yield " + typ + "(" + fields.map { case (f,t) => s"$f.value" }.mkString(", ") + ")")
    ).mkString("\n") 
  }
}


