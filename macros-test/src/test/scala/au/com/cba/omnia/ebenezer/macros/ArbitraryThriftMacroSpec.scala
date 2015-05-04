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

package au.com.cba.omnia.ebenezer.macros.test

import org.specs2.Specification
import org.specs2.matcher.{TerminationMatchers, ThrownExpectations}

import au.com.cba.omnia.ebenezer.macros.ArbitraryThriftMacro._

object ArbitraryThriftMacroSpec extends Specification { def is = s2"""
    Test hello world $hellotest
"""

  def hellotest = {
    val seven = 7

/*
    val gF = genArbitraryFor("Customer",
              List(("id", "String"),
                   ("name", "String"),
                   ("address", "String"),
                   ("age", "Int"))
            )
    println(gF)

    val g = genArbitrary("Customer",
              List(("id", "String"),
                   ("name", "String"),
                   ("address", "String"),
                   ("age", "Int"))
            )
    println(g)
    */

    val cust = Customer("Joe77", "Joe", "13 Joe Street", 77)

    //ArbitraryThriftMacro.hello()
    debug1(seven)
    debug1(cust)

    def isCustomer[T](v: T) = v match {
      case Some(_:Customer) => true
      case _                => false
    }

    //mkFields[Customer]
    //val arb = mkArbitrary[Customer]
    import org.scalacheck.Arbitrary
    import org.scalacheck.Arbitrary.arbitrary

    implicit def CustomerArbitrary: Arbitrary[Customer] = thriftArbitrary[Customer]

    println("Test: Generating customer:")
    val genCustomer = arbitrary[Customer]
    val sampleCustomer = genCustomer.sample
    println(sampleCustomer)
    val result = isCustomer(sampleCustomer)
    println("Is customer?: " + result)

    result must_== true
  }
}
