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

import scala.reflect.macros.Context

//import org.scalacheck._, Gen._, Arbitrary._
import org.scalacheck.Arbitrary

import com.twitter.scrooge._

//import au.com.cba.omnia.maestro.core.codec._

import au.com.cba.omnia.humbug.HumbugThriftStruct


/*
package au.com.cba.omnia.answer.macros

import scala.reflect.macros.Context

import scalikejdbc.{DB => SDB, _}

import au.com.cba.omnia.answer.Extractor
*/

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

/*
  def genArbitraryFor(typ: String, fields: List[(String, String)]): String = {
    (List("implicit def " + typ + "Arbitrary: Arbitrary[" + typ + "] =",
         "  Arbitrary(for {") ++
     fields.map { case (f,t) => s"    $f <- arbitrary[$t]" } ++
     List("  } yield " + typ + "(" + fields.map { case (f,t) => s"$f.value" }.mkString(", ") + ")")
    ).mkString("\n") 
  }

  def genArbitraryFlatMap(typ: String, fields: List[(String, String)]): String = {
    (List("implicit def " + typ + "Arbitrary: Arbitrary[" + typ + "] =",
         "  Arbitrary(") ++
     fields.map { case (f,t) => s" arbitrary[$t] flatMap { $f => " } ++
     List(typ + "(" + fields.map { case (f,t) => s"$f.value" }.mkString(", ") + ")") ++
     fields.map { case _ => "}" } ++ List(")")
    ).mkString("\n") 
  }
  */

/*
  def genArbitrary(typ: String, fields: List[(String, Type)]): String = {
    (
     fields.map { case (f,t) => s" arbitrary[$ts] flatMap { $f => " }
    ).mkString("")
  }
  */

     //List("  } yield " + typ + "(" + fields.map { case (f,t) => s"$f.value" }.mkString(", ") + ")")

  val ProductField = """_(\d+)""".r

  /** Gets all the `_1` style getters and their number for a thrift struct in numerical order.*/
  def indexed[A <: ThriftStruct: c.WeakTypeTag](c: Context): List[(c.universe.MethodSymbol, Int)] = 
    indexedUnsafe(c)(c.universe.weakTypeOf[A])

  /** Same as indexed but for any type where the type is assumed to be ThriftStruct.*/
  def indexedUnsafe(c: Context)(typ: c.universe.Type): List[(c.universe.MethodSymbol, Int)] = {
    import c.universe._
    typ.members.toList.map(member => (member, member.name.toString)).collect({
      case (member, ProductField(n)) =>
        (member.asMethod, n.toInt)
    }).sortBy(_._2)
  }

  def mkFields[A <: ThriftStruct]: Any =
      macro fieldsImpl[A]

  def fieldsImpl[A <: ThriftStruct: c.WeakTypeTag](c: Context) = {
    import c.universe._

    val typ        = c.universe.weakTypeOf[A]
    val entries    = fieldsFields[A](c)
    val companion  = typ.typeSymbol.companionSymbol
    val nameGetter = newTermName("name")
    //val idGetter   = newTermName("id")

    val fields = entries.map({
      case (method, field) =>
        val term    = q"""$companion.${newTermName(field + "Field")}"""
        val srcName = q"""$term.$nameGetter"""
        //val srcId   = q"""$term.$idGetter"""

        println(field)

/*
        val get     = q"""au.com.cba.omnia.maestro.core.data.Accessor[${method.returnType}]($srcId)"""
        val fld     = q"""au.com.cba.omnia.maestro.core.data.Field[$typ, ${method.returnType}]($srcName,$get)"""

        q"""val ${newTermName(field)} = $fld"""
*/
    })
    val refs = entries.map({
      case (method, field) =>
        val n = newTermName(field)
        println(n)
        q"$n"
    })
    val r =q"class FieldsWrapper { ..$fields; def AllFields = List(..$refs) }; new FieldsWrapper {}"

    println(entries)

    println(r)
    c.Expr(r)
  }

  //def generary[A <: ThriftStruct]: List[String] = macro fields2[A]

  /** Gets all the fields of a Thrift struct sorted in order of definition.*/
/*
  def fields2[A <: ThriftStruct: c.WeakTypeTag](c: Context): List[String] =
    fieldsUnsafe(c)(c.universe.weakTypeOf[A]).map{case (x,y) => y}
*/

  /** Gets all the fields of a Thrift struct sorted in order of definition.*/
  def fieldsFields[A <: ThriftStruct: c.WeakTypeTag](c: Context): List[(c.universe.MethodSymbol, String)] =
    fieldsUnsafe(c)(c.universe.weakTypeOf[A])

  /** Same as fields but for any type where the type is assumed to be ThriftStruct.*/
  def fieldsUnsafe(c: Context)(typ: c.universe.Type): List[(c.universe.MethodSymbol, String)] = {
    import c.universe._

    val fields =
      if (typ <:< c.universe.weakTypeOf[HumbugThriftStruct]) {
        // Get the fields in declaration order
        typ.declarations.sorted.toList.collect {
          case sym: TermSymbol if sym.isVar => sym.name.toString.trim //.capitalize
        }
      } else {
/*
        println("Check this out!")
        println( typ.typeSymbol.companionSymbol.typeSignature
          .member.map.toString)
          //.member(newTermName("apply")).asMethod.paramss.toString) //head.map(_.toString.capitalize))
        println("Checked!")
*/
        typ.typeSymbol.companionSymbol.typeSignature
          .member(newTermName("apply")).asMethod.paramss.head.map(_.name.toString) //.capitalize)
      }

    methodsUnsafe(c)(typ).zip(fields)
  }

  /** Gets all the `_1` style getters for a thrift struct in numerical order.*/
  def methods[A <: ThriftStruct: c.WeakTypeTag](c: Context): List[c.universe.MethodSymbol] =
    indexed(c).map({ case (method, _) => method })

  /** Same as methods but for any type where the type is assumed to be ThriftStruct.*/
  def methodsUnsafe(c: Context)(typ: c.universe.Type): List[c.universe.MethodSymbol] =
    indexedUnsafe(c)(typ).map({ case (method, _) => method })

  /** Creates an extractor for a singleton type or product. */
  def mkArbitrary[A]: Arbitrary[A] = macro impl[A]

  def impl[A : c.WeakTypeTag](c: Context): c.Expr[Arbitrary[A]] = {
    import c.universe._

    val targetType: Type = c.universe.weakTypeOf[A]

    /** Fail compilation with nice error message. */
    def abort(msg: String) =
      c.abort(c.enclosingPosition, s"Can't create Arbitrary for $targetType: $msg")

    /** Process an individual column. 
      */
    def processColumn(typ: Type, position: Int): Tree = q"rs.get[$typ]($position)"

    val targetTypes = targetType.declarations.sorted.toList collect {
      case sym: TermSymbol if sym.isVal && sym.isCaseAccessor => sym.typeSignatureIn(targetType)
    }

    val extractors =
      println(targetTypes)
      if (targetTypes.isEmpty) processColumn(targetType, 1)
      else {
        val parts = targetTypes.zipWithIndex.map { case (typ, i) => processColumn(typ, i + 1) }
        q"(..$parts)"
      }

      //import au.com.cba.omnia.ebenezer.Extractor
    c.Expr[Arbitrary[A]](q"""
      import org.scalacheck.Arbitrary
      Arbitrary(rs => $extractors)
    """)
  }

  /** Creates an arbitrary instance for a singleton type or product. */
  def thriftArbitrary[A <: ThriftStruct]: Arbitrary[A] = macro arbImpl[A]

  def arbImpl[A <: ThriftStruct : c.WeakTypeTag](c: Context): c.Expr[Arbitrary[A]] = {
    import c.universe.{Symbol => _, _}

    val srcType       = c.universe.weakTypeOf[A]
    val humbugTyp     = c.universe.weakTypeOf[HumbugThriftStruct]

    // val srcFieldsInfo = fieldsFields[A](c).map { case (f, n) => (n, f) }.toMap
    val srcFieldsInfo = fieldsFields[A](c).map { case (f, n) => f }

    val dstFields     = fieldsFields[A](c).map { case (f, n)  => (f, n) }
    val expectedTypes = dstFields.map { case (f, n) => (n, f.returnType) }

    val in  = newTermName(c.fresh)

    /** Fail compilation with nice error message. */
    def abort(msg: String) =
      c.abort(c.enclosingPosition, s"Can't create arbitrary instance for $srcType: $msg")

    //val humbugQList = srcFieldsInfo.map { case (n) => q"""$n <- arbitrary[String]""" }

    //val humbugArbitrary: Tree  = q"..$humbugQList"
    val hHead = srcFieldsInfo.head
    val humbugArbitrary: Tree = q"""println("Humbug: " + $hHead)"""
    //val humbugArbitrary: Tree  = srcFieldsInfo.map { case (n) => q"""$n <- arbitrary[String]""" }
    //    q"""throw new Exception("humbug")""" // abort(s"OMG HUMBUG")
    //val scroogeArbitrary: Tree = q"""throw new Exception("scrooge")""" // abort(s"OMG Scrooge")
    //val reifiedScrooge = q"""Customer("Jill33", "Jill", "33 Jill Street", 33)"""

/*
    import org.scalacheck.Gen
    def useArb[T, U](f : T => Gen[U])(implicit instance: Arbitrary[T]): Gen[U] = {
        import org.scalacheck.Arbitrary.arbitrary
        arbitrary[T] flatMap f //{ x => f x }
    }
*/

    def scroogeGen(typ: Type, args: List[(String, Type)]): Tree = {

      def mkNew(vals: List[c.Tree]) = {
        val companion = typ.typeSymbol.companionSymbol
        Apply(Select(Ident(companion), newTermName("apply")), vals)
      }

      def mkInner(args: List[(String, Type)], terms: List[Tree]): Tree = {
        if (args.length == 0) {
          mkNew(terms)
        } else {
          val (n,t) = args.head
          val nn = Ident(newTermName(n))
          val inner = mkInner(args.tail, terms :+ nn)
          q"""arbitrary[$t] flatMap { $nn : $t => ..$inner }"""
        }
      }
  
      mkInner(args, List())
    }

    val body = srcType match {
      case t if t <:< humbugTyp => humbugArbitrary
      case _                    => scroogeGen(srcType, expectedTypes)
    }

    val result = q"""
      import org.scalacheck.Arbitrary
      import org.scalacheck.Gen
      import org.scalacheck.Arbitrary.arbitrary
      Arbitrary[$srcType]($body)
    """

    println(srcFieldsInfo)

    println(expectedTypes)

    println(result)
    c.Expr[Arbitrary[A]](result)
  }
  
}
