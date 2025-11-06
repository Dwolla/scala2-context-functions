/* Based on the Cats instances for Function1, but adapted to work with ContextFunction.
 * See https://github.com/typelevel/cats/blob/a37fd5183f39e066c343ba6a088d4f82148868d5/core/src/main/scala/cats/instances/function.scala and
 * https://github.com/typelevel/cats/blob/a37fd5183f39e066c343ba6a088d4f82148868d5/kernel/src/main/scala/cats/kernel/instances/FunctionInstances.scala
 * for the original sources upon which this is based.
 *
 * Original copyright notice:
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package com.dwolla.contextfunctions

import cats.*
import cats.arrow.*
import cats.data.*
import cats.kernel.{Group, Monoid, Semigroup, *}
import cats.syntax.all.*
import com.dwolla.contextfunctions.compat.scalaVersionSpecific.*

import scala.util.control.TailCalls.{TailRec, done, tailcall}

private[contextfunctions] trait ContextFunctionInstances extends ContextFunctionLowPriorityInstances {
  implicit def contravariantMonoidalForContextFunction[R: Monoid]: ContravariantMonoidal[* ?=> R] =
    new ContravariantMonoidal[* ?=> R] {
      override def unit: Unit ?=> R = ContextFunction.const(Monoid[R].empty)

      override def contramap[A, B](fa: A ?=> R)(f: B => A): B ?=> R =
        fa.compose(ContextFunction(f))

      override def product[A, B](fa: A ?=> R,
                                 fb: B ?=> R): (A, B) ?=> R =
        ContextFunction { case (implicit0(a: A), implicit0(b: B)) =>
          fa() |+| fb()
        }
    }

  implicit def monadForContextFunction[T1]: Monad[T1 ?=> *] =
    new Monad[T1 ?=> *] {
      override def pure[R](r: R): T1 ?=> R = (_: T1) => r

      override def flatMap[R1, R2](fa: T1 ?=> R1)
                                  (f: R1 => T1 ?=> R2): T1 ?=> R2 =
        ContextFunction(implicit t => f(fa())())

      override def map[R1, R2](fa: T1 ?=> R1)(f: R1 => R2): T1 ?=> R2 =
        fa.andThen(ContextFunction(f))

      override def map2[A, B, C](fa: T1 ?=> A, fb: T1 ?=> B)
                                (fn: (A, B) => C): T1 ?=> C =
        ContextFunction(implicit t => fn(fa(), fb()))

      override def product[A, B](fa: T1 ?=> A, fb: T1 ?=> B): T1 ?=> (A, B) =
        ContextFunction(implicit t => (fa(), fb()))

      override def ap[A, B](f: T1 ?=> (A => B))
                           (fa: T1 ?=> A): T1 ?=> B =
        ContextFunction(implicit t => f().apply(fa()))

      override def tailRecM[A, B](a: A)(fn: A => T1 ?=> Either[A, B]): T1 ?=> B =
        ContextFunction { implicit t1: T1 =>
          @annotation.tailrec
          def step(thisA: A): B =
            fn(thisA)() match {
              case Right(b) => b
              case Left(nextA) => step(nextA)
            }

          step(a)
        }
    }

  implicit val arrowInstancesForContextFunction: ArrowChoice[ContextFunction] & CommutativeArrow[ContextFunction] =
    new ArrowChoice[ContextFunction] with CommutativeArrow[ContextFunction] {
      override def choose[A, B, C, D](f: A ?=> C)(g: B ?=> D): Either[A, B] ?=> Either[C, D] = ContextFunction {
        case Left(implicit0(a: A))  => Left(f())
        case Right(implicit0(b: B)) => Right(g())
      }

      override def lift[A, B](f: A => B): A ?=> B =
        ContextFunction(f)

      override def first[A, B, C](fa: A ?=> B): (A, C) ?=> (B, C) = ContextFunction {
        case (implicit0(a: A), implicit0(c: C)) => (fa(), c)
      }

      override def split[A, B, C, D](f: A ?=> B, g: C ?=> D): (A, C) ?=> (B, D) = ContextFunction {
        case (implicit0(a: A), implicit0(c: C)) => (f(), g())
      }

      override def compose[A, B, C](f: B ?=> C, g: A ?=> B): A ?=> C = f.compose(g)
    }

  implicit val monoidKForContextFunction: MonoidK[λ[a => a ?=> a]] = new MonoidK[λ[a => a ?=> a]] {
    private val category: Category[?=>] = Category[?=>]

    override def empty[A]: A ?=> A = category.id

    override def combineK[A](x: A ?=> A, y: A ?=> A): A ?=> A =
      ContextFunction(AndThen(implicit a => category.compose(x, y)()))
  }
}

private[contextfunctions] trait ContextFunctionLowPriorityInstances {
  implicit def contravariantForContextFunction[R]: Contravariant[* ?=> R] =
    new Contravariant[* ?=> R] {
      override def contramap[T1, T0](fa: T1 ?=> R)(f: T0 => T1): T0 ?=> R =
        fa.compose(f)
    }

  implicit def distributiveForContextFunction[T1]: Distributive[T1 ?=> *] =
    new Distributive[T1 ?=> *] {
      override def distribute[F[_]: Functor, A, B](fa: F[A])(f: A => (T1 ?=> B)): T1 ?=> F[B] = ContextFunction { implicit t1 =>
        Functor[F].map(fa)(a => f(a))
      }

      override def map[A, B](fa: T1 ?=> A)(f: A => B): T1 ?=> B = ContextFunction { implicit t1 =>
        f(fa())
      }
    }
}

private[contextfunctions] trait FunctionInstances extends FunctionInstances0 {
  implicit def commutativeGroupForContextFunction[A, B](implicit G: CommutativeGroup[B]): CommutativeGroup[A ?=> B] =
    new Function1Group[A, B] with CommutativeGroup[A ?=> B] { def B: Group[B] = G }
}

private[contextfunctions] trait FunctionInstances0 extends FunctionInstances1 {
  implicit def groupForContextFunction[A, B](implicit G: Group[B]): Group[A ?=> B] =
    new Function1Group[A, B] { def B: Group[B] = G }

  implicit def boundedSemilatticeForContextFunction[A, B](implicit
                                                          G: BoundedSemilattice[B]
                                                         ): BoundedSemilattice[A ?=> B] =
    new Function1Monoid[A, B] with BoundedSemilattice[A ?=> B] { def B: Monoid[B] = G }
}

private[contextfunctions] trait FunctionInstances1 extends FunctionInstances2 {
  implicit def commutativeMonoidForContextFunction[A, B](implicit
                                                         M: CommutativeMonoid[B]
                                                        ): CommutativeMonoid[A ?=> B] =
    new Function1Monoid[A, B] with CommutativeMonoid[A ?=> B] { def B: Monoid[B] = M }

  implicit def semilatticeForContextFunction[A, B](implicit M: Semilattice[B]): Semilattice[A ?=> B] =
    new Function1Semigroup[A, B] with Semilattice[A ?=> B] { def B: Semigroup[B] = M }
}

private[contextfunctions] trait FunctionInstances2 extends FunctionInstances3 {
  implicit def monoidForContextFunction[A, B](implicit M: Monoid[B]): Monoid[A ?=> B] =
    new Function1Monoid[A, B] { def B: Monoid[B] = M }

  implicit def bandForContextFunction[A, B](implicit S: Band[B]): Band[A ?=> B] =
    new Function1Semigroup[A, B] with Band[A ?=> B] { def B: Semigroup[B] = S }
}

private[contextfunctions] trait FunctionInstances3 extends FunctionInstances4 {
  implicit def commutativeSemigroupForContextFunction[A, B](implicit
                                                            S: CommutativeSemigroup[B]
                                                           ): CommutativeSemigroup[A ?=> B] =
    new Function1Semigroup[A, B] with CommutativeSemigroup[A ?=> B] { def B: Semigroup[B] = S }
}

private[contextfunctions] trait FunctionInstances4 {
  implicit def semigroupForContextFunction[A, B](implicit S: Semigroup[B]): Semigroup[A ?=> B] =
    new Function1Semigroup[A, B] { def B: Semigroup[B] = S }
}

final private[contextfunctions] case class CombineFunction1[A, B](left: A ?=> B, right: A ?=> B, semiB: Semigroup[B]) extends (A ?=> B) {
  private def call(fn: A ?=> B, a: A): TailRec[B] =
    fn match {
      case ref: CombineFunction1[A, B] @unchecked =>
        for {
          lb <- tailcall(call(ref.left, a))
          rb <- tailcall(call(ref.right, a))
        } yield ref.semiB.combine(lb, rb)
      case _ => done(fn()(a))
    }

  override def apply()(implicit a: A): B = call(this, a).result
}

@suppressUnusedImportWarningForScalaVersionSpecific
private trait Function1Semigroup[A, B] extends Semigroup[A ?=> B] {
  implicit def B: Semigroup[B]

  override def combine(x: A ?=> B, y: A ?=> B): A ?=> B =
    CombineFunction1(x, y, B)

  override def combineAllOption(fns: IterableOnce[A ?=> B]): Option[A ?=> B] =
    if (fns.iterator.isEmpty) None
    else
      Some(ContextFunction {
        implicit a => B.combineAllOption(fns.iterator.map(_.apply())).get
      })
}

private trait Function1Monoid[A, B] extends Function1Semigroup[A, B] with Monoid[A ?=> B] {
  implicit def B: Monoid[B]

  val empty: A ?=> B = ContextFunction.const(B.empty)
}

private trait Function1Group[A, B] extends Function1Monoid[A, B] with Group[A ?=> B] {
  implicit def B: Group[B]

  def inverse(x: A ?=> B): A ?=> B =
    ContextFunction(implicit a => B.inverse(x()))
}
