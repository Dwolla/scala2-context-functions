package com.dwolla.contextfunctions

/**
 * A trait representing a context-dependent function type abstraction.
 *
 * This trait models a function that requires an implicit context of type `Ctx` to produce a value of type `A`.
 * It is intended to represent computations or transformations that depend on an implicitly provided context
 * and simulates the syntax built into Scala 3.
 *
 * The trait would extend `(Ctx => A) with Serializable`, but the resulting `apply` methods have the same type
 * after erasure, so it fails to compile.
 *
 * @tparam Ctx the type of the implicit context required by the function
 * @tparam A   the type of the result produced by the function
 */
trait ContextFunction[-Ctx, +A] {
  def apply()(implicit ctx: Ctx): A

  def compose[B](g: B ?=> Ctx): B ?=> A =
    ContextFunction { implicit x => apply()(g()) }

  def andThen[B](g: A ?=> B): Ctx ?=> B =
    ContextFunction { implicit x => g()(apply()) }
}

object ContextFunction extends ContextFunctionInstances with FunctionInstances {
  def apply[Ctx, A](f: Ctx => A): Ctx ?=> A = new ContextFunctionImpl(f)

  def const[Ctx, A](a: A): Ctx ?=> A = new (Ctx ?=> A) {
    override def apply()(implicit ctx: Ctx): A = a
  }

  implicit def function1ToContextFunction[A, B](f: A => B): A ?=> B = new (A ?=> B) {
    override def apply()(implicit ctx: A): B = f(ctx)
  }

  implicit def contextFunctionToA[Ctx, A](cf: Ctx ?=> A)(implicit ctx: Ctx): A = cf()

  private final class ContextFunctionImpl[Ctx, A](f: Ctx => A) extends (Ctx ?=> A) {
    override def apply()(implicit ctx: Ctx): A = f(ctx)
  }
}
