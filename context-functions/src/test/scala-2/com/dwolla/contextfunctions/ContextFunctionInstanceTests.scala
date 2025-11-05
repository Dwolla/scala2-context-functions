package com.dwolla.contextfunctions

import cats.*
import cats.kernel.CommutativeGroup
import cats.kernel.laws.discipline.*
import cats.laws.discipline.*
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.laws.discipline.arbitrary.*
import cats.syntax.*
import cats.tests.Helpers.*
import munit.*
import org.scalacheck.*

class ContextFunctionInstanceTests
  extends FunSuite
    with EqSyntax
    with DisciplineSuite {

  implicit def arbContextFunction[A: Cogen, B: Arbitrary]: Arbitrary[A ?=> B] = Arbitrary {
    Arbitrary.arbitrary[A => B].map(ContextFunction(_))
  }

  implicit def eqForContextFunctionExhaustive[A: ExhaustiveCheck, B: Eq]: Eq[A ?=> B] = Eq.instance[A ?=> B] {
    case (f, g) => ExhaustiveCheck[A].allValues.forall(implicit a => Eq[B].eqv(f(), g()))
  }

  implicit val intCommutativeGroup: CommutativeGroup[Int] = cats.instances.int.catsKernelStdGroupForInt

  // by analogy from cats
  checkAll("ContravariantMonoidal[* ?=> Int]", ContravariantMonoidalTests[* ?=> Int].contravariantMonoidal[MiniInt, MiniInt, MiniInt])
  checkAll("Monad[MiniInt ?=> *]", MonadTests[MiniInt ?=> *].monad[String, String, String])
  checkAll("ArrowChoice[ContextFunction]", ArrowChoiceTests[ContextFunction].arrowChoice[MiniInt, MiniInt, MiniInt, MiniInt, String, String])
  checkAll("CommutativeArrow[ContextFunction]", CommutativeArrowTests[ContextFunction].commutativeArrow[MiniInt, MiniInt, MiniInt, MiniInt, String, String])
  checkAll("MonoidK[λ[a => a ?=> a]]", MonoidKTests[λ[a => a ?=> a]].monoidK[MiniInt])
  checkAll("Contravariant[* ?=> Int]", ContravariantTests[* ?=> Int].contravariant[MiniInt, MiniInt, MiniInt])
  checkAll("Distributive[MiniInt ?=> *]", DistributiveTests[MiniInt ?=> *].distributive[MiniInt, MiniInt, MiniInt, Option, Function0])

  // by analogy from cats-kernel
  checkAll("Semigroup[A ?=> B]", SemigroupTests[MiniInt ?=> Semi].semigroup)
  checkAll("CommutativeSemigroup[A ?=> B]", CommutativeSemigroupTests[MiniInt ?=> CSemi].commutativeSemigroup)
  checkAll("Band[A ?=> B]", BandTests[MiniInt ?=> Bnd].band)
  checkAll("Semilattice[A ?=> B]", SemilatticeTests[MiniInt ?=> SL].semilattice)
  checkAll("BoundedSemilattice[A ?=> B]", BoundedSemilatticeTests[MiniInt ?=> BSL].boundedSemilattice)
  checkAll("Monoid[A ?=> B]", MonoidTests[MiniInt ?=> Mono].monoid)
  checkAll("CommutativeMonoid[A ?=> B]", CommutativeMonoidTests[MiniInt ?=> CMono].commutativeMonoid)
  checkAll("Group[A ?=> B]", GroupTests[MiniInt ?=> Grp].group)
  checkAll("CommutativeGroup[A ?=> B]", CommutativeGroupTests[MiniInt ?=> CGrp].commutativeGroup)

  {
    implicit val isoCodomain: Isomorphisms[* ?=> Long] = Isomorphisms.invariant[* ?=> Long]
    checkAll("ContravariantMonoidal[A ?=> Long]", ContravariantMonoidalTests[* ?=> Long].contravariantMonoidal[MiniInt, MiniInt, MiniInt])
  }
}
