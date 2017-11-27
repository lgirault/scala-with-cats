package gp

import cats.tests.CatsSuite
//import cats.laws.discipline.MonoidKTests
import cats.kernel.laws.discipline.MonoidTests

class OrMonoidTestSuite extends CatsSuite {

  import Chap2MonoidAndSemigroups.orMonoid
  checkAll("OrMonoid", MonoidTests[Boolean].monoid)

}

class AndMonoidTestSuite extends CatsSuite {

  import Chap2MonoidAndSemigroups.andMonoid
  checkAll("AndMonoid", MonoidTests[Boolean].monoid)

}

class UnionSetMonoidTestSuite extends CatsSuite {

  import Chap2MonoidAndSemigroups.unionMonoid
  checkAll("UniontSetMonoid", MonoidTests[Set[Int]].monoid)

}