package com.dwolla

package object contextfunctions {
  type ?=>[-A, +B] = ContextFunction[A, B]
  val ?=> : ContextFunction.type = ContextFunction
}
