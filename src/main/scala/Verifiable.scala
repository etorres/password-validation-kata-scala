package es.eriktorr.password_validation

import cats.data.Validated

trait Verifiable[A]:
  extension (a: A) def isValidRelaxed: Boolean

object Verifiable:
  given Verifiable[AllErrorsOr[Password]] with
    extension (a: AllErrorsOr[Password])
      def isValidRelaxed: Boolean = a.fold(_.length == 1, _ => true)
