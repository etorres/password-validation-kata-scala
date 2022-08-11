package es.eriktorr.password_validation
package model

import model.PasswordValidation.AllErrorsOr

import cats.data.Validated

trait Verifiable[A]:
  extension (a: A) def isValidRelaxed: Boolean

object Verifiable:
  given Verifiable[AllErrorsOr[Password]] with
    extension (a: AllErrorsOr[Password])
      def isValidRelaxed: Boolean = a match
        case Validated.Valid(_) => true
        case Validated.Invalid(errors) => errors.length == 1
