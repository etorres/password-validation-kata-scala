package es.eriktorr.password_validation
package model

import cats.data.ValidatedNec

sealed trait PasswordValidation:
  type AllErrorsOr[A] = ValidatedNec[PasswordValidationError, A]

object PasswordValidation extends PasswordValidation
