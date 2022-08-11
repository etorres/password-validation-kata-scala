package es.eriktorr.password_validation
package model

import cats.data.ValidatedNec

object PasswordValidation:
  type AllErrorsOr[A] = ValidatedNec[PasswordValidationError, A]

  type ConstraintIn[A] = A => AllErrorsOr[A]
