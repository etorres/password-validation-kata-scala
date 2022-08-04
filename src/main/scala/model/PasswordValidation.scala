package es.eriktorr.password_validation
package model

import cats.data.ValidatedNec

sealed trait PasswordValidation:
  type AllErrorsOr[A] = ValidatedNec[PasswordValidationError, A]

  type ConstraintIn[A] = A => AllErrorsOr[A]

object PasswordValidation extends PasswordValidation
