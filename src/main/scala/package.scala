package es.eriktorr

import cats.data.ValidatedNec

package object password_validation:
  type AllErrorsOr[A] = ValidatedNec[PasswordValidationError, A]

  type ConstraintIn[A] = A => AllErrorsOr[A]
