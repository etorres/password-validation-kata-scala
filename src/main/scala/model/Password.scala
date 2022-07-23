package es.eriktorr.password_validation
package model

import model.PasswordValidationError.PasswordIsNotEmpty

opaque type Password = String

object Password:
  def unsafeFrom(value: String): Password = value

  def from(value: String): Either[PasswordValidationError, Password] = if value.nonEmpty then
    Right(unsafeFrom(value))
  else Left(PasswordIsNotEmpty)

  extension (password: Password) def value: String = password
