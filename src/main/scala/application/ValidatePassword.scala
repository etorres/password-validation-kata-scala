package es.eriktorr.password_validation
package application

import model.PasswordValidation.AllErrorsOr
import model.PasswordValidationError.*
import model.{Password, PasswordValidation, PasswordValidationError}

import cats.data.ValidatedNec
import cats.syntax.all.*

sealed trait ValidatePassword:
  def validate(password: Password): AllErrorsOr[Password]

object ValidatePassword:
  val minimumLength = 8

  private[this] val upperCaseLetterPattern = raw".*[A-Z]+.*".r
  private[this] val lowerCaseLetterPattern = raw".*[a-z]+.*".r
  private[this] val anyNumberPattern = raw".*[0-9]+.*".r
  private[this] val underscorePattern = raw".*_+.*".r

  private[this] def hasMinimumLength(password: Password): AllErrorsOr[Password] =
    if password.value.length > minimumLength then password.validNec
    else PasswordHasMinimumLength(minimumLength).invalidNec

  private[this] def containsAtLeastOneUppercaseLetter(password: Password): AllErrorsOr[Password] =
    if upperCaseLetterPattern.matches(password.value) then password.validNec
    else PasswordContainsAtLeastOneUpperCaseLetter.invalidNec

  private[this] def containsAtLeastOneLowercaseLetter(password: Password): AllErrorsOr[Password] =
    if lowerCaseLetterPattern.matches(password.value) then password.validNec
    else PasswordContainsAtLeastOneLowerCaseLetter.invalidNec

  private[this] def containsAnyNumber(password: Password): AllErrorsOr[Password] =
    if anyNumberPattern.matches(password.value) then password.validNec
    else PasswordContainsAnyNumber.invalidNec

  private[this] def containsAnUnderscore(password: Password): AllErrorsOr[Password] =
    if underscorePattern.matches(password.value) then password.validNec
    else PasswordContainsAnUnderscore.invalidNec

  def impl: ValidatePassword = new ValidatePassword:
    override def validate(password: Password): AllErrorsOr[Password] =
      (
        hasMinimumLength(password),
        containsAtLeastOneUppercaseLetter(password),
        containsAtLeastOneLowercaseLetter(password),
        containsAnyNumber(password),
        containsAnUnderscore(password),
      )
        .mapN((_, _, _, _, _) => password)
