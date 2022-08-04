package es.eriktorr.password_validation
package application

import model.PasswordValidation.{AllErrorsOr, ConstraintIn}
import model.PasswordValidationError.*
import model.{Password, PasswordValidation, PasswordValidationError}

import cats.Semigroup
import cats.data.{NonEmptyList, Validated, ValidatedNec}
import cats.syntax.all.*
import es.eriktorr.password_validation

import scala.annotation.tailrec

sealed trait ValidatePassword:
  def validate(password: Password): AllErrorsOr[Password]

  def isValid(password: Password): Boolean = validate(password) match
    case Validated.Valid(_) => true
    case Validated.Invalid(_) => false

  def isValidRelaxed(password: Password): Boolean = validate(password) match
    case Validated.Valid(_) => true
    case Validated.Invalid(errors) => errors.length == 1

object ValidatePassword:
  private[this] def hasMinimumLength(minimumLength: Int): ConstraintIn[Password] =
    (password: Password) =>
      if password.value.length >= minimumLength then password.validNec
      else PasswordHasMinimumLength(minimumLength).invalidNec

  private[this] val containsAtLeastOneUppercaseLetter: ConstraintIn[Password] =
    (password: Password) =>
      val upperCaseLetterPattern = raw".*[A-Z]+.*".r
      if upperCaseLetterPattern.matches(password.value) then password.validNec
      else PasswordContainsAtLeastOneUpperCaseLetter.invalidNec

  private[this] val containsAtLeastOneLowercaseLetter: ConstraintIn[Password] =
    (password: Password) =>
      val lowerCaseLetterPattern = raw".*[a-z]+.*".r
      if lowerCaseLetterPattern.matches(password.value) then password.validNec
      else PasswordContainsAtLeastOneLowerCaseLetter.invalidNec

  private[this] val containsAnyNumber: ConstraintIn[Password] = (password: Password) =>
    val anyNumberPattern = raw".*[0-9]+.*".r
    if anyNumberPattern.matches(password.value) then password.validNec
    else PasswordContainsAnyNumber.invalidNec

  private[this] val containsAnUnderscore: ConstraintIn[Password] =
    (password: Password) =>
      val underscorePattern = raw".*_+.*".r
      if underscorePattern.matches(password.value) then password.validNec
      else PasswordContainsAnUnderscore.invalidNec

  private[this] def validateWith(
      password: Password,
      constraints: NonEmptyList[ConstraintIn[Password]],
  ): AllErrorsOr[Password] =
    implicit val semigroupPassword: Semigroup[Password] = (x: Password, y: Password) =>
      assert(x == y)
      x

    @tailrec
    def combineAll(
        constraints: List[ConstraintIn[Password]],
        accumulated: AllErrorsOr[Password],
    ): AllErrorsOr[Password] =
      constraints match
        case Nil => accumulated
        case ::(head, next) =>
          combineAll(next, accumulated.combine(head(password)))

    combineAll(constraints.tail, constraints.head(password))

  def withFirstRuleSet: ValidatePassword = new ValidatePassword:
    override def validate(password: Password): AllErrorsOr[Password] =
      validateWith(
        password,
        NonEmptyList.of(
          hasMinimumLength(9),
          containsAtLeastOneUppercaseLetter,
          containsAtLeastOneLowercaseLetter,
          containsAnyNumber,
          containsAnUnderscore,
        ),
      )

  def withSecondRuleSet: ValidatePassword = new ValidatePassword:
    override def validate(password: Password): AllErrorsOr[Password] =
      validateWith(
        password,
        NonEmptyList.of(
          hasMinimumLength(7),
          containsAtLeastOneUppercaseLetter,
          containsAtLeastOneLowercaseLetter,
          containsAnyNumber,
        ),
      )

  def withThirdRuleSet: ValidatePassword = new ValidatePassword:
    override def validate(password: Password): AllErrorsOr[Password] =
      validateWith(
        password,
        NonEmptyList.of(
          hasMinimumLength(17),
          containsAtLeastOneUppercaseLetter,
          containsAtLeastOneLowercaseLetter,
          containsAnUnderscore,
        ),
      )

  def withFourthRuleSet: ValidatePassword = new ValidatePassword:
    override def validate(password: Password): AllErrorsOr[Password] =
      validateWith(
        password,
        NonEmptyList.of(
          hasMinimumLength(9),
          containsAtLeastOneUppercaseLetter,
          containsAnyNumber,
          containsAnUnderscore,
        ),
      )
