package es.eriktorr.password_validation
package application

import model.PasswordValidation.{AllErrorsOr, PasswordConstraint}
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
  private[this] val upperCaseLetterPattern = raw".*[A-Z]+.*".r
  private[this] val lowerCaseLetterPattern = raw".*[a-z]+.*".r
  private[this] val anyNumberPattern = raw".*[0-9]+.*".r
  private[this] val underscorePattern = raw".*_+.*".r

  private[this] def hasMinimumLength(minimumLength: Int): PasswordConstraint[Password] =
    (password: Password) =>
      if password.value.length > minimumLength then password.validNec
      else PasswordHasMinimumLength(minimumLength).invalidNec

  private[this] val containsAtLeastOneUppercaseLetter: PasswordConstraint[Password] =
    (password: Password) =>
      if upperCaseLetterPattern.matches(password.value) then password.validNec
      else PasswordContainsAtLeastOneUpperCaseLetter.invalidNec

  private[this] val containsAtLeastOneLowercaseLetter: PasswordConstraint[Password] =
    (password: Password) =>
      if lowerCaseLetterPattern.matches(password.value) then password.validNec
      else PasswordContainsAtLeastOneLowerCaseLetter.invalidNec

  private[this] val containsAnyNumber: PasswordConstraint[Password] = (password: Password) =>
    if anyNumberPattern.matches(password.value) then password.validNec
    else PasswordContainsAnyNumber.invalidNec

  private[this] val containsAnUnderscore: PasswordConstraint[Password] =
    (password: Password) =>
      if underscorePattern.matches(password.value) then password.validNec
      else PasswordContainsAnUnderscore.invalidNec

  private[this] def validateWith(
      password: Password,
      constraints: NonEmptyList[PasswordConstraint[Password]],
  ): AllErrorsOr[Password] =
    implicit val semigroupPassword: Semigroup[Password] = (x: Password, y: Password) =>
      assert(x == y)
      x

    @tailrec
    def combineAll(
        constraints: List[PasswordConstraint[Password]],
        accumulated: AllErrorsOr[Password],
    ): AllErrorsOr[Password] =
      constraints match
        case Nil => accumulated
        case ::(head, next) =>
          combineAll(next, accumulated.combine(head(password)))

    combineAll(constraints.tail, constraints.head(password))

  private[this] val firstRuleSet = NonEmptyList.of(
    hasMinimumLength(8),
    containsAtLeastOneUppercaseLetter,
    containsAtLeastOneLowercaseLetter,
    containsAnyNumber,
    containsAnUnderscore,
  )

  private[this] val secondRuleSet = NonEmptyList.of(
    hasMinimumLength(6),
    containsAtLeastOneUppercaseLetter,
    containsAtLeastOneLowercaseLetter,
    containsAnyNumber,
  )

  private[this] val thirdRuleSet = NonEmptyList.of(
    hasMinimumLength(16),
    containsAtLeastOneUppercaseLetter,
    containsAtLeastOneLowercaseLetter,
    containsAnUnderscore,
  )

  private[this] val fourthRuleSet = NonEmptyList.of(
    hasMinimumLength(8),
    containsAtLeastOneUppercaseLetter,
    containsAnyNumber,
    containsAnUnderscore,
  )

  def withFirstRuleSet: ValidatePassword = new ValidatePassword:
    override def validate(password: Password): AllErrorsOr[Password] =
      validateWith(password, firstRuleSet)

  def withSecondRuleSet: ValidatePassword = new ValidatePassword:
    override def validate(password: Password): AllErrorsOr[Password] =
      validateWith(password, secondRuleSet)

  def withThirdRuleSet: ValidatePassword = new ValidatePassword:
    override def validate(password: Password): AllErrorsOr[Password] =
      validateWith(password, thirdRuleSet)

  def withFourthRuleSet: ValidatePassword = new ValidatePassword:
    override def validate(password: Password): AllErrorsOr[Password] =
      validateWith(password, fourthRuleSet)
