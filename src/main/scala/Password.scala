package es.eriktorr.password_validation

import PasswordValidationError.*

import cats.Semigroup
import cats.data.{NonEmptyList, Validated, ValidatedNec}
import cats.syntax.all.*

import scala.annotation.tailrec

opaque type Password = String

object Password:
  def unsafeFrom(value: String | Null): Password = value.nn

  def withFirstRuleSetFrom(value: String): AllErrorsOr[Password] = validateWith(
    value,
    NonEmptyList.of(
      hasMinimumLength(9),
      containsAtLeastOneUppercaseLetter,
      containsAtLeastOneLowercaseLetter,
      containsAnyNumber,
      containsAnUnderscore,
    ),
  )

  def withSecondRuleSetFrom(value: String): AllErrorsOr[Password] =
    validateWith(
      value,
      NonEmptyList.of(
        hasMinimumLength(7),
        containsAtLeastOneUppercaseLetter,
        containsAtLeastOneLowercaseLetter,
        containsAnyNumber,
      ),
    )

  def withThirdRuleSetFrom(value: String): AllErrorsOr[Password] =
    validateWith(
      value,
      NonEmptyList.of(
        hasMinimumLength(17),
        containsAtLeastOneUppercaseLetter,
        containsAtLeastOneLowercaseLetter,
        containsAnUnderscore,
      ),
    )

  def withFourthRuleSetFrom(value: String): AllErrorsOr[Password] =
    validateWith(
      value,
      NonEmptyList.of(
        hasMinimumLength(9),
        containsAtLeastOneUppercaseLetter,
        containsAnyNumber,
        containsAnUnderscore,
      ),
    )

  extension (password: Password) def value: String = password

  private[this] def hasMinimumLength(minimumLength: Int): ConstraintIn[String] =
    (value: String) =>
      if value.length >= minimumLength then value.validNec
      else PasswordHasMinimumLength(minimumLength).invalidNec

  private[this] val containsAtLeastOneUppercaseLetter: ConstraintIn[String] =
    (value: String) =>
      if raw".*[A-Z]+.*".r.matches(value) then value.validNec
      else PasswordContainsAtLeastOneUpperCaseLetter.invalidNec

  private[this] val containsAtLeastOneLowercaseLetter: ConstraintIn[String] =
    (value: String) =>
      if raw".*[a-z]+.*".r.matches(value) then value.validNec
      else PasswordContainsAtLeastOneLowerCaseLetter.invalidNec

  private[this] val containsAnyNumber: ConstraintIn[String] = (value: String) =>
    if raw".*[0-9]+.*".r.matches(value) then value.validNec
    else PasswordContainsAnyNumber.invalidNec

  private[this] val containsAnUnderscore: ConstraintIn[String] =
    (value: String) =>
      if raw".*_+.*".r.matches(value) then value.validNec
      else PasswordContainsAnUnderscore.invalidNec

  private[this] def validateWith(
      value: String,
      constraints: NonEmptyList[ConstraintIn[String]],
  ): AllErrorsOr[Password] =
    implicit val semigroupPassword: Semigroup[String] = (x: String, y: String) =>
      assert(x == y)
      x

    @tailrec
    def combineAll(
        constraints: List[ConstraintIn[String]],
        accumulated: AllErrorsOr[String],
    ): AllErrorsOr[String] =
      constraints match
        case Nil => accumulated
        case ::(head, next) =>
          combineAll(next, accumulated.combine(head(value)))

    combineAll(constraints.tail, constraints.head(value)).map(unsafeFrom)
