package es.eriktorr.password_validation
package model

import scala.util.control.NoStackTrace

@SuppressWarnings(Array("org.wartremover.warts.Null"))
sealed abstract class PasswordValidationError(
    message: String,
    cause: Option[Throwable] = Option.empty[Throwable],
) extends NoStackTrace:
  import scala.language.unsafeNulls
  override def getCause: Throwable = cause.orNull
  override def getMessage: String = message

object PasswordValidationError:
  case object PasswordIsNotEmpty extends PasswordValidationError("Cannot be empty")

  final case class PasswordHasMinimumLength(minimumLength: Int)
      extends PasswordValidationError(s"Has at least $minimumLength characters")

  case object PasswordContainsAtLeastOneUpperCaseLetter
      extends PasswordValidationError("Contains a capital letter")

  case object PasswordContainsAtLeastOneLowerCaseLetter
      extends PasswordValidationError("Contains a lowercase letter")

  case object PasswordContainsAnyNumber extends PasswordValidationError("Contains a number")

  case object PasswordContainsAnUnderscore extends PasswordValidationError("Contains an underscore")
