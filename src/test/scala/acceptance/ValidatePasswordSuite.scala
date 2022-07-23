package es.eriktorr.password_validation
package acceptance

import acceptance.ValidatePasswordSuite.testCaseGen
import application.ValidatePassword
import application.ValidatePassword.minimumLength
import model.Password
import model.PasswordValidation.AllErrorsOr
import model.PasswordValidationError.*

import cats.data.NonEmptyChain
import cats.data.Validated.Invalid
import cats.syntax.validated.*
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

final class ValidatePasswordSuite extends ScalaCheckSuite:
  property("it should validate a password") {
    forAll(testCaseGen) { testCase =>
      ValidatePassword.impl.validate(testCase.password) == testCase.expected
    }
  }

object ValidatePasswordSuite:
  private[this] val random = scala.util.Random

  private[this] def validCharsGen(n: Int) = for
    lowerCaseLetters <- Gen.containerOfN[List, Char](10, Gen.alphaLowerChar)
    upperCaseLetters <- Gen.containerOfN[List, Char](10, Gen.alphaUpperChar)
    numbers <- Gen.containerOfN[List, Char](10, Gen.numChar)
    underscores <- Gen.containerOfN[List, Char](10, Gen.const('_'))
  yield
    val validChars = random.shuffle(lowerCaseLetters ++ upperCaseLetters ++ numbers ++ underscores)
    validChars.take(n)

  private[this] val validPasswordGen: Gen[Password] =
    for
      lowerCaseLetter <- Gen.alphaLowerChar
      upperCaseLetter <- Gen.alphaUpperChar
      number <- Gen.numChar
      mandatoryChars = List(lowerCaseLetter, upperCaseLetter, number, '_')
      additionalCharsN <- Gen.choose(5, 12)
      additionalChars <- validCharsGen(additionalCharsN)
    yield
      val randomizedString = random.shuffle(mandatoryChars ++ additionalChars).mkString
      Password.unsafeFrom(randomizedString)

  private[this] val noMinimumLengthPasswordGen: Gen[Password] = for
    lowerCaseLetter <- Gen.alphaLowerChar
    upperCaseLetter <- Gen.alphaUpperChar
    number <- Gen.numChar
    mandatoryChars = List(lowerCaseLetter, upperCaseLetter, number, '_')
  yield
    val randomizedString = random.shuffle(mandatoryChars).mkString
    Password.unsafeFrom(randomizedString)

  private[this] val noUpperCasePasswordGen: Gen[Password] =
    validPasswordGen.map(x => Password.unsafeFrom(x.value.toLowerCase))

  private[this] val noLowerCasePasswordGen: Gen[Password] =
    validPasswordGen.map(x => Password.unsafeFrom(x.value.toUpperCase))

  private[this] val noNumberPasswordGen: Gen[Password] = for
    password <- validPasswordGen
    replacement <- Gen.frequency(1 -> Gen.alphaChar.map(_.toString), 1 -> "_")
  yield Password.unsafeFrom(password.value.replaceAll("[0-9]", replacement))

  private[this] val noUnderscorePasswordGen: Gen[Password] = for
    password <- validPasswordGen
    replacement <- Gen.alphaNumChar.map(_.toString)
  yield Password.unsafeFrom(password.value.replaceAll("_", replacement))

  private[this] val allErrorsGen: Gen[Password] =
    Gen
      .frequency(
        1 -> Gen
          .choose(1, 7)
          .flatMap(n => Gen.containerOfN[List, Char](n, Gen.oneOf('.', ',', '|', '[')))
          .map(_.mkString),
        1 -> "",
      )
      .map(Password.unsafeFrom)

  final private case class TestCase(
      password: Password,
      expected: AllErrorsOr[Password],
  )

  private val testCaseGen: Gen[TestCase] =
    for (password, expected) <- Gen.frequency(
        1 -> noMinimumLengthPasswordGen.map(
          (_, PasswordHasMinimumLength(ValidatePassword.minimumLength).invalidNec),
        ),
        1 -> noUpperCasePasswordGen.map((_, PasswordContainsAtLeastOneUpperCaseLetter.invalidNec)),
        1 -> noLowerCasePasswordGen.map((_, PasswordContainsAtLeastOneLowerCaseLetter.invalidNec)),
        1 -> noNumberPasswordGen.map((_, PasswordContainsAnyNumber.invalidNec)),
        1 -> noUnderscorePasswordGen.map((_, PasswordContainsAnUnderscore.invalidNec)),
        1 -> allErrorsGen.map(
          (
            _,
            Invalid(
              NonEmptyChain(
                PasswordHasMinimumLength(ValidatePassword.minimumLength),
                PasswordContainsAtLeastOneUpperCaseLetter,
                PasswordContainsAtLeastOneLowerCaseLetter,
                PasswordContainsAnyNumber,
                PasswordContainsAnUnderscore,
              ),
            ),
          ),
        ),
        1 -> validPasswordGen.map(x => (x, x.validNec)),
      )
    yield TestCase(password, expected)
