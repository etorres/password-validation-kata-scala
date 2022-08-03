package es.eriktorr.password_validation
package acceptance

import acceptance.ValidatePasswordSuite.{
  firstRuleSetTestCaseGen,
  secondRuleSetTestCaseGen,
  thirdRuleSetTestCaseGen,
  TestCase,
}
import application.ValidatePassword
import application.ValidatePassword.firstRuleSet
import model.PasswordValidation.AllErrorsOr
import model.PasswordValidationError.*
import model.{Password, PasswordValidation}

import cats.data.Validated.Invalid
import cats.data.{NonEmptyChain, Validated}
import cats.syntax.validated.*
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

final class ValidatePasswordSuite extends ScalaCheckSuite:

  private[this] def checkWith(systemUnderTest: ValidatePassword, testCase: TestCase) =
    systemUnderTest.validate(testCase.password) == testCase.expectedValidation && systemUnderTest
      .isValid(
        testCase.password,
      ) == testCase.expectedIsValid

  property("it should validate a password with the first rule set") {
    forAll(firstRuleSetTestCaseGen) { testCase =>
      checkWith(ValidatePassword.withFirstRuleSet, testCase)
    }
  }

  property("it should validate a password with the second rule set") {
    forAll(secondRuleSetTestCaseGen) { testCase =>
      checkWith(ValidatePassword.withSecondRuleSet, testCase)
    }
  }

  property("it should validate a password with the third rule set") {
    forAll(thirdRuleSetTestCaseGen) { testCase =>
      checkWith(ValidatePassword.withThirdRuleSet, testCase)
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

  private[this] val firstRuleSetValidPasswordGen: Gen[Password] =
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

  private[this] val secondRuleSetValidPasswordGen: Gen[Password] =
    for
      lowerCaseLetter <- Gen.alphaLowerChar
      upperCaseLetter <- Gen.alphaUpperChar
      number <- Gen.numChar
      mandatoryChars = List(lowerCaseLetter, upperCaseLetter, number)
      additionalCharsN <- Gen.choose(4, 12)
      additionalChars <- validCharsGen(additionalCharsN)
    yield
      val randomizedString = random.shuffle(mandatoryChars ++ additionalChars).mkString
      Password.unsafeFrom(randomizedString)

  private[this] val thirdRuleSetValidPasswordGen: Gen[Password] =
    for
      lowerCaseLetter <- Gen.alphaLowerChar
      upperCaseLetter <- Gen.alphaUpperChar
      mandatoryChars = List(lowerCaseLetter, upperCaseLetter, '_')
      additionalCharsN <- Gen.choose(14, 18)
      additionalChars <- validCharsGen(additionalCharsN)
    yield
      val randomizedString = random.shuffle(mandatoryChars ++ additionalChars).mkString
      Password.unsafeFrom(randomizedString)

  private[this] val firstRuleSetNoMinimumLengthPasswordGen: Gen[Password] = for
    lowerCaseLetter <- Gen.alphaLowerChar
    upperCaseLetter <- Gen.alphaUpperChar
    number <- Gen.numChar
    mandatoryChars = List(lowerCaseLetter, upperCaseLetter, number, '_')
  yield
    val randomizedString = random.shuffle(mandatoryChars).mkString
    Password.unsafeFrom(randomizedString)

  private[this] val secondRuleSetNoMinimumLengthPasswordGen: Gen[Password] = for
    lowerCaseLetter <- Gen.alphaLowerChar
    upperCaseLetter <- Gen.alphaUpperChar
    number <- Gen.numChar
    mandatoryChars = List(lowerCaseLetter, upperCaseLetter, number)
  yield
    val randomizedString = random.shuffle(mandatoryChars).mkString
    Password.unsafeFrom(randomizedString)

  private[this] val thirdRuleSetNoMinimumLengthPasswordGen: Gen[Password] = for
    lowerCaseLetter <- Gen.alphaLowerChar
    upperCaseLetter <- Gen.alphaUpperChar
    mandatoryChars = List(lowerCaseLetter, upperCaseLetter, '_')
  yield
    val randomizedString = random.shuffle(mandatoryChars).mkString
    Password.unsafeFrom(randomizedString)

  private[this] def noUpperCasePasswordGen(validPasswordGen: Gen[Password]): Gen[Password] =
    validPasswordGen.map(x => Password.unsafeFrom(x.value.toLowerCase))

  private[this] def noLowerCasePasswordGen(validPasswordGen: Gen[Password]): Gen[Password] =
    validPasswordGen.map(x => Password.unsafeFrom(x.value.toUpperCase))

  private[this] def noNumberPasswordGen(validPasswordGen: Gen[Password]): Gen[Password] = for
    password <- validPasswordGen
    replacement <- Gen.frequency(1 -> Gen.alphaChar.map(_.toString), 1 -> "_")
  yield Password.unsafeFrom(password.value.replaceAll("[0-9]", replacement))

  private[this] def noUnderscorePasswordGen(validPasswordGen: Gen[Password]): Gen[Password] = for
    password <- validPasswordGen
    replacement <- Gen.alphaNumChar.map(_.toString)
  yield Password.unsafeFrom(password.value.replaceAll("_", replacement))

  private[this] def allErrorsGen(maxSize: Int): Gen[Password] =
    Gen
      .frequency(
        1 -> Gen
          .choose(1, maxSize)
          .flatMap(n => Gen.containerOfN[List, Char](n, Gen.oneOf('.', ',', '|', '[')))
          .map(_.mkString),
        1 -> "",
      )
      .map(Password.unsafeFrom)

  final private case class TestCase(
      password: Password,
      expectedValidation: AllErrorsOr[Password],
      expectedIsValid: Boolean,
  )

  private val firstRuleSetTestCaseGen: Gen[TestCase] =
    val minimumLength = 8
    val validPasswordGen = firstRuleSetValidPasswordGen
    for (password, expectedValidation) <- Gen.frequency(
        1 -> firstRuleSetNoMinimumLengthPasswordGen.map(
          (_, PasswordHasMinimumLength(minimumLength).invalidNec),
        ),
        1 -> noUpperCasePasswordGen(validPasswordGen).map(
          (_, PasswordContainsAtLeastOneUpperCaseLetter.invalidNec),
        ),
        1 -> noLowerCasePasswordGen(validPasswordGen).map(
          (_, PasswordContainsAtLeastOneLowerCaseLetter.invalidNec),
        ),
        1 -> noNumberPasswordGen(validPasswordGen).map((_, PasswordContainsAnyNumber.invalidNec)),
        1 -> noUnderscorePasswordGen(validPasswordGen).map(
          (_, PasswordContainsAnUnderscore.invalidNec),
        ),
        1 -> allErrorsGen(minimumLength - 1).map(
          (
            _,
            Invalid(
              NonEmptyChain(
                PasswordHasMinimumLength(minimumLength),
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
    yield TestCase(
      password,
      expectedValidation,
      expectedValidation match
        case Validated.Valid(_) => true
        case Invalid(_) => false,
    )

  private val secondRuleSetTestCaseGen: Gen[TestCase] =
    val minimumLength = 6
    val validPasswordGen = secondRuleSetValidPasswordGen
    for (password, expectedValidation) <- Gen.frequency(
        1 -> secondRuleSetNoMinimumLengthPasswordGen.map(
          (_, PasswordHasMinimumLength(minimumLength).invalidNec),
        ),
        1 -> noUpperCasePasswordGen(validPasswordGen).map(
          (_, PasswordContainsAtLeastOneUpperCaseLetter.invalidNec),
        ),
        1 -> noLowerCasePasswordGen(validPasswordGen).map(
          (_, PasswordContainsAtLeastOneLowerCaseLetter.invalidNec),
        ),
        1 -> noNumberPasswordGen(validPasswordGen).map((_, PasswordContainsAnyNumber.invalidNec)),
        1 -> allErrorsGen(minimumLength - 1).map(
          (
            _,
            Invalid(
              NonEmptyChain(
                PasswordHasMinimumLength(minimumLength),
                PasswordContainsAtLeastOneUpperCaseLetter,
                PasswordContainsAtLeastOneLowerCaseLetter,
                PasswordContainsAnyNumber,
              ),
            ),
          ),
        ),
        1 -> validPasswordGen.map(x => (x, x.validNec)),
      )
    yield TestCase(
      password,
      expectedValidation,
      expectedValidation match
        case Validated.Valid(_) => true
        case Invalid(_) => false,
    )

  private val thirdRuleSetTestCaseGen: Gen[TestCase] =
    val minimumLength = 16
    val validPasswordGen = thirdRuleSetValidPasswordGen
    for (password, expectedValidation) <- Gen.frequency(
        1 -> thirdRuleSetNoMinimumLengthPasswordGen.map(
          (_, PasswordHasMinimumLength(minimumLength).invalidNec),
        ),
        1 -> noUpperCasePasswordGen(validPasswordGen).map(
          (_, PasswordContainsAtLeastOneUpperCaseLetter.invalidNec),
        ),
        1 -> noLowerCasePasswordGen(validPasswordGen).map(
          (_, PasswordContainsAtLeastOneLowerCaseLetter.invalidNec),
        ),
        1 -> noUnderscorePasswordGen(validPasswordGen).map(
          (_, PasswordContainsAnUnderscore.invalidNec),
        ),
        1 -> allErrorsGen(minimumLength - 1).map(
          (
            _,
            Invalid(
              NonEmptyChain(
                PasswordHasMinimumLength(minimumLength),
                PasswordContainsAtLeastOneUpperCaseLetter,
                PasswordContainsAtLeastOneLowerCaseLetter,
                PasswordContainsAnUnderscore,
              ),
            ),
          ),
        ),
        1 -> validPasswordGen.map(x => (x, x.validNec)),
      )
    yield TestCase(
      password,
      expectedValidation,
      expectedValidation match
        case Validated.Valid(_) => true
        case Invalid(_) => false,
    )
