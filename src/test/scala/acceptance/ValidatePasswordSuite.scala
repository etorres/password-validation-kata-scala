package es.eriktorr.password_validation
package acceptance

import acceptance.ValidatePasswordSuite.*
import application.ValidatePassword
import model.PasswordValidation.AllErrorsOr
import model.PasswordValidationError.*
import model.{Password, PasswordValidation}

import cats.data.Validated.Invalid
import cats.data.{NonEmptyChain, Validated}
import cats.syntax.validated.*
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import scala.jdk.CollectionConverters.*

final class ValidatePasswordSuite extends ScalaCheckSuite:

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

  property("it should validate a password with the fourth rule set") {
    forAll(fourthRuleSetTestCaseGen) { testCase =>
      checkWith(ValidatePassword.withFourthRuleSet, testCase)
    }
  }

  private[this] def checkWith(systemUnderTest: ValidatePassword, testCase: TestCase) =
    systemUnderTest.validate(testCase.password) == testCase.expectedValidation && systemUnderTest
      .isValid(testCase.password) == testCase.expectedIsValid && systemUnderTest.isValidRelaxed(
      testCase.password,
    ) == testCase.expectedIsValidRelaxed

object ValidatePasswordSuite:
  private[this] val random = scala.util.Random

  private[this] def validCharsGen(n: Int): Gen[List[Char]] = for
    lowerCaseLetters <- Gen.containerOfN[List, Char](10, Gen.alphaLowerChar)
    upperCaseLetters <- Gen.containerOfN[List, Char](10, Gen.alphaUpperChar)
    numbers <- Gen.containerOfN[List, Char](10, Gen.numChar)
    underscores <- Gen.containerOfN[List, Char](10, Gen.const('_'))
  yield
    val validChars = random.shuffle(lowerCaseLetters ++ upperCaseLetters ++ numbers ++ underscores)
    validChars.take(n)

  private[this] def validPasswordGen(
      mandatoryCharsGens: List[Gen[Char]],
      minimumLength: Int,
  ): Gen[Password] = for
    mandatoryChars <- Gen.sequence(mandatoryCharsGens).map(_.asScala.toList)
    additionalCharsN <- Gen.choose(minimumLength - mandatoryChars.length, minimumLength + 10)
    additionalChars <- validCharsGen(additionalCharsN)
  yield
    val randomizedString = random.shuffle(mandatoryChars ++ additionalChars).mkString
    Password.unsafeFrom(randomizedString)

  private[this] def noMinimumLengthPasswordGen(mandatoryCharsGens: List[Gen[Char]]): Gen[Password] =
    for mandatoryChars <- Gen.sequence(mandatoryCharsGens).map(_.asScala.toList)
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

  private[this] def allErrorsGen(minimumLength: Int): Gen[Password] =
    Gen
      .frequency(
        1 -> Gen
          .choose(1, minimumLength - 1)
          .flatMap(n => Gen.containerOfN[List, Char](n, Gen.oneOf('.', ',', '|', '[')))
          .map(_.mkString),
        1 -> "",
      )
      .map(Password.unsafeFrom)

  final private case class TestCase(
      password: Password,
      expectedValidation: AllErrorsOr[Password],
      expectedIsValid: Boolean,
      expectedIsValidRelaxed: Boolean,
  )

  private val firstRuleSetTestCaseGen: Gen[TestCase] =
    val minimumLength = 9
    val mandatoryCharsGens =
      List(Gen.alphaLowerChar, Gen.alphaUpperChar, Gen.numChar, Gen.const('_'))
    for (password, expectedValidation) <- Gen.frequency(
        1 -> noMinimumLengthPasswordGen(mandatoryCharsGens).map(
          (_, PasswordHasMinimumLength(minimumLength).invalidNec),
        ),
        1 -> noUpperCasePasswordGen(validPasswordGen(mandatoryCharsGens, minimumLength)).map(
          (_, PasswordContainsAtLeastOneUpperCaseLetter.invalidNec),
        ),
        1 -> noLowerCasePasswordGen(validPasswordGen(mandatoryCharsGens, minimumLength)).map(
          (_, PasswordContainsAtLeastOneLowerCaseLetter.invalidNec),
        ),
        1 -> noNumberPasswordGen(validPasswordGen(mandatoryCharsGens, minimumLength))
          .map((_, PasswordContainsAnyNumber.invalidNec)),
        1 -> noUnderscorePasswordGen(validPasswordGen(mandatoryCharsGens, minimumLength)).map(
          (_, PasswordContainsAnUnderscore.invalidNec),
        ),
        1 -> allErrorsGen(minimumLength).map(
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
        1 -> validPasswordGen(mandatoryCharsGens, minimumLength).map(x => (x, x.validNec)),
      )
    yield TestCase(
      password,
      expectedValidation,
      expectedValidation match
        case Validated.Valid(_) => true
        case Invalid(_) => false
      ,
      expectedValidation match
        case Validated.Valid(_) => true
        case Validated.Invalid(errors) => errors.length == 1,
    )

  private val secondRuleSetTestCaseGen: Gen[TestCase] =
    val minimumLength = 7
    val mandatoryCharsGens = List(Gen.alphaLowerChar, Gen.alphaUpperChar, Gen.numChar)
    for (password, expectedValidation) <- Gen.frequency(
        1 -> noMinimumLengthPasswordGen(mandatoryCharsGens).map(
          (_, PasswordHasMinimumLength(minimumLength).invalidNec),
        ),
        1 -> noUpperCasePasswordGen(validPasswordGen(mandatoryCharsGens, minimumLength)).map(
          (_, PasswordContainsAtLeastOneUpperCaseLetter.invalidNec),
        ),
        1 -> noLowerCasePasswordGen(validPasswordGen(mandatoryCharsGens, minimumLength)).map(
          (_, PasswordContainsAtLeastOneLowerCaseLetter.invalidNec),
        ),
        1 -> noNumberPasswordGen(validPasswordGen(mandatoryCharsGens, minimumLength))
          .map((_, PasswordContainsAnyNumber.invalidNec)),
        1 -> allErrorsGen(minimumLength).map(
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
        1 -> validPasswordGen(mandatoryCharsGens, minimumLength).map(x => (x, x.validNec)),
      )
    yield TestCase(
      password,
      expectedValidation,
      expectedValidation match
        case Validated.Valid(_) => true
        case Invalid(_) => false
      ,
      expectedValidation match
        case Validated.Valid(_) => true
        case Validated.Invalid(errors) => errors.length == 1,
    )

  private val thirdRuleSetTestCaseGen: Gen[TestCase] =
    val minimumLength = 17
    val mandatoryCharsGens = List(Gen.alphaLowerChar, Gen.alphaUpperChar, Gen.const('_'))
    for (password, expectedValidation) <- Gen.frequency(
        1 -> noMinimumLengthPasswordGen(mandatoryCharsGens).map(
          (_, PasswordHasMinimumLength(minimumLength).invalidNec),
        ),
        1 -> noUpperCasePasswordGen(validPasswordGen(mandatoryCharsGens, minimumLength)).map(
          (_, PasswordContainsAtLeastOneUpperCaseLetter.invalidNec),
        ),
        1 -> noLowerCasePasswordGen(validPasswordGen(mandatoryCharsGens, minimumLength)).map(
          (_, PasswordContainsAtLeastOneLowerCaseLetter.invalidNec),
        ),
        1 -> noUnderscorePasswordGen(validPasswordGen(mandatoryCharsGens, minimumLength)).map(
          (_, PasswordContainsAnUnderscore.invalidNec),
        ),
        1 -> allErrorsGen(minimumLength).map(
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
        1 -> validPasswordGen(mandatoryCharsGens, minimumLength).map(x => (x, x.validNec)),
      )
    yield TestCase(
      password,
      expectedValidation,
      expectedValidation match
        case Validated.Valid(_) => true
        case Invalid(_) => false
      ,
      expectedValidation match
        case Validated.Valid(_) => true
        case Validated.Invalid(errors) => errors.length == 1,
    )

  private val fourthRuleSetTestCaseGen: Gen[TestCase] =
    val minimumLength = 9
    val mandatoryCharsGens = List(Gen.alphaUpperChar, Gen.numChar, Gen.const('_'))
    for (password, expectedValidation) <- Gen.frequency(
        1 -> noMinimumLengthPasswordGen(mandatoryCharsGens).map(
          (_, PasswordHasMinimumLength(minimumLength).invalidNec),
        ),
        1 -> noUpperCasePasswordGen(validPasswordGen(mandatoryCharsGens, minimumLength)).map(
          (_, PasswordContainsAtLeastOneUpperCaseLetter.invalidNec),
        ),
        1 -> noNumberPasswordGen(validPasswordGen(mandatoryCharsGens, minimumLength))
          .map((_, PasswordContainsAnyNumber.invalidNec)),
        1 -> noUnderscorePasswordGen(validPasswordGen(mandatoryCharsGens, minimumLength)).map(
          (_, PasswordContainsAnUnderscore.invalidNec),
        ),
        1 -> allErrorsGen(minimumLength).map(
          (
            _,
            Invalid(
              NonEmptyChain(
                PasswordHasMinimumLength(minimumLength),
                PasswordContainsAtLeastOneUpperCaseLetter,
                PasswordContainsAnyNumber,
                PasswordContainsAnUnderscore,
              ),
            ),
          ),
        ),
        1 -> validPasswordGen(mandatoryCharsGens, minimumLength).map(x => (x, x.validNec)),
      )
    yield TestCase(
      password,
      expectedValidation,
      expectedValidation match
        case Validated.Valid(_) => true
        case Invalid(_) => false
      ,
      expectedValidation match
        case Validated.Valid(_) => true
        case Validated.Invalid(errors) => errors.length == 1,
    )
