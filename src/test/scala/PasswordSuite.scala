package es.eriktorr.password_validation

import PasswordSuite.*
import PasswordValidation.AllErrorsOr
import PasswordValidationError.*
import Verifiable.given_Verifiable_AllErrorsOr

import cats.data.{NonEmptyChain, Validated}
import cats.syntax.validated.*
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import scala.jdk.CollectionConverters.*

final class PasswordSuite extends ScalaCheckSuite:

  property("it should validate a password with the first rule set") {
    forAll(firstRuleSetTestCaseGen) { testCase =>
      checkWith(Password.withFirstRuleSetFrom, testCase)
    }
  }

  property("it should validate a password with the second rule set") {
    forAll(secondRuleSetTestCaseGen) { testCase =>
      checkWith(Password.withSecondRuleSetFrom, testCase)
    }
  }

  property("it should validate a password with the third rule set") {
    forAll(thirdRuleSetTestCaseGen) { testCase =>
      checkWith(Password.withThirdRuleSetFrom, testCase)
    }
  }

  property("it should validate a password with the fourth rule set") {
    forAll(fourthRuleSetTestCaseGen) { testCase =>
      checkWith(Password.withFourthRuleSetFrom, testCase)
    }
  }

  private[this] def checkWith(
      smartConstructorUnderTest: String => AllErrorsOr[Password],
      testCase: TestCase,
  ) =
    val actualValidation = smartConstructorUnderTest(testCase.password)
    actualValidation == testCase.expectedValidation
    && actualValidation.isValid == testCase.expectedIsValid
    && actualValidation.isValidRelaxed == testCase.expectedIsValidRelaxed

object PasswordSuite:

  private[this] val random = scala.util.Random

  private[this] def passwordGen(
      mandatoryCharsGens: List[Gen[Char]],
      minimumLength: Int,
  ): Gen[String] = for
    mandatoryChars <- Gen.sequence(mandatoryCharsGens).map(_.asScala.toList)
    additionalCharsN <- Gen.choose(minimumLength - mandatoryChars.length, minimumLength + 10)
    additionalChars <- Gen.containerOfN[List, Char](additionalCharsN, Gen.asciiPrintableChar)
  yield random.shuffle(mandatoryChars ++ additionalChars).mkString

  private[this] def noMinimumLengthPasswordGen(
      mandatoryCharsGens: List[Gen[Char]],
      minimumLength: Int,
  ): Gen[String] =
    for
      mandatoryChars <- Gen.sequence(mandatoryCharsGens).map(_.asScala.toList)
      _ = assert(mandatoryChars.length < minimumLength)
    yield random.shuffle(mandatoryChars).mkString

  private[this] def noUpperCasePasswordGen(validPasswordGen: Gen[String]): Gen[String] =
    validPasswordGen.map(_.toLowerCase.nn)

  private[this] def noLowerCasePasswordGen(validPasswordGen: Gen[String]): Gen[String] =
    validPasswordGen.map(_.toUpperCase.nn)

  private[this] def noNumberPasswordGen(validPasswordGen: Gen[String]): Gen[String] = for
    password <- validPasswordGen
    replacement <- Gen.alphaChar.map(_.toString)
  yield password.replaceAll("[0-9]", replacement).nn

  private[this] def noUnderscorePasswordGen(validPasswordGen: Gen[String]): Gen[String] = for
    password <- validPasswordGen
    replacement <- Gen.alphaNumChar.map(_.toString)
  yield password.replaceAll("_", replacement).nn

  private[this] def allErrorsGen(minimumLength: Int): Gen[String] =
    Gen
      .frequency(
        1 -> Gen
          .choose(1, minimumLength - 1)
          .flatMap(n => Gen.containerOfN[List, Char](n, Gen.oneOf('.', ',', '|', '[')))
          .map(_.mkString),
        1 -> "",
      )

  final private case class TestCase(
      password: String,
      expectedValidation: AllErrorsOr[Password],
      expectedIsValid: Boolean,
      expectedIsValidRelaxed: Boolean,
  )

  private object TestCase:
    def from(password: String, expectedValidation: AllErrorsOr[Password]): TestCase =
      TestCase(
        password,
        expectedValidation,
        expectedValidation match
          case Validated.Valid(_) => true
          case Validated.Invalid(_) => false
        ,
        expectedValidation match
          case Validated.Valid(_) => true
          case Validated.Invalid(errors) => errors.length == 1,
      )

  private val firstRuleSetTestCaseGen: Gen[TestCase] =
    val minimumLength = 9
    val mandatoryCharsGens =
      List(Gen.alphaLowerChar, Gen.alphaUpperChar, Gen.numChar, Gen.const('_'))
    val validPasswordGen = passwordGen(mandatoryCharsGens, minimumLength)
    for (password, expectedValidation) <- Gen.frequency(
        1 -> noMinimumLengthPasswordGen(mandatoryCharsGens, minimumLength).map(
          (_, PasswordHasMinimumLength(minimumLength).invalidNec),
        ),
        1 -> noUpperCasePasswordGen(validPasswordGen).map(
          (_, PasswordContainsAtLeastOneUpperCaseLetter.invalidNec),
        ),
        1 -> noLowerCasePasswordGen(validPasswordGen).map(
          (_, PasswordContainsAtLeastOneLowerCaseLetter.invalidNec),
        ),
        1 -> noNumberPasswordGen(validPasswordGen)
          .map((_, PasswordContainsAnyNumber.invalidNec)),
        1 -> noUnderscorePasswordGen(validPasswordGen).map(
          (_, PasswordContainsAnUnderscore.invalidNec),
        ),
        1 -> allErrorsGen(minimumLength).map(
          (
            _,
            Validated.Invalid(
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
        1 -> validPasswordGen.map(x => (x, Password.unsafeFrom(x).validNec)),
      )
    yield TestCase.from(password, expectedValidation)

  private val secondRuleSetTestCaseGen: Gen[TestCase] =
    val minimumLength = 7
    val mandatoryCharsGens = List(Gen.alphaLowerChar, Gen.alphaUpperChar, Gen.numChar)
    val validPasswordGen = passwordGen(mandatoryCharsGens, minimumLength)
    for (password, expectedValidation) <- Gen.frequency(
        1 -> noMinimumLengthPasswordGen(mandatoryCharsGens, minimumLength).map(
          (_, PasswordHasMinimumLength(minimumLength).invalidNec),
        ),
        1 -> noUpperCasePasswordGen(validPasswordGen).map(
          (_, PasswordContainsAtLeastOneUpperCaseLetter.invalidNec),
        ),
        1 -> noLowerCasePasswordGen(validPasswordGen).map(
          (_, PasswordContainsAtLeastOneLowerCaseLetter.invalidNec),
        ),
        1 -> noNumberPasswordGen(validPasswordGen)
          .map((_, PasswordContainsAnyNumber.invalidNec)),
        1 -> allErrorsGen(minimumLength).map(
          (
            _,
            Validated.Invalid(
              NonEmptyChain(
                PasswordHasMinimumLength(minimumLength),
                PasswordContainsAtLeastOneUpperCaseLetter,
                PasswordContainsAtLeastOneLowerCaseLetter,
                PasswordContainsAnyNumber,
              ),
            ),
          ),
        ),
        1 -> validPasswordGen.map(x => (x, Password.unsafeFrom(x).validNec)),
      )
    yield TestCase.from(password, expectedValidation)

  private val thirdRuleSetTestCaseGen: Gen[TestCase] =
    val minimumLength = 17
    val mandatoryCharsGens = List(Gen.alphaLowerChar, Gen.alphaUpperChar, Gen.const('_'))
    val validPasswordGen = passwordGen(mandatoryCharsGens, minimumLength)
    for (password, expectedValidation) <- Gen.frequency(
        1 -> noMinimumLengthPasswordGen(mandatoryCharsGens, minimumLength).map(
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
        1 -> allErrorsGen(minimumLength).map(
          (
            _,
            Validated.Invalid(
              NonEmptyChain(
                PasswordHasMinimumLength(minimumLength),
                PasswordContainsAtLeastOneUpperCaseLetter,
                PasswordContainsAtLeastOneLowerCaseLetter,
                PasswordContainsAnUnderscore,
              ),
            ),
          ),
        ),
        1 -> validPasswordGen.map(x => (x, Password.unsafeFrom(x).validNec)),
      )
    yield TestCase.from(password, expectedValidation)

  private val fourthRuleSetTestCaseGen: Gen[TestCase] =
    val minimumLength = 9
    val mandatoryCharsGens = List(Gen.alphaUpperChar, Gen.numChar, Gen.const('_'))
    val validPasswordGen = passwordGen(mandatoryCharsGens, minimumLength)
    for (password, expectedValidation) <- Gen.frequency(
        1 -> noMinimumLengthPasswordGen(mandatoryCharsGens, minimumLength).map(
          (_, PasswordHasMinimumLength(minimumLength).invalidNec),
        ),
        1 -> noUpperCasePasswordGen(validPasswordGen).map(
          (_, PasswordContainsAtLeastOneUpperCaseLetter.invalidNec),
        ),
        1 -> noNumberPasswordGen(validPasswordGen)
          .map((_, PasswordContainsAnyNumber.invalidNec)),
        1 -> noUnderscorePasswordGen(validPasswordGen).map(
          (_, PasswordContainsAnUnderscore.invalidNec),
        ),
        1 -> allErrorsGen(minimumLength).map(
          (
            _,
            Validated.Invalid(
              NonEmptyChain(
                PasswordHasMinimumLength(minimumLength),
                PasswordContainsAtLeastOneUpperCaseLetter,
                PasswordContainsAnyNumber,
                PasswordContainsAnUnderscore,
              ),
            ),
          ),
        ),
        1 -> validPasswordGen.map(x => (x, Password.unsafeFrom(x).validNec)),
      )
    yield TestCase.from(password, expectedValidation)
