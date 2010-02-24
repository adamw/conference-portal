package pl.softwaremill.model

import xml._

import net.liftweb.mapper._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.textile.TextileParser
import net.liftweb.util.Helpers._
import net.liftweb.util.FieldError

import S._
import SHtml._

import dispatch._

object User extends User with MetaMegaProtoUser[User] {
  override def dbTableName = "users" // define the DB table name
  override def screenWrap = Full(<lift:surround with="default" at="content"><lift:bind /></lift:surround>)

  override def signupFields = firstName :: lastName :: email :: password :: mappedSex :: bio :: Nil

  // define the order fields will appear in forms and output
  override def fieldOrder = List(id, firstName, lastName, email, locale, timezone, password, bio)

  // comment this line out to require email validations
  override def skipEmailValidation = true

  // templates
  override def loginXhtml =
    <span>
      <h1 class="tytul">{S.??("log.in")}</h1>
      <hr class="clear"/>
      <form method="post" action={S.uri}>
        <table>
          <tr><td>{S.??("email.address")}</td><td><user:email /></td></tr>
          <tr><td>{S.??("password")}</td><td><user:password /></td></tr>
          <tr><td><a href={lostPasswordPath.mkString("/", "/", "")}>{S.??("recover.password")}</a></td><td><user:submit /></td></tr>
        </table>
      </form>
    </span>

  override def lostPasswordXhtml =
    <span>
      <h1 class="tytul">{S.??("enter.email")}</h1>
      <hr class="clear"/>
      <form method="post" action={S.uri}>
        <table>
          <tr><td>{S.??("email.address")}</td><td><user:email /></td></tr>
          <tr><td>&nbsp;</td><td><user:submit /></td></tr>
        </table>
      </form>
    </span>

  override def passwordResetXhtml =
    <span>
      <h1 class="tytul">{S.??("reset.your.password")}</h1>
      <hr class="clear"/>
      <form method="post" action={S.uri}>
        <table>
          <tr><td>{S.??("enter.your.new.password")}</td><td><user:pwd/></td></tr>
          <tr><td>{S.??("repeat.your.new.password")}</td><td><user:pwd/></td></tr>
          <tr><td>&nbsp;</td><td><user:submit/></td></tr>
        </table>
      </form>
    </span>

  override def changePasswordXhtml =
    <span>
      <h1 class="tytul">{S.??("change.password")}</h1>
      <hr class="clear"/>
      <form method="post" action={S.uri}>
        <table>
          <tr><td>{S.??("old.password")}</td><td><user:old_pwd /></td></tr>
          <tr><td>{S.??("new.password")}</td><td><user:new_pwd /></td></tr>
          <tr><td>{S.??("repeat.password")}</td><td><user:new_pwd /></td></tr>
          <tr><td>&nbsp;</td><td><user:submit /></td></tr>
        </table>
      </form>
    </span>

  override def editXhtml(user: User) =
    <span>
      <h1 class="tytul">{S.??("edit")}</h1>
      <hr class="clear"/>
      <form method="post" action={S.uri}>
        <table>
          {localForm(user, true)}
          <tr><td>&nbsp;</td><td><user:submit/></td></tr>
        </table>
      </form>
    </span>

  override def signupXhtml(user: User) =
    <span>
      <h1 class="tytul">{S.??("sign.up")}</h1>
      <hr class="clear"/>
      <form method="post" action={S.uri}>
        <table>
          {localForm(user, false)}
          <tr>
            <td>&nbsp;</td>
            <td><lift:embed what="recaptcha" /></td>
          </tr>
          <tr><td>&nbsp;</td><td><user:submit/></td></tr>
        </table>
      </form>
    </span>

  def checkCaptcha(challenge: String, response: String): Boolean = {
    val http = new Http
    val req = :/("api-verify.recaptcha.net") / "verify"
    val remoteAddress = S.request.map(_.request.remoteAddress) openOr ""
    val postParams = Map("privatekey" -> "6LfLawsAAAAAAN0cK1scWz9osc6wHP6E_O2HroAH",
      "remoteip" -> remoteAddress,
      "challenge" -> challenge,
      "response" -> response)

    val rform = req << postParams
    http(rform >~ { s => { s.getLine(1) == "true" } })
  }

  def validateSignup(theUser: User): List[FieldError] = {
    def wrongCaptcha = List(FieldError(id, Text(?("captcha.wrong"))))

    (for (captchaChallenge <- S.param("recaptcha_challenge_field");
          captchaResponse <- S.param("recaptcha_response_field")) yield {
      if (!checkCaptcha(captchaChallenge, captchaResponse))
        wrongCaptcha
      else
        theUser.validate
    }) openOr wrongCaptcha
  }
  
  // TODO: remove after Lift supports a validate signup method
  override def signup = {
    val theUser: User = create
    val theName = signUpPath.mkString("")

    def testSignup() {
      validateSignup(theUser) match {
        case Nil =>
          actionsAfterSignup(theUser)
          S.redirectTo(homePage)

        case xs => S.error(xs) ; signupFunc(Full(innerSignup _))
      }
    }

    def innerSignup = bind("user",
                           signupXhtml(theUser),
                           "submit" -> SHtml.submit(S.??("sign.up"), testSignup _))

    innerSignup
  }
}

class User extends MegaProtoUser[User] {
  def getSingleton = User // what's the "meta" server

  // define an additional field for a personal essay
  object bio extends MappedTextarea(this, 2048) {
    override def textareaRows  = 10
    override def textareaCols = 50
    override def displayName = ?("user.bio")

    override def toForm = {
      // TODO: remove after the NPE is fixed in MappedTextarea
      if (bio.is == null) bio("")

      super.toForm
    }

    def toHtml: NodeSeq = bio.is match {
      case null => NodeSeq.Empty
      case s => TextileParser.parse(s, None).map(_.toHtml).getOrElse(NodeSeq.Empty)
    }
  }

  object mappedSex extends MappedInt(this) {
    override def defaultValue = Sex.Male.id
    override def dbColumnName = "sex"
    override def displayName = ?("user.sex")

    override def _toForm = {
      val options = Sex.map { sex => (sex, ?(sex.toString)) }.toList
      Full(selectObj[Sex.Value](options, Full(sex), sex(_)))
    }
  }

  def sex = Sex(mappedSex.is)
  def sex(newSex: Sex.Value) = mappedSex(newSex.id)
}

object Sex extends Enumeration {
  val Female = Value("sex.female")
  val Male = Value("sex.male")
}
