package pl.softwaremill.services

import _root_.scala.xml.{NodeSeq}
import _root_.javax.mail._
import _root_.javax.mail.internet._
import _root_.javax.naming.{Context, InitialContext}
import _root_.java.util.Properties
import net.liftweb._
import net.liftweb.util._
import common._
import actor._

/**
 * Utilities for sending email.
 */
object CustomMailer extends CustomMailerImpl

/**
 * This trait implmenets the mail sending
 */
protected trait CustomMailerImpl {
  sealed abstract class MailTypes
  sealed abstract class MailBodyType extends MailTypes
  case class PlusImageHolder(name: String, mimeType: String, bytes: Array[Byte])

  /**
   * Represents a text/plain mail body. The given text will
   * be encoded as UTF-8 when sent.
   */
  case class PlainMailBodyType(text: String) extends MailBodyType

  /**
   * Represents a text/plain mail body that is encoded with the
   * specified charset
   */
  case class PlainPlusBodyType(text: String, charset: String) extends MailBodyType

  case class XHTMLMailBodyType(text: NodeSeq) extends MailBodyType
  case class XHTMLPlusImages(text: NodeSeq, items: PlusImageHolder*) extends MailBodyType

  sealed abstract class RoutingType extends MailTypes
  sealed abstract class AddressType(val adr: String) extends RoutingType
  case class From(address: String) extends AddressType(address)
  case class To(address: String) extends AddressType(address)
  case class CC(address: String) extends AddressType(address)
  case class Subject(subject: String) extends RoutingType
  case class BCC(address: String) extends AddressType(address)
  case class ReplyTo(address: String) extends AddressType(address)

  implicit def stringToMailBodyType(text: String): MailBodyType = PlainMailBodyType(text)

  implicit def xmlToMailBodyType(html: NodeSeq): MailBodyType = XHTMLMailBodyType(html)

  case class MessageInfo(from: From, subject: Subject, afterSend: () => Unit, info: List[MailTypes])

  implicit def addressToAddress(in: AddressType): Address = new InternetAddress(in.adr)

  implicit def adListToAdArray(in: List[AddressType]): Array[Address] = in.map(a => new InternetAddress(a.adr)).toArray

  /**
   * Passwords cannot be accessed via System.getProperty.  Instead, we
   * provide a means of explicitlysetting the authenticator.
   */
  //def authenticator = authenticatorFunc
  var authenticator: Box[Authenticator] = Empty

  /**
   * Use the mailer resource in your container by specifying the JNDI name
   */
  var jndiName: Box[String] = Empty

  /**
   * Custom properties for the JNDI session
   */
  var customProperties: Map[String, String] = Map()

  lazy val jndiSession: Box[Session] =
  for{
    name <- jndiName
    contextObj <- Helpers.tryo(new InitialContext().lookup("java:comp/env"))
    context <- Box.asA[Context](contextObj)
    sessionObj <- Helpers.tryo(context.lookup(name))
    session <- Box.asA[Session](sessionObj)
  } yield session

  lazy val properties: Properties = {
    val p = System.getProperties.clone.asInstanceOf[Properties]
    customProperties.foreach {case (name, value) => p.put(name, value)}
    // allow the properties file to set/override system properties

    Props.props.foreach {
      case (name, value) =>
        p.setProperty(name, value)
    }
    p
  }

  /**
   * The host that should be used to send mail.
   */
  def host = hostFunc()

  /**
   * To change the way the host is calculated, set this to the function that calcualtes the host name.
   * By default: System.getProperty("mail.smtp.host")
   */
  var hostFunc: () => String = _host _

  private def _host = properties.getProperty("mail.smtp.host") match {
    case null => "localhost"
    case s => s
  }

  def buildProps: Properties = {
    val p = properties.clone.asInstanceOf[Properties]
    p.getProperty("mail.smtp.host") match {
      case null => p.put("mail.smtp.host", host)
      case _ =>
    }

    p
  }

  /**
   * Set the mail.charset property to something other than UTF-8 for non-UTF-8
   * mail.
   */
  lazy val charSet = properties.getProperty("mail.charset") match {
    case null => "UTF-8"
    case x => x
  }

  // def host_=(hostname: String) = System.setProperty("mail.smtp.host", hostname)

  protected class MsgSender extends SpecializedLiftActor[MessageInfo] {
    protected def messageHandler = {
      case MessageInfo(from, subject, afterSend, info) =>
        try {
          val session = authenticator match {
            case Full(a) => jndiSession openOr Session.getInstance(buildProps, a)
            case _ => jndiSession openOr Session.getInstance(buildProps)
          }

          val message = new MimeMessage(session)
          message.setFrom(from)
          message.setRecipients(Message.RecipientType.TO, info.flatMap {case x: To => Some[To](x) case _ => None})
          message.setRecipients(Message.RecipientType.CC, info.flatMap {case x: CC => Some[CC](x) case _ => None})
          message.setRecipients(Message.RecipientType.BCC, info.flatMap {case x: BCC => Some[BCC](x) case _ => None})
          // message.setReplyTo(filter[MailTypes, ReplyTo](info, {case x @ ReplyTo(_) => Some(x); case _ => None}))
          message.setReplyTo(info.flatMap {case x: ReplyTo => Some[ReplyTo](x) case _ => None})
          message.setSubject(subject.subject)
          val bodyTypes = info.flatMap {case x: MailBodyType => Some[MailBodyType](x); case _ => None}
          bodyTypes match {
            case PlainMailBodyType(txt) :: Nil =>
              message.setText(txt)

            case _ =>
              val multiPart = new MimeMultipart("alternative")
              bodyTypes.foreach {
                tab =>
                    val bp = new MimeBodyPart
                    tab match {
                      case PlainMailBodyType(txt) => bp.setText(txt, "UTF-8")
                      case PlainPlusBodyType(txt, charset) => bp.setText(txt, charset)
                      case XHTMLMailBodyType(html) => bp.setContent(html.toString, "text/html; charset=" + charSet)
                      case XHTMLPlusImages(html, img@_*) =>
                        val html_mp = new MimeMultipart("related")
                        val bp2 = new MimeBodyPart
                        bp2.setContent(html.toString, "text/html; charset=" + charSet)
                        html_mp.addBodyPart(bp2)
                        img.foreach {
                          i =>
                              val rel_bpi = new MimeBodyPart
                              rel_bpi.setFileName(i.name)
                              rel_bpi.setContentID(i.name)
                              rel_bpi.setDisposition("inline")
                              rel_bpi.setDataHandler(new _root_.javax.activation.DataHandler(new _root_.javax.activation.DataSource {
                                def getContentType = i.mimeType

                                def getInputStream = new _root_.java.io.ByteArrayInputStream(i.bytes)

                                def getName = i.name

                                def getOutputStream = throw new _root_.java.io.IOException("Unable to write to item")
                              }))
                              html_mp.addBodyPart(rel_bpi)
                        }
                        bp.setContent(html_mp)
                    }
                    multiPart.addBodyPart(bp)
              }
              message.setContent(multiPart);
          }

          CustomMailerImpl.this.performTransportSend(message)

          afterSend()
        } catch {
          case e: Exception => Log.error("Couldn't send mail", e)
        }
    }
  }

  protected def performTransportSend(msg: MimeMessage) = Transport.send(msg)

  protected lazy val msgSender = new MsgSender

  /**
   * Asynchronously send an email.
   */
  def sendMail(from: From, subject: Subject, afterSend: () => Unit, rest: MailTypes*) {
    // forward it to an actor so there's no time on this thread spent sending the message
    msgSender ! MessageInfo(from, subject, afterSend, rest.toList)
  }
}