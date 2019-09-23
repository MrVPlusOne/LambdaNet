package lambdanet.utils

import courier._
import courier.Defaults._

import scala.concurrent.{Await, TimeoutException}
import scala.concurrent.duration.Duration

case class EmailService(userEmail: String, password: String) {

  private var warned = false
  def atFirstTime(action: => Unit): Unit = {
    if (!warned) {
      action
    }
    warned = true
  }

  val Array(user, domain) = userEmail.split("@")

  def sendMail(
      targetEmail: String,
      timeOutSeconds: Int = 10
  )(subject: String, msg: String): Unit = {
    try {
      val Array(targetUser, targetDomain) = targetEmail.split("@")

      val mailer = Mailer("smtp.gmail.com", 587)
        .auth(true)
        .as(s"$user@$domain", password)
        .startTls(true)()

      Await.result(
        mailer(
          Envelope
            .from(user `@` domain)
            .to(targetUser `@` targetDomain)
            .subject(subject)
            .content(Text(msg))
        ),
        Duration(s"${timeOutSeconds}s")
      )
    } catch {
      case _: TimeoutException =>
        Console.err.println(
          s"send mail timed out: {subject: '$subject', msg: '$msg'}."
        )
      case e: Exception =>
        Console.err.println(
          s"Failed to send email: {subject: '$subject', msg: '$msg'}. Exception: ${e.getMessage}"
        )
    }
  }
}
