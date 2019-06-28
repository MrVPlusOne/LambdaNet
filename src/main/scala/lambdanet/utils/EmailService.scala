package lambdanet.utils

import courier._
import courier.Defaults._

import scala.concurrent.{Await, TimeoutException}
import scala.concurrent.duration.Duration

case class EmailService(userEmail: String, password: String) {
  val Array(user, domain) = userEmail.split("@")

  def sendMail(
      targetEmail: String,
      timeOutSeconds: Int = 10,
  )(subject: String, msg: String): Unit = {
    val Array(targetUser, targetDomain) = targetEmail.split("@")

    val mailer = Mailer("smtp.gmail.com", 587)
      .auth(true)
      .as(s"$user@$domain", password)
      .startTls(true)()

    try {
      Await.result(
        mailer(
          Envelope
            .from(user `@` domain)
            .to(targetUser `@` targetDomain)
            .subject(subject)
            .content(Text(msg)),
        ),
        Duration(s"${timeOutSeconds}s"),
      )
    } catch {
      case _: TimeoutException =>
        throw new Exception(
          s"send mail {subject: '$subject', msg: '$msg'} time out.",
        )
    }
  }
}
