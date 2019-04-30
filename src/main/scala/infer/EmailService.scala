package infer

import courier._
import Defaults._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

case class EmailService(userEmail: String, password: String) {
  val Array(user, domain) = userEmail.split("@")

  def sendMail(
      targetEmail: String,
      timeOutSeconds: Int = 10
  )(subject: String, msg: String): Unit = {
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
  }
}
