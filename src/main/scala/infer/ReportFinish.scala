package infer

import ammonite.ops.{pwd, read}

object ReportFinish {
  def main(args: Array[String]): Unit = {
    val emailService = {
      println("reading email credentials from 'emails.txt'...")
      val Array(email, password) = read(pwd / "emails.txt").trim.split("\n")
      EmailService(email, password)
    }

    emailService.sendMail(emailService.userEmail)(
      "TypingNet: Training process stopped",
      "The training process has stopped."
    )
  }
}
