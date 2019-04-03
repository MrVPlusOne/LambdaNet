package infer

import ammonite.ops.{pwd, read}

object ReportFinish {
  def main(args: Array[String]): Unit = {
    val (name, emailService) = readEmailInfo()

    emailService.sendMail(emailService.userEmail)(
      s"TypingNet: Training process on $name has stopped",
      "The training process has stopped."
    )
  }

  type MachineName = String
  def readEmailInfo(): (MachineName, EmailService) = {
    println("reading email credentials from 'emails.txt'...")
    val Array(email, password, name) = read(pwd / "emails.txt").trim.split("\n")
    name -> EmailService(email, password)
  }
}
