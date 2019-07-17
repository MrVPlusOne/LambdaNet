package lambdanet.utils

import ammonite.ops.{pwd, read}

object ReportFinish {
  def main(args: Array[String]): Unit = {
    val (name, emailService) = readEmailInfo()

    emailService.sendMail(emailService.userEmail)(
      s"TypingNet: Training process on $name has stopped",
      "The training process has stopped.",
    )
  }

  type MachineName = String
  def readEmailInfo(taskName: String): (MachineName, EmailService) = {
    val emailFile = pwd / "configs" / "emails.txt"
    println(s"reading email credentials from '$emailFile'...")
    val Array(email, password, name) = read(emailFile).trim.split("\n")
    s"$name-$taskName" -> EmailService(email, password)
  }
}
