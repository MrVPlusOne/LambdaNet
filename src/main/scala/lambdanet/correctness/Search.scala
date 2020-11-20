package lambdanet.correctness

trait Search {}

trait Correction extends (Assignment => Assignment)

trait Objective extends (Assignment => Double)
