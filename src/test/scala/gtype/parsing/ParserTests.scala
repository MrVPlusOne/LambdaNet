package gtype.parsing

import gtype.MyTest
import org.scalatest.WordSpec
import ammonite.ops._

class ParserTests extends WordSpec with MyTest {

  "Expressions parsing test" in {
    val content =
      """
        |3;
        |"abc";
        |true;
        |false;
        |null;
        |[1,2,3];
        |f(a);
        |new A(1,2);
      """.stripMargin

    GStmtParsing.parseContent(content)
  }

  "Statements parsing test" in {
    val content =
      """
        |let x: number = 4;
        |const y = "yyy";
        |let x = {a: 1, b: {c: "x"}};
        |let one = {a: 1}.a;
        |let a = array[1];
        |a + b;
        |a = b + 1;
        |x = (y = 3);
        |!(a==b);
        |v++;
        |(1+1==2)? good: bad;
      """.stripMargin

    GStmtParsing.parseContent(content).foreach(println)
  }
}
