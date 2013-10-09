package objsets

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.Random

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("mostRetweeted: empty set") {
    val s1 = new Empty
    intercept[NoSuchElementException] {
      s1.mostRetweeted
    }
  }

  test("mostRetweeted: real set") {
    val s = Seq.fill(1000)(Random.nextInt(1000000))
    var e: TweetSet = new Empty
    val tss = s.foldRight(e)((i, t) => t.incl(new Tweet("user", "text" + i, i)))
    assert(s.max == tss.mostRetweeted.retweets)
  }

  test("descending: empty set") {
    val s1 = new Empty
    assert(s1.descendingByRetweet == Nil)
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty, "empty")
      assert(trends.head.user == "a" || trends.head.user == "b", "bad head")
    }
  }

  test("descending: test like a man") {
    val s = Seq.fill(1000)(Random.nextInt(1000000))
    var e: TweetSet = new Empty
    val tss = s.foldRight(e)((i, t) => t.incl(new Tweet("user", "text" + i, i)))
    val trends = tss.descendingByRetweet
    assert(!trends.isEmpty)
    assert(trends.head.user == "user")
    assert(s.max == trends.head.retweets)
  }
}
