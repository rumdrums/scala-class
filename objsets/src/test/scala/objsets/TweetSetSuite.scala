package objsets

import objsets.GoogleVsApple.google
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

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

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      trends.foreach(f => println("Tweet = " + f))
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

  test("mostRetweeted works") {
    val greatest = new Tweet("e", "e body greatest", 100)
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body 1", 21))
    val set3 = set2.incl(new Tweet("b", "b body 2", 20))
    val set4 = set3.incl(new Tweet("c", "c body 3", 19))
    val set5 = set4.incl(greatest)
    val set6 = set5.incl(new Tweet("d", "d body 4", 1))
    set6.foreach(f => println(f))
    assert(set6.mostRetweeted == greatest)
  }

  test("google tweets") {
    println(GoogleVsApple.googleTweets.foreach(f => println(f)))
  }

  test("apple tweets") {
    println(GoogleVsApple.appleTweets.foreach(f => println(f)))
  }

  test("filter misc") {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body 1", 21))
    val set3 = set2.incl(new Tweet("b", "b body 2", 28))
    val set4 = set3.incl(new Tweet("c", "c body 3", 19))
    val filtered = set4.filter(t => t.retweets % 7 == 0)
    filtered.foreach(t => println(t))
  }
}
