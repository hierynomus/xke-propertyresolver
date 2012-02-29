package nl.up.property.resolver
import scala.collection.mutable.{ Map => MMap }
import java.util.Properties
import scala.io.Source
import java.io.File
import scala.io.Codec
import util.parsing.combinator.RegexParsers
import scala.None
import collection.immutable.List

/**
 * Trait to resolve placeholders contained in Maps.
 */
trait PropertyResolver {

  /**
   * Resolves placeholders with the format ${my.place.holder} contained in the value of one or many maps.
   * Example 1 single reference:
   * db.url=${prod.db.config}
   * prod.db.config=jdbc:oracle:thin
   *
   * Output:
   * db.url=jdbc:oracle:thin
   * prod.db.config=jdbc:oracle:thin
   *
   * Example 2 multiple references:
   * name=${first.name} ${last.name}
   * first.name=Tom
   * last.name=Hanks
   *
   * Output:
   * name=Tom Hanks
   * first.name=Tom
   * last.name=Hanks
   *
   * Example 3 chained references:
   * host=${host.ref}
   * host.ref=${test.host}
   * test.host=test.server.com
   *
   * Output:
   * host=test.server.com
   * host.ref=test.server.com
   * test.host=test.server.com
   * 
   * BONUS: 
   * 1. detect cyclic references: -> throw Exception or don't resolve
   * 2. detect non-existing references: -> throw Exception or don't resolve
   */
  def resolve(inputMaps: Map[String, String]*): Map[String, String] = {
    val consolidated : Map[String,  String] = Map(inputMaps.flatten: _*)
    def resolved: Map[Placeholder, Full] = startResolve(buildLookup(consolidated))
    resolved.map {case (p, f) => (p.w, f.words.collect({case Some(x: SimpleWord) => x.w}).mkString(" "))}
  }

  def startResolve(map: Map[Placeholder,  Full]): Map[Placeholder, Full] = {
    map.map { case (p, f) => (p, resolveDeep(p, f, map)) }
  }

  def resolveDeep(placeholder: Placeholder, full: Full, map: Map[Placeholder, Full]): Full = {
    Full(full.words.map(_ match {
      case Some(x: Placeholder) =>
        if (placeholder != x)
          resolveDeep(placeholder, lookup(x, map), map).words
        else List(None)
      case Some(x: SimpleWord) => List(Some(x))
      case None => List(None)
    }).flatten)
  }

  def buildLookup(map: Map[String, String]): Map[Placeholder, Full] = {
    map.map {case (k, v) => (Placeholder(k), parse(v)) }
  }

  def lookup(p: Placeholder, map: Map[Placeholder, Full]): Full = {
    map.get(p) match {
      case Some(x) => x
      case _ => throw new IllegalArgumentException("None")
    }
  }

  def parse(s: String) = PlaceholderParser.parse(PlaceholderParser.full, s).get
}

sealed trait Word
case class Placeholder(w: String) extends Word 
case class SimpleWord(w: String) extends Word

case class Full(words: List[Option[Word]])

object PlaceholderParser extends RegexParsers {
  def word =  "[A-Za-z0-9?.]+".r ^^ { case w => SimpleWord(w) }
  def placeholder = "${" ~>  "[A-Za-z0-9.]+".r <~ "}" ^^ { case s: String => Placeholder(s) }
  def wordOrPlaceholder = word|placeholder
  def full = rep(wordOrPlaceholder) ^^ { case wp: List[Word] => Full(wp.map(Some(_))) }
}

/**
 * ====================================================================
 * Helper class/traits to load properties from file and execute as Main
 * ====================================================================
 */

/**
 * Trait to load property files and combine them into a Map
 */
trait PropertyLoader {
  import scala.collection.JavaConversions._
  /**
   * Load properties file(s) and convert them into a Map
   */
  def load(paths: String*): Map[String, String] = {
    val maps = paths.map { loadFromPath }
    Map(maps.toSeq.flatten: _*)
  }

  private def loadFromPath(path: String): MMap[String, String] = {
    val prop = new Properties()
    val file = new File(path)
    if (file.exists()) {
      prop.load(Source.fromFile(file)(Codec.UTF8).bufferedReader())
    } else {
      prop.load(Thread.currentThread().getContextClassLoader.getResourceAsStream(path))
    }
    prop
  }
}

/**
 * Resolver combining all relevant traits
 */
object PropertyResolver extends PropertyResolver with PropertyLoader

/**
 * Main class
 */
object PropertyResolverMain {
  import PropertyResolver._
  def main(args: Array[String]): Unit = {
    val maps = load(args: _*)
    val resolved = resolve(maps)
    resolved foreach { case (k, v) => println("%s=%s" format (k, v)) }
  }
}
