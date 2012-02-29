package nl.up.property.resolver

import org.junit.runner.RunWith

import org.specs2.matcher._
import org.specs2.mutable._

import scala.collection.immutable.TreeMap
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PropertyResolverTest extends Specification /*with ScalaCheck*/ {

  import PropertyResolver._

  "PropertyResolver" should {
    "resolve single reference" in {
      val props = Map(
        ("jndiname" -> "${jndi.ref}"),
        ("jndi.ref" -> "MyQueue"))
      val resolved = resolve(props)
      sort(resolved) must_== TreeMap(("jndi.ref", "MyQueue"), ("jndiname", "MyQueue"))
    }
    "resolve multiple references" in {
      val props = Map(
        ("name" -> "${first.name} ${last.name}"),
        ("first.name" -> "Elton"),
        ("last.name" -> "John"))
      val resolved = resolve(props)
      sort(resolved) must_== TreeMap(("first.name" -> "Elton"), ("last.name" -> "John"), ("name", "Elton John"))
    }
    "resolve chained references" in {
      val props = Map(
        ("a" -> "link to ${b}"),
        ("b" -> "another link to ${c}"),
        ("c" -> "target"))
      val resolved = resolve(props)
      sort(resolved) must_== TreeMap(("c", "target"), ("a", "link to another link to target"), ("b", "another link to target"))
    }
    "BONUS: resolve cyclic references" in {
      val props = Map(
        ("a" -> "Hoe dacht je ${b}"),
        ("b" -> "${a} dit precies op te lossen?"))
      //optionally fill in blanks
      val resolved = resolve(props)
      sort(resolved) must_== TreeMap(("a", "Hoe dacht je dit precies op te lossen?"), ("b", "Hoe dacht je dit precies op te lossen?"))
    }
    "BONUS: resolve chained cyclic references" in {
      val props = Map(
        ("a" -> "a points to ${b}"),
        ("b" -> "b points to ${c}"),
        ("c" -> "c points to ${a}"))
      //optionally don't resolve reference
      val resolved = resolve(props)
      sort(resolved) must_== TreeMap(("a", "a points to b points to c points to"), ("b", "b points to c points to a points to"), ("c", "c points to a points to b points to"))
//      resolve(props) must throwA[IllegalArgumentException]
    }
    "BONUS: not resolve non-existant references" in {
      val props = Map(
        ("a" -> "a points to ${nowhere}"))
      //optionally leave reference unresolved
      resolve(props) must throwA[IllegalArgumentException]
    }
  }

  def sort(map: Map[String, String]) = TreeMap(map.toSeq: _*)
}

