package proyecto

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import Similitud._

/**
 * Pruebas de contrato de la versión secuencial: una por función, con los
 * valores de los dos ejemplos del enunciado. No se modifican.
 *
 * La rúbrica pide además cinco casos propios por función, distintos de estos y
 * de los ejemplos del enunciado. Van al final de este archivo o en un archivo
 * nuevo de esta misma carpeta; las dos formas valen.
 */
@RunWith(classOf[JUnitRunner])
class SimilitudTest extends AnyFunSuite {

  val eps = 1e-9

  // Ejemplo 1 del enunciado, con las palabras ya normalizadas para que estas
  // pruebas no dependan unas de otras.
  val d0: Documento = ("d0", Vector("el", "gato", "duerme", "en", "el", "sofa"))
  val d1: Documento = ("d1", Vector("el", "gato", "duerme", "en", "la", "alfombra"))
  val d2: Documento = ("d2", Vector("la", "lluvia", "moja", "el", "parque"))
  val c1: Corpus = Vector(d0, d1, d2)

  // Ejemplo 2 del enunciado.
  val e0: Documento = ("e0", Vector("la", "funcion", "recursiva", "termina",
    "cuando", "llega", "al", "caso", "base"))
  val e1: Documento = ("e1", Vector("toda", "funcion", "recursiva", "termina",
    "cuando", "llega", "al", "caso", "base"))
  val e2: Documento = ("e2", Vector("el", "paralelismo", "divide", "el",
    "trabajo", "en", "tareas", "independientes"))
  val e3: Documento = ("e3", Vector("el", "paralelismo", "divide", "el",
    "trabajo", "en", "hilos", "independientes"))
  val c2: Corpus = Vector(e0, e1, e2, e3)

  test("normalizar: baja a minúsculas y parte en palabras") {
    assert(normalizar("El Gato Duerme") == Vector("el", "gato", "duerme"))
  }

  test("normalizar: la puntuación separa y no deja huecos") {
    assert(normalizar("hola, mundo; adios.") ==
      Vector("hola", "mundo", "adios"))
  }

  test("normalizar: los dígitos se conservan como parte de la palabra") {
    assert(normalizar("RFC 2026 v1") == Vector("rfc", "2026", "v1"))
  }

  test("normalizar: un texto sin letras ni dígitos da el vector vacío") {
    assert(normalizar("!!! ??? ---") == Vector())
    assert(normalizar("") == Vector())
  }

  test("normalizar: los separadores repetidos y de los extremos no dejan vacías") {
    assert(normalizar("  el  gato  ") == Vector("el", "gato"))
    assert(normalizar("a--b") == Vector("a", "b"))
  }


  test("normalizar: conserva las letras con tilde y la ene") {
    assert(normalizar("Función Ágil, niño") == Vector("función", "ágil", "niño"))
  }

  test("fragmentos: son las ventanas consecutivas, en orden") {
    val ts = Vector("a", "b", "c", "d")
    assert(fragmentos(ts, 2) ==
      Vector(Vector("a", "b"), Vector("b", "c"), Vector("c", "d")))
  }

  test("fragmentos: con k = 1 cada palabra es un fragmento") {
    val ts = Vector("el", "gato", "duerme")
    assert(fragmentos(ts, 1) ==
      Vector(Vector("el"), Vector("gato"), Vector("duerme")))
  }

  test("fragmentos: con k igual al largo hay un solo fragmento") {
    val ts = Vector("el", "gato", "duerme")
    assert(fragmentos(ts, 3) == Vector(Vector("el", "gato", "duerme")))
  }

  test("fragmentos: si hay menos palabras que k el resultado es vacío") {
    val ts = Vector("el", "gato")
    assert(fragmentos(ts, 3) == Vector())
    assert(fragmentos(Vector(), 1) == Vector())
    assert(fragmentos(ts, 0) == Vector())
  }

  test("fragmentos: la cantidad es t - k + 1") {
    val ts = Vector("p0", "p1", "p2", "p3", "p4", "p5")
    assert(fragmentos(ts, 4).length == 3)
    assert(fragmentos(ts, 6).length == 1)
  }

  test("fragmentos: conserva los fragmentos repetidos, uno por posición") {
    val ts = Vector("a", "b", "a", "b")
    assert(fragmentos(ts, 2) ==
      Vector(Vector("a", "b"), Vector("b", "a"),
             Vector("a", "b"), Vector("b", "a")))
  }

  test("huella: el ejemplo 1 tiene 4, 4 y 3 fragmentos") {
    assert(huella(d0, 3).size == 4)
    assert(huella(d1, 3).size == 4)
    assert(huella(d2, 3).size == 3)
  }

  test("huella: sin fragmentos repetidos el tamaño es t - k + 1") {
    val d: Documento = ("h0", Vector("a", "b", "c", "d"))
    assert(huella(d, 2) == Set(Vector("a", "b"), Vector("b", "c"),
      Vector("c", "d")))
  }

  test("huella: un fragmento repetido cuenta una sola vez") {
    val d: Documento = ("h1", Vector("x", "y", "x", "y", "x"))
    assert(huella(d, 2) == Set(Vector("x", "y"), Vector("y", "x")))
  }

  test("huella: si todas las palabras son iguales hay un solo fragmento") {
    val d: Documento = ("h2", Vector("p", "p", "p", "p"))
    assert(huella(d, 3) == Set(Vector("p", "p", "p")))
  }

  test("huella: si k supera el largo del documento la huella es vacía") {
    val d: Documento = ("h3", Vector("el", "gato"))
    assert(huella(d, 3).isEmpty)
    assert(huella(("h4", Vector()), 1).isEmpty)
  }

  test("huella: usa las palabras del documento, no su identificador") {
    val d: Documento = ("d0", Vector("uno", "dos", "tres"))
    assert(huella(d, 2) == Set(Vector("uno", "dos"), Vector("dos", "tres")))
  }

  test("huellas: hay una huella por documento") {
    assert(huellas(c1, 3).length == 3)
  }
  test("huellas: un corpus vacío da un vector vacío") {
    assert(huellas(Vector(), 2) == Vector())
  }

  test("huellas: la i-ésima huella corresponde al i-ésimo documento") {
    val c: Corpus = Vector(("a", Vector("x", "y", "z")), ("b", Vector("m", "n")))
    val hs = huellas(c, 2)
    assert(hs(0) == Set(Vector("x", "y"), Vector("y", "z")))
    assert(hs(1) == Set(Vector("m", "n")))
  }

  test("huellas: un documento más corto que k da huella vacía") {
    val c: Corpus = Vector(("a", Vector("x", "y")), ("b", Vector("p", "q", "r")))
    val hs = huellas(c, 3)
    assert(hs(0).isEmpty && hs(1).size == 1)
  }

  test("huellas: con k = 1 la huella es el vocabulario del documento") {
    val c: Corpus = Vector(("a", Vector("x", "y", "x", "x")))
    assert(huellas(c, 1)(0) == Set(Vector("x"), Vector("y")))
  }

  test("huellas: documentos idénticos producen huellas iguales") {
    val d = Vector("a", "b", "c", "d")
    val c: Corpus = Vector(("u", d), ("v", d))
    val hs = huellas(c, 2)
    assert(hs(0) == hs(1) && hs.length == 2)
  }

  test("jaccard: ejemplo 2, las dos parejas dan 0.75 y 0.5") {
    assert(math.abs(jaccard(huella(e0, 3), huella(e1, 3)) - 0.75) < eps)
    assert(math.abs(jaccard(huella(e2, 3), huella(e3, 3)) - 0.5) < eps)
  }
  test("jaccard: huellas iguales dan 1.0") {
    val h: Huella = Set(Vector("a"), Vector("b"))
    assert(math.abs(jaccard(h, h) - 1.0) < eps)
  }

  test("jaccard: huellas disjuntas dan 0.0") {
    assert(jaccard(Set(Vector("a")), Set(Vector("b"))) == 0.0)
  }

  test("jaccard: dos huellas vacías dan 0.0 sin dividir por cero") {
    assert(jaccard(Set(), Set()) == 0.0)
  }

  test("jaccard: una vacía y otra no dan 0.0") {
    assert(jaccard(Set(), Set(Vector("a"))) == 0.0)
  }

  test("jaccard: una huella contenida en otra da |A| / |B|") {
    val a: Huella = Set(Vector("a"), Vector("b"))
    val b: Huella = Set(Vector("a"), Vector("b"), Vector("c"), Vector("d"))
    assert(math.abs(jaccard(a, b) - 0.5) < eps)
  }




  test("parejas: hay n (n - 1) / 2") {
    assert(parejas(5).length == 10)
  }
  test("parejas: con n = 0 y n = 1 no hay parejas") {
    assert(parejas(0) == Vector())
    assert(parejas(1) == Vector())
  }

  test("parejas: con n = 2 solo está (0, 1)") {
    assert(parejas(2) == Vector((0, 1)))
  }

  test("parejas: con n = 3 salen tres, en orden") {
    assert(parejas(3) == Vector((0, 1), (0, 2), (1, 2)))
  }

  test("parejas: con n = 4 salen seis, en orden") {
    assert(parejas(4) == Vector((0, 1), (0, 2), (0, 3), (1, 2), (1, 3), (2, 3)))
  }

  test("parejas: todas cumplen i < j, sin repetidas, y son n(n-1)/2") {
    val ps = parejas(8)
    assert(ps.forall(p => p._1 < p._2))
    assert(ps.distinct.length == ps.length)
    assert(ps.length == 8 * 7 / 2)
  }

  test("similitudes: el ejemplo 1 da 1/3, 0 y 0") {
    val hs = huellas(c1, 3)
    val ms = similitudes(hs, parejas(3)).map(_._3)
    assert(math.abs(ms(0) - 2.0 / 6.0) < eps && ms(1) == 0.0 && ms(2) == 0.0)
  }

  test("indiceInvertido: el fragmento inicial de d0 está en d0 y en d1") {
    val idx = indiceInvertido(huellas(c1, 3))
    assert(idx(Vector("el", "gato", "duerme")) == Set(0, 1))
  }

  test("parejasCandidatas: en el ejemplo 2 quedan dos de seis") {
    val idx = indiceInvertido(huellas(c2, 3))
    assert(parejasCandidatas(idx) == Vector((0, 1), (2, 3)))
  }

  test("ranking: ordena de mayor a menor y trunca a p") {
    val ms: Vector[Medicion] = Vector((0, 1, 0.4), (0, 2, 0.1), (1, 2, 0.9))
    assert(ranking(ms, 0.0, 10).map(_._3) == Vector(0.9, 0.4, 0.1))
    assert(ranking(ms, 0.0, 2).length == 2)
  }

  test("detectar: el ejemplo 2 reporta (0, 1) con 0.75 y (2, 3) con 0.5") {
    val r = detectarDirecto(c2, 3, 0.3, 5)
    assert(r.map(m => (m._1, m._2)) == Vector((0, 1), (2, 3)))
    assert(math.abs(r(0)._3 - 0.75) < eps && math.abs(r(1)._3 - 0.5) < eps)
  }

  test("detectar: las dos soluciones coinciden en los ejemplos") {
    assert(detectarDirecto(c1, 3, 0.2, 5) == detectarConIndice(c1, 3, 0.2, 5))
    assert(detectarDirecto(c2, 3, 0.3, 5) == detectarConIndice(c2, 3, 0.3, 5))
  }
}
