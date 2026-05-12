// =====================================================================
// Ejercicios 3 y 5: Detección y conteo de entidades
// =====================================================================
import java.util.regex.Pattern

/**
 * Responsable de detectar entidades nombradas en texto libre y
 * producir estadísticas sobre ellas.
 */
object Analyzer {

  /**
   * Construye una regex robusta para buscar una entidad dentro del texto.
   *
   * La entidad se escapa con Pattern.quote para que caracteres especiales
   * como ".", "+", "(", ")" o "?" se interpreten literalmente y no como
   * operadores de regex.
   *
   * Los espacios internos de la entidad se reemplazan por "\s+", lo que
   * permite matchear uno o más espacios, tabs o saltos de línea entre palabras.
   * Por ejemplo, "San Juan" puede matchear "San Juan", "san   juan" o
   * "San\nJuan".
   *
   * La regex usa los flags (?iu):
   * - i: ignora mayúsculas/minúsculas.
   * - u: hace que el matching sea Unicode-aware.
   *
   * El lookbehind negativo (?<![\p{L}\p{N}\-_]) exige que antes de la entidad
   * no haya una letra ni un número. Esto evita detectar la entidad dentro
   * de otra palabra.
   *
   * El lookahead negativo (?![\p{L}\p{N}\-_]) exige lo mismo después de la
   * entidad: que no continúe inmediatamente con una letra o número.
   *
   * En conjunto, estos bordes permiten matchear la entidad cuando está
   * separada por espacios, signos de puntuación o inicio/fin de texto,
   * pero evitan falsos positivos dentro de palabras más largas.
   *
   * \p{L} matchea cualquier letra
   * \p{N} matchea cualquier número
   * \- matchea - (se usa \- para escapar el - porque sino se detecta un rango)
   * _ matecha _
   */

  private def entityRegex(entityText: String): scala.util.matching.Regex = {
    val escaped =
      entityText
        .trim
        .split("\\s+")
        .map(Pattern.quote)
        .mkString("\\s+")

    s"(?iu)(?<![\\p{L}\\p{N}\\-_])$escaped(?![\\p{L}\\p{N}\\-_])".r
  }

  /**
   * Detecta las entidades del diccionario que aparecen en el texto dado.
   *
   * @param text       texto a analizar (ej: título o cuerpo de un post)
   * @param dictionary lista de entidades conocidas (cargadas desde los diccionarios)
   * @return lista de entidades cuyo texto aparece en el texto analizado
   *
   * TODO (Ejercicio 3): Implementar este método.
   *
   *   Para cada entidad en el diccionario, verificar si su texto aparece en el
   *   texto del post. Retornar únicamente las entidades que aparecen.
   *
   *   Ejemplo:
   *     text       = "Scala fue creado en EPFL por Martin Odersky"
   *     dictionary = List(
   *                    ProgrammingLanguage("Scala"),
   *                    University("EPFL"),
   *                    Person("Martin Odersky"),
   *                    Person("Ada Lovelace")   ← no aparece en el texto
   *                  )
   *     resultado  = List(
   *                    ProgrammingLanguage("Scala"),
   *                    University("EPFL"),
   *                    Person("Martin Odersky")
   *                  )
   */
  def detectEntities(text: String, dictionary: List[NamedEntity]): List[NamedEntity] = {
    try
    {
      val entityList = dictionary.filter { entity =>
        entityRegex(entity.text).findFirstIn(text).isDefined}
      entityList
    } catch {
      case e: NullPointerException =>
      println(s"El texto o el diccionario es Null")
      List.empty
    }
  }

  /**
   * Cuenta cuántas entidades de cada tipo fueron detectadas.
   *
   * @param entities lista de entidades detectadas
   * @return mapa de entityType → cantidad de apariciones
   *
   * TODO (Ejercicio 5): Implementar este método.
   *
   *   Ejemplo:
   *     entities = List(
   *                  Person("Alan Turing"),
   *                  ProgrammingLanguage("Scala"),
   *                  Person("Ada Lovelace"),
   *                  University("MIT")
   *                )
   *     resultado = Map(
   *                   "Person"              -> 2,
   *                   "ProgrammingLanguage" -> 1,
   *                   "University"          -> 1
   *                 )
   */
  def countByType(entities: List[NamedEntity]): Map[String, Int] = {

    if (entities.nonEmpty) {
      val entityCount = entities.groupBy(e => e.entityType).view.mapValues(_.length).toMap
      entityCount
    } else {
      val entityCount = Map("Person" -> 0,
        "University" -> 0,
        "ProgrammingLanguage" -> 0,
        "Organization" -> 0,
        "Place" -> 0,
        "Tipo Desconocido" -> 0)
      entityCount
    }
  }
}
