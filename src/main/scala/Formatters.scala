// =====================================================================
// Ejercicios 4 y 5: Formateo de resultados
// =====================================================================

/**
 * Responsable de convertir los resultados del análisis a texto para mostrar.
 */
object Formatters {

  /**
   * Formatea el análisis NER de un post individual.
   *
   * @param postTitle título del post analizado
   * @param entities  entidades detectadas en ese post
   * @return bloque de texto con el título y las entidades encontradas
   *
   * TODO (Ejercicio 4): Implementar este método.
   *
   *   Usar el método describe de cada entidad para generar la salida.
   *   No es necesario hacer match sobre el tipo concreto de cada entidad:
   *   describe ya funciona correctamente para cualquier subtipo (polimorfismo).
   *
   *   Ejemplo de salida esperada:
   *
   *     Post: "Scala 3 released at EPFL by Martin Odersky"
   *     Entidades detectadas:
   *       [ProgrammingLanguage] Scala
   *       [University] EPFL
   *       [Person] Martin Odersky
   *
   *   Si no se detectaron entidades, mostrar un mensaje indicándolo.
   */
  def formatNERResult(postTitle: String, entities: List[NamedEntity]): String = {
    val title = s"\nPost: \"${postTitle}\"\n"
    if (entities.isEmpty) {
      val aux = "\n\u0009(no se encontraron entidades)\n "
      val matching =  s"\n\u0009${title} ${aux} \n"
      matching
    } else {
      val aux = entities.map(e => ("\n\u0009")++e.describe).mkString("\n")
      val matching = s"\n\u0009${title} \nEntidades detectadas: \n ${aux} \n"
      matching
    }
  }

  /**
   * Formatea un resumen de estadísticas de entidades por tipo.
   *
   * @param counts mapa de entityType → cantidad
   * @return texto con las estadísticas ordenadas por cantidad (de mayor a menor)
   *
   * TODO (Ejercicio 5): Implementar este método.
   *
   *   Ejemplo de salida esperada:
   *
   *     === Estadísticas de entidades ===
   *     Person: 5
   *     ProgrammingLanguage: 3
   *     Organization: 2
   *     University: 2
   */
  def formatEntityStats(counts: Map[String, Int]): String = {
    val stats =
      counts.toList
        .filter { case (_, count) => count > 0 }
        .sortBy { case (_, count) => -count }
        .map { case (entityType, count) => s"$entityType: $count" }

    if (stats.isEmpty) {
      "\n=== Estadísticas de entidades ===\n(sin entidades detectadas)\n"
    } else {
      s"\n=== Estadísticas de entidades ===\n${stats.mkString("\n")}\n"
    }
  }

  def formatHierarchicalEntityStats(
      directCounts: Map[String, Int],
      hierarchicalCounts: Map[String, Int]
  ): String = {
    val sections = List(
      formatHierarchy("Technology", List("ProgrammingLanguage"), directCounts, hierarchicalCounts),
      formatHierarchy("Organization", List("University"), directCounts, hierarchicalCounts)
    ).filter(_.nonEmpty)

    if (sections.isEmpty) {
      "\n=== Estadísticas jerárquicas ===\n(sin entidades detectadas)\n"
    } else {
      s"\n=== Estadísticas jerárquicas ===\n${sections.mkString("\n\n")}\n"
    }
  }

  private def formatHierarchy(
      parentType: String,
      childTypes: List[String],
      directCounts: Map[String, Int],
      hierarchicalCounts: Map[String, Int]
  ): String = {
    val total = hierarchicalCounts.getOrElse(parentType, 0)

    if (total == 0) {
      ""
    } else {
      val childLines =
        childTypes
          .map(childType => childType -> directCounts.getOrElse(childType, 0))
          .filter { case (_, count) => count > 0 }
          .map { case (childType, count) => s"  $childType: $count" }

      val directCount = directCounts.getOrElse(parentType, 0)
      val directLine =
        if (directCount > 0) List(s"  ($parentType directa): $directCount")
        else List.empty

      (s"$parentType: $total" :: childLines ::: directLine).mkString("\n")
    }
  }
}
