// =====================================================================
// Ejercicio 6: Integración del sistema completo
// =====================================================================

object Main {
  def main(args: Array[String]): Unit = {

    // ------------------------------------------------------------------
    // Paso 1: Cargar diccionarios
    // ------------------------------------------------------------------
    // TODO (Ejercicio 2)
    val dictionary: List[NamedEntity] = Dictionary.loadAll()

    println(s"Diccionario cargado: ${dictionary.size} entidades.\n")

    // ------------------------------------------------------------------
    // Paso 2: Descargar posts
    // ------------------------------------------------------------------
    val subscriptions = FileIO.readSubscriptions()

    val allPosts: List[(String, List[String])] = subscriptions.map { url =>
      println(s"Descargando posts de: $url")
      val json   = FileIO.downloadFeed(url)
      val titles = FileIO.extractPostTitles(json)
      (url, titles)
    }

    // ------------------------------------------------------------------
    // Paso 3: Detectar entidades y mostrar resultados por post
    // ------------------------------------------------------------------
    // TODO (Ejercicios 3, 4 y 6):
    //   Para cada post:
    //     1. Detectar entidades
    //     2. Formatear y mostrar el resultado
    allPosts.foreach { case (url, titles) => println(s"\nPosts de: $url")
      titles.foreach { title =>
        val entities = Analyzer.detectEntities(title, dictionary)
        print(s"""${Formatters.formatNERResult(title, entities)}""")
      }
    }

    // ------------------------------------------------------------------
    // Paso 4: Estadísticas globales
    // ------------------------------------------------------------------
    // TODO (Ejercicios 5 y 6):
    //   1. Recolectar TODAS las entidades detectadas en todos los posts
    //   2. Contar por tipo
    //   3. Mostrar el resumen

    val allEntities =
      allPosts.flatMap { case (_, titles) =>
        titles.flatMap { title =>
          Analyzer.detectEntities(title, dictionary)
        }
      }

    val entityCount = Analyzer.countByType(allEntities)
    val entityStats = Formatters.formatEntityStats(entityCount)
    println(entityStats)

    val hierarchicalCount = Analyzer.countByHierarchy(allEntities)
    val hierarchicalStats = Formatters.formatHierarchicalEntityStats(entityCount, hierarchicalCount)
    println(hierarchicalStats)
  }
}
