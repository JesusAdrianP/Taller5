package scalashop

import common.task
import org.scalameter.{Key, Warmer, config}
import scalashop.{Img, desenfoqueNuclear}

object EjecutorDesenfoqueHorizontal {

  val standardConfig = config(
    Key.exec.minWarmupRuns := 5,
    Key.exec.maxWarmupRuns := 10,
    Key.exec.benchRuns := 10,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit = {
    val radio = 3
    val ancho = 1920
    val alto = 1080
    val fte = new Img(ancho, alto)
    val dst = new Img(ancho, alto)
    val seqtime = standardConfig measure {
      DesenfoqueHorizontal.desenfoque(fte, dst, 0, alto, radio)
    }
    println(s"tiempo de desenfoque secuencial: $seqtime ms")

    val numTareas = 32
    val partime = standardConfig measure {
      DesenfoqueHorizontal.desenfoquePar(fte, dst, numTareas, radio)
    }
    println(s"tiempo de desenfoque paralelo: $partime ms")
    println(s"aceleracion: ${seqtime.value / partime.value}")
  }
}


/** Una computación sencilla trivialmente paralelizable */
object DesenfoqueHorizontal {

  /** Desenfoca las filas de la imagen fuente 'fte' en la imagen destino 'dst'
   * empezando desde la fila 'inicial' y terminando con la fila 'fin' (exclusive)
   *
   * Dentro de cada fila, el 'desenfoque' atraviesa los pixels de izquierda a derecha
   *
   */
  def desenfoque(fte: Img, dst: Img, inicial: Int, fin: Int, radio: Int): Unit = {
    for(
      x <- 0 until fte.ancho;
      y <- inicial until fin;
      if y >= 0 && y < fte.alto
    ) yield {
      dst.update(x, y, desenfoqueNuclear(fte, x, y, radio))
    }
  }

  /**  Desenfoca las filas de la imagen fuente 'fte' en la imagen destino 'dst'
   *  en paralelo usando 'numTareas' tareas.
   *
   *  La paralelizacion se hace dividiendo la imagen fuente 'fte' en  conjuntos de filas,
   *  cada conjunto correspondiente a una de las 'numTareas' en que se hace el calculo en paralelo
   *
   */
  def desenfoquePar(fte: Img, dst: Img, numTareas: Int, radio: Int):Unit = {
    val tareasPorFila = Math.max(fte.alto / numTareas, 1)
    val puntosDeDivision = Range(0, fte.alto) by tareasPorFila

    val tasks = puntosDeDivision.map(t => {
      task {
        desenfoque(fte, dst, t, t + tareasPorFila, radio)
      }
    })

    tasks.map(t => t.join())
  }

}