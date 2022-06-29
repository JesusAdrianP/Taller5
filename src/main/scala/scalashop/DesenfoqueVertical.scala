package scalashop

import common.task
import org.scalameter.{Key, Warmer, config}
import scalashop.{Img, desenfoqueNuclear}

object EjecutorDesenfoqueVertical {

  val standardConfig = config(
    Key.exec.minWarmupRuns := 5,
    Key.exec.maxWarmupRuns := 10,
    Key.exec.benchRuns := 10,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit = {
    val radio = 5
    val ancho = 1920
    val alto = 1080
    val fte = new Img(ancho, alto)
    val dst = new Img(ancho, alto)
    val seqtime = standardConfig measure {
      DesenfoqueVertical.desenfoque(fte, dst, 0, ancho, radio)
    }
    println(s"tiempo de desenfoque secuencial: $seqtime ms")

    val numTareas = 32
    val partime = standardConfig measure {
      DesenfoqueVertical.desenfoquePar(fte, dst, numTareas, radio)
    }
    println(s"tiempo de desenfoque paralelo: $partime ms")
    println(s"aceleracion: ${seqtime.value / partime.value}")
  }

}

/** Una computación sencilla trivialmente paralelizable */
object DesenfoqueVertical {

  /** Desenfoca las columnas de la imagen fuente 'fte' en la imagen destino 'dst'
   * empezando desde la columna 'inicial' y terminando con la columna 'fin' (exclusive)
   *
   * Dentro de cada columna, el 'desenfoque' atraviesa los pixels de arriba a abajo
   */
  def desenfoque(fte: Img, dst: Img, inicial: Int, fin: Int, radio: Int): Unit = {
    for(
      x <- inicial until fin;
      y <- 0 until fte.alto
      if x >= 0 && x < fte.ancho
    ) yield {
      dst.update(x, y,  desenfoqueNuclear(fte, x, y, radio))
    }
  }

  /**  Desenfoca las columnas de la imagen fuente 'fte' en la imagen destino 'dst'
   *  en paralelo usando 'numTareas' tareas.
   *
   *  La paralelizacion se hace dividiendo la imagen fuente 'fte' en  conjuntos de columnas,
   *  cada conjunto correspondiente a una de las 'numTareas' en que se hace el calculo en paralelo
   *
   */
  def desenfoquePar(fte: Img, dst: Img, numTareas: Int, radio: Int): Unit = {
    val tareasPorColumna = Math.max(fte.ancho / numTareas,1)
    val puntosDeDivision = Range(0, fte.ancho) by tareasPorColumna

    val tasks = puntosDeDivision.map(t => {
      task {
        desenfoque(fte, dst, t, t + tareasPorColumna, radio)
      }
    })
    tasks.map(t => t.join())
  }

}