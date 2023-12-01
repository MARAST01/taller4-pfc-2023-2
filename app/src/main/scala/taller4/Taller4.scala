/**
  * Taller 3 - Programación Funcional
  * Autores: <Estudiantes>
  * Profesor: Carlos A Delgado
  */
package taller4

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer

object Taller4{

  def saludo() = "Taller 4 2023-II"
  type Matriz = Vector[Vector[Int]]

  def prodPunto(v1:Vector[Int],v2:Vector[Int]):Int = {
    (v1 zip v2).map({case (i,j)=>i*j}).sum
  }
  def transpuesta (m:Matriz):Matriz = {
    val l=m.length
    Vector.tabulate(l,l)((i,j)=>m(j)(i))
  }
  def mulMatriz(m1:Matriz,m2:Matriz):Matriz = {
    val m2t=transpuesta(m2)
    Vector.tabulate(m1.length,m2t.length)((i,j)=>prodPunto(m1(i),m2t(j)))
  }
  // usando las abstracciones de parallel o task
  def multMatrizPar (m1:Matriz,m2:Matriz):Matriz = {

  }

  def main(args: Array[String]): Unit = {
    println(saludo())
    println(
      withWarmer(new Warmer.Default) measure {
        (1 to 100000000).toArray
      }
    )
  }
 }
