/**
  * Taller 3 - Programación Funcional
  * Autores: <Estudiantes>
  * Profesor: Carlos A Delgado
  */
package taller4

import org.scalameter._
import org.scalameter.withWarmer
import org.scalameter.Warmer
import common._
import scala.util.Random




object Taller4{

  type Matriz = Vector[Vector[Int]]
  def matrizAlAzar(long: Int, vals: Int): Matriz =  {

    val random = new Random()
    val v = Vector.fill(long, long)(random.nextInt(vals))
    v
  }

  def compararAlgoritmos(funcion1: (Matriz, Matriz) => Matriz, funcion2: (Matriz, Matriz) => Matriz)
                        (matriz1: Matriz, matriz2: Matriz): (Double, Double, Double) = {
    val tiempoFuncion1 = withWarmer(new Warmer.Default) measure {
      funcion1(matriz1, matriz2)
    }
    val tiempoFuncion2 = withWarmer(new Warmer.Default) measure {
      funcion2(matriz1, matriz2)
    }
    val tiempo1: Double = tiempoFuncion1.value
    val tiempo2: Double = tiempoFuncion2.value
    val aceleracion = tiempo1 / tiempo2
    (tiempo1, tiempo2,aceleracion)
  }
  def prodPunto(v1:Vector[Int],v2:Vector[Int]):Int = {
    (v1 zip v2).map({case (i,j)=>i*j}).sum
  }
  def transpuesta (m:Matriz):Matriz = {
    val l=m.length
    Vector.tabulate(l,l)((i,j)=>m(j)(i))
  }
  def mulMatriz(m1:Matriz,m2:Matriz):Matriz = {
    val m2t = transpuesta(m2)
    val l = m1.length
    Vector.tabulate(l,l)((i,j)=>prodPunto(m1(i),m2t(j)))
  }
  // usando task multiplicar matrices de froma parallela
  def multMatrizPar(m1:Matriz,m2:Matriz):Matriz = {
    val m2t=transpuesta(m2)
    val (a,b)= m1.splitAt(m1.length/2)
    val t1 = task (a.map(f => m2t.map (c => prodPunto(f, c))))
    val t2 = task (b.map(f => m2t.map (c => prodPunto(f, c))))
    t1.join() ++ t2.join()
  }

  def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
    Vector.tabulate(l, l)((f, c) => m(i + f)(j + c))
  }
  def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    Vector.tabulate(n, n) { (i, j) =>
      m1(i)(j) + m2(i)(j)
    }
  }
  def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {
  val n = m1.length

  if (n == 1) {
    // Caso base: matrices de 1x1
    Vector(Vector(m1(0)(0) * m2(0)(0)))
  } else {
    // Dividir y conquistar
    val m = n / 2

    // Submatrices de A
    val a11 = subMatriz(m1, 0, 0, m)
    val a12 = subMatriz(m1, 0, m, m)
    val a21 = subMatriz(m1, m, 0, m)
    val a22 = subMatriz(m1, m, m, m)

    // Submatrices de B
    val b11 = subMatriz(m2, 0, 0, m)
    val b12 = subMatriz(m2, 0, m, m)
    val b21 = subMatriz(m2, m, 0, m)
    val b22 = subMatriz(m2, m, m, m)

    // Calcula las submatrices intermedias
    val c11 = sumMatriz(multMatrizRec(a11, b11), multMatrizRec(a12, b21))
    val c12 = sumMatriz(multMatrizRec(a11, b12), multMatrizRec(a12, b22))
    val c21 = sumMatriz(multMatrizRec(a21, b11), multMatrizRec(a22, b21))
    val c22 = sumMatriz(multMatrizRec(a21, b12), multMatrizRec(a22, b22))

    // Combina las submatrices intermedias para obtener la matriz resultante
    Vector.tabulate(n, n) { (i, j) =>
      if (i < m && j < m) c11(i)(j)
      else if (i < m) c12(i)(j - m)
      else if (j < m) c21(i - m)(j)
      else c22(i - m)(j - m)
      }
    }
  }
  //paralizar recursion
  def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length

    if (n == 1) {
      // Caso base: matrices de 1x1
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      // Dividir y conquistar
      val m = n / 2

      // Submatrices de A
      val a11 = subMatriz(m1, 0, 0, m)
      val a12 = subMatriz(m1, 0, m, m)
      val a21 = subMatriz(m1, m, 0, m)
      val a22 = subMatriz(m1, m, m, m)

      // Submatrices de B
      val b11 = subMatriz(m2, 0, 0, m)
      val b12 = subMatriz(m2, 0, m, m)
      val b21 = subMatriz(m2, m, 0, m)
      val b22 = subMatriz(m2, m, m, m)

      // Calcula las submatrices intermedias
      val (c11,c12,c21,c22)= parallel(sumMatriz(multMatrizRecPar(a11, b11), multMatrizRecPar(a12, b21)),
        sumMatriz(multMatrizRecPar(a11, b12), multMatrizRecPar(a12, b22)),
        sumMatriz(multMatrizRecPar(a21, b11), multMatrizRecPar(a22, b21)),
        sumMatriz(multMatrizRecPar(a21, b12), multMatrizRecPar(a22, b22)))

      // Combina las submatrices intermedias para obtener la matriz resultante
      Vector.tabulate(n, n) { (i, j) =>
        if (i < m && j < m) c11(i)(j)
        else if (i < m) c12(i)(j - m)
        else if (j < m) c21(i - m)(j)
        else c22(i - m)(j - m)
      }
    }
  }

  def sumarMatrices(m1: Matriz, m2: Matriz): Matriz = {
    Vector.tabulate(m1.size, m1.head.size) { (i, j) =>
      m1(i)(j) + m2(i)(j)
    }
  }

  def restaMatriz(m1:Matriz, m2:Matriz):Matriz = {
    val n = m1.length
    Vector.tabulate(n,n){(i,j)=>
      m1(i)(j)-m2(i)(j)
    }
  }

  def multStrassen(m1: Matriz, m2: Matriz): Matriz = {

    val n = m1.head.count(_ => true)

    if (n == 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      val m = n / 2

      val a11 = subMatriz(m1, 0, 0, m)
      val a12 = subMatriz(m1, 0, m, m)
      val a21 = subMatriz(m1, m, 0, m)
      val a22 = subMatriz(m1, m, m, m)

      val b11 = subMatriz(m2, 0, 0, m)
      val b12 = subMatriz(m2, 0, m, m)
      val b21 = subMatriz(m2, m, 0, m)
      val b22 = subMatriz(m2, m, m, m)

      val p1 = multStrassen(sumMatriz(a11, a22), sumMatriz(b11, b22))
      val p2 = multStrassen(sumMatriz(a21, a22), b11)
      val p3 = multStrassen(a11, restaMatriz(b12, b22))
      val p4 = multStrassen(a22, restaMatriz(b21, b11))
      val p5 = multStrassen(sumMatriz(a11, a12), b22)
      val p6 = multStrassen(restaMatriz(a21, a11), sumMatriz(b11, b12))
      val p7 = multStrassen(restaMatriz(a12, a22), sumMatriz(b21, b22))

      val c11 = restaMatriz(sumMatriz(sumMatriz(p1, p4), p7), p5)
      val c12 = sumMatriz(p3, p5)
      val c21 = sumMatriz(p2, p4)
      val c22 = restaMatriz(sumMatriz(sumMatriz(p1, p3), p6), p2)

      // Construir la matriz resultante
      Vector.tabulate(n, n) { (i, j) =>
        if (i < m && j < m) c11(i)(j)
        else if (i < m && j >= m) c12(i)(j - m)
        else if (i >= m && j < m) c21(i - m)(j)
        else c22(i - m)(j - m)
      }
    }
  }

  def multStrassenPar(m1: Matriz, m2: Matriz): Matriz = {

    val n = m1.head.count(_ => true)

    if (n == 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      val m = n / 2

      val a11 = task {subMatriz(m1, 0, 0, m)}
      val a12 = task {subMatriz(m1, 0, m, m)}
      val a21 = task {subMatriz(m1, m, 0, m)}
      val a22 = task {subMatriz(m1, m, m, m)}

      val b11 = task {subMatriz(m2, 0, 0, m)}
      val b12 = task {subMatriz(m2, 0, m, m)}
      val b21 = task {subMatriz(m2, m, 0, m)}
      val b22 = task {subMatriz(m2, m, m, m)}


      val p1 = task {multStrassenPar(sumMatriz(a11.join, a22.join), sumMatriz(b11.join, b22.join))}
      val p2 = task {multStrassenPar(sumMatriz(a21.join, a22.join), b11.join)}
      val p3 = task {multStrassenPar(a11.join, restaMatriz(b12.join, b22.join))}
      val p4 = task {multStrassenPar(a22.join, restaMatriz(b21.join, b11.join))}
      val p5 = task {multStrassenPar(sumMatriz(a11.join, a12.join), b22.join)}
      val p6 = task {multStrassenPar(restaMatriz(a21.join, a11.join), sumMatriz(b11.join, b12.join))}
      val p7 = task {multStrassenPar(restaMatriz(a12.join, a22.join), sumMatriz(b21.join, b22.join))}

      val c11 = restaMatriz(sumMatriz(sumMatriz(p1.join, p4.join), p7.join), p5.join)
      val c12 = sumMatriz(p3.join, p5.join)
      val c21 = sumMatriz(p2.join, p4.join)
      val c22 = restaMatriz(sumMatriz(sumMatriz(p1.join, p3.join), p6.join), p2.join)



      // Construir la matriz resultante
      Vector.tabulate(n, n) { (i, j) =>
        if (i < m && j < m) c11(i)(j)
        else if (i < m && j >= m) c12(i)(j - m)
        else if (i >= m && j < m) c21(i - m)(j)
        else c22(i - m)(j - m)
      }
    }}
  def main(args: Array[String]): Unit = {

    val matriz1: Matriz = Vector(
      Vector(1, 2, 3, 4),
      Vector(5, 6, 7, 8),
      Vector(9, 10, 11, 12),
      Vector(13, 14, 15, 16)
    )

    val matriz2: Matriz = Vector(
      Vector(17, 18, 19, 20),
      Vector(21, 22, 23, 24),
      Vector(25, 26, 27, 28),
      Vector(29, 30, 31, 32)
    )



    /*
    println("\nResultado (Multiplicación de Matrices - Secuencial):")
    mulMatriz(matriz1, matriz2).foreach(row => println(row.mkString(" ")))
    */

    /*
    // Mostrar el resultado de la multiplicación de matrices

    println("\nResultado (Multiplicación de Matrices - paralela):")
    multMatrizPar(matriz1, matriz2 ).foreach(row => println(row.mkString(" ")))
    */

    /*
    val submatrizA11 = subMatriz(matriz1, 0, 0, matriz1.length / 2)
    val submatrizA22 = subMatriz(matriz1, (matriz1.length / 2)-1, (matriz1.length / 2)-1, 3)

    println("Submatriz A11:")
    submatrizA11.foreach(row => println(row.mkString(" ")))

    println("\nSubmatriz A22:")
    submatrizA22.foreach(row => println(row.mkString(" ")))
    */
    /*
    println("\nResultado (SUMA de Matrices - secuencial):")
    sumMatriz(matriz1, matriz2).foreach(row => println(row.mkString(" ")))
    */

    /*
    println("\nResultado (Multiplicación de Matrices - recursiva secuencial):")
    multMatrizRec(matriz1, matriz2).foreach(row => println(row.mkString(" ")))
    */

    /*
    println("\nResultado (Multiplicación de Matrices - recursiva paralela):")
    multMatrizRecPar(matriz1, matriz2).foreach(row => println(row.mkString(" ")))
    */

    /*
    println("\nResultado (Multiplicación de Matrices - strassen secuencial):")
    multStrassen(matriz1, matriz2).foreach(row => println(row.mkString(" ")))
    */

    /*
    println("\nResultado (Multiplicación de Matrices - strassen paralela):")
    multStrassenPar(matriz1, matriz2).foreach(row => println(row.mkString(" ")))
    */
    //comparar algoritmos
    //println(compararAlgoritmos(multStrassen,multStrassenPar)(matrizAlAzar(32,8),matrizAlAzar(32,8)))
    //println(compararAlgoritmos(multMatrizRec,multMatrizRecPar)(matrizAlAzar(32,8),matrizAlAzar(32,8)))
    //println(compararAlgoritmos(mulMatriz,multMatrizzPar)(matrizAlAzar(32,8),matrizAlAzar(32,8)))
    /*
    for {
      i <- 1 to 10
      m1 = matrizAlAzar(math.pow(2, i).toInt, 2)
      m2 = matrizAlAzar(math.pow(2, i).toInt, 2)
    } yield (compararAlgoritmos(multMatrizRec, multMatrizRecPar)(m1, m2), math.pow(2, i).toInt)
      */
  }
 }
