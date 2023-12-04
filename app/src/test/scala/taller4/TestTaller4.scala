/**
 * Plantilla para pruebas
* @author Carlos Delgado
* @version 1.0
* @note 22 de Noviembre de 2023 
 */
package taller4

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestTaller4 extends AnyFunSuite{
    type Matriz = Vector[Vector[Int]]
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
    val matrizresultado1_2: Matriz = Vector(
        Vector(250, 260, 270, 280),
        Vector(618, 644, 670, 696),
        Vector(986, 1028, 1070, 1112),
        Vector(1354, 1412, 1470, 1528)
    )
    test("testmultmatriz1"){
        //val taller4 = new Taller4()
        assert(matrizresultado1_2 == Taller4.mulMatriz(matriz1, matriz2))
        //lloremos juntos :(
        // cual forma de llorar es la mejor? :(
    }

    val matriz3: Matriz = Vector(
        Vector(9, 8, 7, 6),
        Vector(6, 5, 4, 3),
        Vector(3, 2, 1, 0),
        Vector(0, 1, 2, 3)
    )
    val matriz4: Matriz = Vector(
        Vector(5, 2, 3, 4),
        Vector(4, 5, 6, 7),
        Vector(7, 8, 5, 6),
        Vector(6, 7, 8, 5)
        // vaya duermanse
        // usted si jajajaj
    )
    val matrizresultado3_4: Matriz = Vector(
        Vector(203, 241, 159, 193),
        Vector(121, 145, 97, 115),
        Vector(41, 65, 33, 51),
        Vector(54, 78, 70, 85)
    )
    test("testmultmatriz2") {
        //val taller4 = new Taller4()
        assert(matrizresultado3_4 == Taller4.mulMatriz(matriz3, matriz4))

    }



}
