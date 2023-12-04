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
    //matrices 4x4
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
    //resultado de multiplicar la matriz 1 con la 2
    val matrizresultado1_2: Matriz = Vector(
        Vector(250, 260, 270, 280),
        Vector(618, 644, 670, 696),
        Vector(986, 1028, 1070, 1112),
        Vector(1354, 1412, 1470, 1528)
     )

    //matrices 4x4
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
        // usted si jajajaja
    )
    //resultado de multiplicar la matriz 3 con la 4
    val matrizresultado3_4: Matriz = Vector(
        Vector(162, 156, 158, 164),
        Vector(96, 90, 92, 98),
        Vector(30, 24, 26, 32),
        Vector(36, 42, 40, 34)
    )

    //matriz 8 X 8
    val matriz5: Matriz = Vector(
        Vector(9, 8, 7, 6, 5, 4, 3, 2),
        Vector(6, 5, 4, 3, 2, 1, 0, 9),
        Vector(3, 2, 1, 0, 9, 8, 7, 6),
        Vector(0, 1, 2, 3, 4, 5, 6, 7),
        Vector(7, 6, 5, 4, 3, 2, 1, 0),
        Vector(4, 5, 6, 7, 8, 9, 0, 1),
        Vector(1, 0, 9, 8, 7, 6, 5, 4),
        Vector(8, 7, 6, 5, 4, 3, 2, 1)
    )
    val matriz6: Matriz = Vector(
        Vector(5, 2, 3, 4, 5, 6, 7, 8),
        Vector(4, 5, 6, 7, 8, 9, 5, 6),
        Vector(7, 8, 5, 6, 7, 8, 9, 5),
        Vector(6, 7, 8, 5, 6, 7, 8, 9),
        Vector(5, 6, 7, 8, 5, 6, 7, 8),
        Vector(4, 5, 6, 7, 8, 5, 6, 7),
        Vector(3, 4, 5, 6, 7, 8, 5, 6),
        Vector(2, 3, 4, 5, 6, 7, 8, 5)
    )
    //resultado de multiplicar la matriz 5 con la 6
    val matrizresultado5_6: Matriz = Vector(
        Vector(216, 224, 240, 260, 284, 312, 304, 305),
        Vector(128, 134, 148, 166, 188, 214, 219, 193),
        Vector(140, 164, 196, 232, 232, 236, 234, 241),
        Vector(108, 136, 156, 172, 184, 192, 191, 181),
        Vector(144, 144, 152, 164, 180, 200, 194, 197),
        Vector(202, 226, 242, 254, 262, 266, 281, 287),
        Vector(198, 234, 238, 246, 258, 274, 294, 273),
        Vector(180, 184, 196, 212, 232, 256, 249, 251)
    )


    //matrices 16x16
    val matriz7: Matriz = Vector(
        Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
        Vector(16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1),
        Vector(2, 4, 6, 8, 10, 12, 14, 16, 2, 4, 6, 8, 10, 12, 14, 16),
        Vector(16, 14, 12, 10, 8, 6, 4, 2, 16, 14, 12, 10, 8, 6, 4, 2),
        Vector(3, 6, 9, 12, 15, 3, 6, 9, 12, 15, 3, 6, 9, 12, 15, 3),
        Vector(1, 3, 5, 7, 9, 11, 13, 15, 1, 3, 5, 7, 9, 11, 13, 15),
        Vector(7, 8, 4, 2, 10, 6, 13, 14, 11, 9, 3, 1, 12, 15, 5, 16),
        Vector(14, 5, 3, 8, 6, 16, 12, 1, 9, 10, 11, 7, 2, 4, 15, 13),
        Vector(11, 2, 15, 6, 8, 9, 4, 14, 3, 10, 13, 16, 1, 5, 12, 7),
        Vector(16, 13, 1, 4, 5, 8, 12, 7, 11, 14, 2, 10, 9, 15, 3, 6),
        Vector(12, 11, 9, 14, 16, 4, 7, 5, 6, 8, 2, 3, 10, 13, 15, 1),
        Vector(3, 7, 8, 1, 13, 2, 16, 9, 14, 5, 12, 11, 4, 15, 6, 10),
        Vector(5, 1, 10, 3, 7, 15, 8, 13, 16, 6, 4, 14, 12, 2, 9, 11),
        Vector(15, 12, 13, 11, 2, 1, 6, 16, 5, 7, 9, 8, 14, 3, 10, 4),
        Vector(4, 10, 2, 5, 11, 7, 3, 12, 15, 13, 1, 6, 8, 14, 16, 9),
        Vector(9, 14, 11, 10, 3, 13, 1, 4, 7, 15, 16, 2, 6, 8, 5, 12)
    )
    val matriz8: Matriz = Vector(
        Vector(4, 8, 12, 16, 2, 6, 10, 14, 1, 5, 9, 13, 3, 7, 11, 15),
        Vector(15, 11, 7, 3, 16, 12, 8, 4, 13, 9, 5, 1, 14, 10, 6, 2),
        Vector(6, 2, 14, 10, 7, 3, 15, 11, 4, 8, 12, 16, 5, 1, 13, 9),
        Vector(9, 13, 5, 1, 10, 14, 2, 6, 11, 15, 7, 3, 12, 16, 4, 8),
        Vector(8, 16, 4, 12, 9, 1, 13, 5, 6, 14, 2, 10, 7, 15, 3, 11),
        Vector(11, 3, 15, 7, 8, 16, 4, 12, 9, 1, 13, 5, 10, 2, 14, 6),
        Vector(14, 10, 6, 2, 15, 11, 7, 3, 16, 12, 8, 4, 13, 9, 5, 1),
        Vector(1, 5, 9, 13, 4, 8, 12, 16, 2, 6, 10, 14, 3, 7, 11, 15),
        Vector(16, 12, 8, 4, 15, 11, 7, 3, 14, 10, 6, 2, 13, 9, 5, 1),
        Vector(7, 15, 3, 11, 6, 14, 2, 10, 5, 1, 13, 9, 4, 8, 12, 16),
        Vector(10, 6, 2, 14, 11, 7, 3, 15, 12, 8, 4, 16, 1, 5, 9, 13),
        Vector(12, 4, 16, 8, 13, 5, 9, 1, 7, 15, 3, 11, 2, 6, 10, 14),
        Vector(13, 9, 1, 5, 12, 16, 4, 8, 15, 11, 7, 3, 6, 2, 14, 10),
        Vector(2, 14, 10, 6, 3, 15, 11, 7, 8, 4, 16, 12, 9, 1, 13, 5),
        Vector(5, 1, 13, 9, 14, 10, 6, 2, 3, 7, 11, 15, 8, 12, 16, 4),
        Vector(3, 7, 11, 15, 5, 13, 9, 1, 10, 14, 2, 6, 4, 8, 12, 16)
    )
    //resultado de multiplicar la matriz 7 con la 8
    val matrizresultado7_8: Matriz = Vector(
        Vector(1088, 1108, 1152, 1196, 1285, 1489, 977, 877, 1199, 1211, 1083, 1239, 884, 936, 1480, 1292),
        Vector(1224, 1204, 1160, 1116, 1265, 1265, 1097, 1129, 1113, 1169, 1093, 1141, 1054, 1070, 1206, 1190),
        Vector(1088, 1128, 1280, 1240, 1306, 1522, 1138, 1002, 1214, 1302, 1174, 1294, 1016, 1056, 1504, 1320),
        Vector(1360, 1320, 1168, 1208, 1394, 1394, 1058, 1122, 1234, 1218, 1130, 1226, 1036, 1068, 1340, 1308),
        Vector(1134, 1314, 1086, 1098, 1320, 1392, 1056, 936, 1107, 1203, 1179, 1227, 1047, 1143, 1335, 1191),
        Vector(952, 992, 1144, 1104, 1156, 1360, 1016, 884, 1078, 1162, 1046, 1154, 902, 938, 1346, 1174),
        Vector(1072, 1280, 1080, 1176, 1183, 1487, 1111, 967, 1216, 1164, 1116, 1096, 1013, 961, 1353, 1221),
        Vector(1170, 1078, 1246, 1218, 1293, 1429, 917, 957, 1147, 1127, 1087, 1179, 986, 1066, 1378, 1218),
        Vector(995, 951, 1323, 1367, 1165, 1173, 1117, 1141, 905, 1133, 1117, 1505, 747, 955, 1443, 1459),
        Vector(1198, 1314, 1150, 1098, 1230, 1490, 1026, 982, 1183, 1083, 1163, 1063, 1033, 953, 1329, 1201),
        Vector(1110, 1290, 1098, 1070, 1277, 1349, 1077, 957, 1077, 1193, 1161, 1197, 1080, 1140, 1300, 1120),
        Vector(1186, 1230, 1130, 1150, 1335, 1275, 1155, 887, 1247, 1279, 1019, 1235, 988, 988, 1244, 1148),
        Vector(1212, 996, 1288, 1176, 1314, 1366, 1062, 970, 1155, 1211, 1063, 1159, 927, 919, 1415, 1263),
        Vector(1109, 1053, 1129, 1193, 1260, 1316, 1060, 1148, 1066, 1190, 1098, 1278, 885, 989, 1389, 1333),
        Vector(1094, 1250, 1158, 1130, 1279, 1471, 1047, 887, 1097, 1113, 1157, 1149, 1042, 1074, 1386, 1162),
        Vector(1143, 1163, 1107, 1223, 1204, 1480, 928, 1140, 1172, 1044, 1132, 1180, 941, 933, 1397, 1309)
    )


    // matrices 8x8
    val matriz9: Matriz = Vector(
        Vector(1, 0, 1, 0, 1, 0, 1, 0),
        Vector(0, 1, 0, 1, 0, 1, 0, 1),
        Vector(1, 0, 1, 0, 1, 0, 1, 0),
        Vector(0, 1, 0, 1, 0, 1, 0, 1),
        Vector(1, 0, 1, 0, 1, 0, 1, 0),
        Vector(0, 1, 0, 1, 0, 1, 0, 1),
        Vector(1, 0, 1, 0, 1, 0, 1, 0),
        Vector(0, 1, 0, 1, 0, 1, 0, 1)
    )
    val matriz10: Matriz = Vector(
        Vector(0, 1, 0, 1, 0, 1, 0, 1),
        Vector(1, 0, 1, 0, 1, 0, 1, 0),
        Vector(0, 1, 0, 1, 0, 1, 0, 1),
        Vector(1, 0, 1, 0, 1, 0, 1, 0),
        Vector(0, 1, 0, 1, 0, 1, 0, 1),
        Vector(1, 0, 1, 0, 1, 0, 1, 0),
        Vector(0, 1, 0, 1, 0, 1, 0, 1),
        Vector(1, 0, 1, 0, 1, 0, 1, 0)
    )
    // Resultado de multiplicar las matrices 9 y 10
    val matrizresultado9_10: Matriz = Vector(
        Vector(0, 4, 0, 4, 0, 4, 0, 4),
        Vector(4, 0, 4, 0, 4, 0, 4, 0),
        Vector(0, 4, 0, 4, 0, 4, 0, 4),
        Vector(4, 0, 4, 0, 4, 0, 4, 0),
        Vector(0, 4, 0, 4, 0, 4, 0, 4),
        Vector(4, 0, 4, 0, 4, 0, 4, 0),
        Vector(0, 4, 0, 4, 0, 4, 0, 4),
        Vector(4, 0, 4, 0, 4, 0, 4, 0)
    )


    //test de la funcion basica secuencial
    test("testmultmatriz1") {
        assert(matrizresultado1_2 == Taller4.mulMatriz(matriz1, matriz2))
        //lloremos juntos :(
        // cual forma de llorar es la mejor? :(
    }
    test("testmultmatriz2") {
        assert(matrizresultado3_4 == Taller4.mulMatriz(matriz3, matriz4))
    }
    test( "testmulmatriz3") {
        assert(matrizresultado5_6 == Taller4.mulMatriz(matriz5, matriz6))
    }
    test("testmulmatriz4") {
        assert(matrizresultado7_8 == Taller4.mulMatriz(matriz7, matriz8))
    }
    test("testmulmatriz5") {
        assert(matrizresultado9_10 == Taller4.mulMatriz(matriz9, matriz10))
    }
    //test de la funcion basica paralela
    test("testmulmatrizPar1"){
        assert(matrizresultado1_2 == Taller4.multMatrizPar(matriz1, matriz2))
    }
    test("testmulmatrizPar2"){
        assert(matrizresultado3_4 == Taller4.multMatrizPar(matriz3, matriz4))
    }
    test("testmulmatrizPar3"){
        assert(matrizresultado5_6 == Taller4.multMatrizPar(matriz5, matriz6))
    }
    test("testmulmatrizPar4"){
        assert(matrizresultado7_8 == Taller4.multMatrizPar(matriz7, matriz8))
    }
    test("testmulmatrizPar5"){
        assert(matrizresultado9_10 == Taller4.multMatrizPar(matriz9, matriz10))
    }
    //test de la funcion recursiva secuencial
    test("testmulmatrizRec1"){
        assert(matrizresultado1_2 == Taller4.multMatrizRec(matriz1, matriz2))
    }
    test("testmulmatrizRec2"){
        assert(matrizresultado3_4 == Taller4.multMatrizRec(matriz3, matriz4))
    }
    test("testmulmatrizRec3"){
        assert(matrizresultado5_6 == Taller4.multMatrizRec(matriz5, matriz6))
    }
    test("testmulmatrizRec4"){
        assert(matrizresultado7_8 == Taller4.multMatrizRec(matriz7, matriz8))
    }
    test("testmulmatrizRec5"){
        assert(matrizresultado9_10 == Taller4.multMatrizRec(matriz9, matriz10))
    }


    //test de la funcion recursiva paralela
    test("testmulmatrizRecPar1"){
        assert(matrizresultado1_2 == Taller4.multMatrizRecPar(matriz1, matriz2))
    }
    test("testmulmatrizRecPar2"){
        assert(matrizresultado3_4 == Taller4.multMatrizRecPar(matriz3, matriz4))
    }
    test("testmulmatrizRecPar3"){
        assert(matrizresultado5_6 == Taller4.multMatrizRecPar(matriz5, matriz6))
    }
    test("testmulmatrizRecPar4"){
        assert(matrizresultado7_8 == Taller4.multMatrizRecPar(matriz7, matriz8))
    }
    test("testmulmatrizRecPar5"){
        assert(matrizresultado9_10 == Taller4.multMatrizRecPar(matriz9, matriz10))
    }


    //test de la funcion strassen secuencial
    test("testmulmatrizStrassen1"){
        assert(matrizresultado1_2 == Taller4.multStrassen(matriz1, matriz2))
    }
    test("testmulmatrizStrassen2"){
        assert(matrizresultado3_4 == Taller4.multStrassen(matriz3, matriz4))
    }
    test("testmulmatrizStrassen3"){
        assert(matrizresultado5_6 == Taller4.multStrassen(matriz5, matriz6))
    }
    test("testmulmatrizStrassen4"){
        assert(matrizresultado7_8 == Taller4.multStrassen(matriz7, matriz8))
    }
    test("testmulmatrizStrassen5"){
        assert(matrizresultado9_10 == Taller4.multStrassen(matriz9, matriz10))
    }


    //test de la multiplicacion de matrices del metodo strassen paralelo
    test("testmulmatrizStrassenPar1"){
        assert(matrizresultado1_2 == Taller4.multStrassenPar(matriz1, matriz2))
    }
    test("testmulmatrizStrassenPar2"){
        assert(matrizresultado3_4 == Taller4.multStrassenPar(matriz3, matriz4))
    }
    test("testmulmatrizStrassenPar3"){
        assert(matrizresultado5_6 == Taller4.multStrassenPar(matriz5, matriz6))
    }
    test("testmulmatrizStrassenPar4"){
        assert(matrizresultado7_8 == Taller4.multStrassenPar(matriz7, matriz8))
    }
    test("testmulmatrizStrassenPar5"){
        assert(matrizresultado9_10 == Taller4.multStrassenPar(matriz9, matriz10))
    }
}
