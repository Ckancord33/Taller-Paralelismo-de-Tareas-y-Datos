import Benchmark.*
import Matrices.*
import org.scalameter.*

import scala.collection.compat.immutable.ArraySeq

// Funciones de multiplicación de matrices en un arreglo
val algoritmosMult: ArraySeq[(Matriz,Matriz)=>Matriz] =
  ArraySeq(
    multMatriz,      multMatrizPar,
    multMatrizRec,   multMatrizRecPar,
    multStrassen,    multStrassenPar
  )

// Definicion de las matrices de prueba
// cambiando math.pow(2, k).toInt se elige la dimensión 2^k.
val k = 2
val m1 = matrizAlAzar(math.pow(2,k).toInt, 2)
val m2 = matrizAlAzar(math.pow(2,k).toInt, 2)

// Comparacion de cada algoritmo secuencial contra su versión paralela
// Se repite 5 veces para cada diemnsion
for (i <- 0 until algoritmosMult.length by 2) yield {
  compararAlgoritmos(
    algoritmosMult(i),   // algoritmo secuencial
    algoritmosMult(i+1)  // algoritmo paralelo
  )(m1, m2)
}




