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
// Se repite el mismo codigo 5 veces para cada dimensión
for {
  i <- 0 to 6 by 2
} yield {
  compararAlgoritmos(
    algoritmosMult(i),   // algoritmo secuencial
    algoritmosMult(i+1)  // algoritmo paralelo
  )(m1, m2)
}


//Comparación de los algoritmo productPunto y productPuntoPar, para analizar el paralelismo de datos
// Compara cada vez con potencias de 10, desde 1 hasta 7
for {
  i <- 1 to 7
} yield (compararProdPunto(math.pow(10,i).toInt),math.pow(10,i))

//Casos de prueba para verificar que los algoritmos multiplican matrices correctamente.
// cambiando math.pow(2, k).toInt se elige la dimensión 2^k.
val n = 2
val m3 = matrizAlAzar(math.pow(2,n).toInt, 2)
val m4 = matrizAlAzar(math.pow(2,n).toInt, 2)

multMatriz(m3,m4)
multMatrizPar(m3,m4)
multMatrizRec(m3,m4)
multMatrizRecPar(m3,m4)
multStrassen(m3,m4)
multStrassenPar(m3,m4)

//luego entonces se puede verificar la correcta multiplicación con aplicaciones como matrizCal.com



