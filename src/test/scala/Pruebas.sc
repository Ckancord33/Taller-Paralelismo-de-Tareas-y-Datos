import Benchmark.*
import Matrices.*
import org.scalameter.*

import scala.collection.compat.immutable.ArraySeq


val algoritmosMult: ArraySeq[(Matriz,Matriz)=>Matriz] = ArraySeq(multMatriz,multMatrizPar,multMatrizRec,multMatrizRecPar,multStrassen,multStrassenPar)

val m1 = matrizAlAzar(math.pow(2,2).toInt,2)
val m2 = matrizAlAzar(math.pow(2,2).toInt,2)

for (
  i <- 0 to 5 by 2
)yield {
  compararAlgoritmos(
    algoritmosMult(i),algoritmosMult(i+1)
  )(m1,m2)
}

for (
  i <- 0 to 5 by 2
)yield {
  compararAlgoritmos(
    algoritmosMult(i),algoritmosMult(i+1)
  )(m1,m2)
}

for (
  i <- 0 to 5 by 2
)yield {
  compararAlgoritmos(
    algoritmosMult(i),algoritmosMult(i+1)
  )(m1,m2)
}

for (
  i <- 0 to 5 by 2
)yield {
  compararAlgoritmos(
    algoritmosMult(i),algoritmosMult(i+1)
  )(m1,m2)
}

for (
  i <- 0 to 5 by 2
)yield {
  compararAlgoritmos(
    algoritmosMult(i),algoritmosMult(i+1)
  )(m1,m2)
}


for {
  i <- 1 to 7
} yield (compararProdPunto(math.pow(10,i).toInt), math.pow(10,i))








