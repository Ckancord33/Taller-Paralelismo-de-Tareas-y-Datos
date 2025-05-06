import Benchmark.*
import Matrices.*
import org.scalameter.*

for {
  i <- 1 to 6
  m1 = matrizAlAzar(math.pow(2, i).toInt, 2)
  m2 = matrizAlAzar(math.pow(2, i).toInt, 2)
} yield {(
  compararAlgoritmos(
    multMatriz,
    multMatrizPar
  )(m1, m2), math.pow(2, i).toInt)
}

for {
  i <- 1 to 6
  m1 = matrizAlAzar(math.pow(2, i).toInt, 2)
  m2 = matrizAlAzar(math.pow(2, i).toInt, 2)
} yield {(
  compararAlgoritmos(
    multMatrizRec,
    multMatrizRecPar
  )(m1, m2), math.pow(2, i).toInt)
}

val m1 = matrizAlAzar(256, 2)
val m2 = matrizAlAzar(256, 2)
multMatrizPar(m1, m2) == multMatriz(m1, m2)

for {
  i <- 1 to 6
  m1 = matrizAlAzar(math.pow(2, i).toInt, 2)
  m2 = matrizAlAzar(math.pow(2, i).toInt, 2)
} yield {(
  compararAlgoritmos(
    multMatrizRecPar,
    multMatrizRecPar
  )(m1, m2), math.pow(2, i).toInt)
}







