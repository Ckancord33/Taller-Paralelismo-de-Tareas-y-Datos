import Matrices._
import org.scalameter._

val m1= matrizAlAzar(128, 2)
val m2= matrizAlAzar(128, 2)
measure { multMatriz(m1, m2)}
measure { multMatrizPar(m1, m2) }

val matrizPrueba = Vector(
  Vector(1, 2, 3, 4),
  Vector(5, 6, 7, 8),
  Vector(9, 10, 11, 12),
  Vector(13, 14, 15, 16)
)
subMatriz(matrizPrueba, 0, 0, 2)

val m3= matrizAlAzar(128, 2)
val m4= matrizAlAzar(128, 2)

measure {multMatrizRec(m3, m4)}
measure {multMatrizRecPar(m3, m4)}

multMatriz(m1, m2) == multMatrizPar(m1, m2) && multMatrizPar(m1, m2) == multMatrizRec(m1, m2) && multMatrizRec(m1, m2) == multMatrizRecPar(m1, m2)








