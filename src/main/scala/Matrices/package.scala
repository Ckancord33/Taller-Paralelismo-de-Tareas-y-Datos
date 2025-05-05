import common._
import scala.collection.parallel.immutable.ParVector
import scala.util.Random
package object Matrices {
  val random = new Random()
  type Matriz = Vector[Vector[Int]]

  def matrizAlAzar(long: Int, vals: Int): Matriz = {
    // Crea una matriz de enteros cuadrada de long x long,
    // con valores aleatorios entre 0 y vals
    val v = Vector.fill(long, long)(random.nextInt(vals))
    v
  }

  def vectorAlAzar(long: Int, vals: Int): Vector[Int] = {
    // Crea un vector de enteros de longitud 'long',
    // con valores aleatorios entre 0 y 'vals'
    val v = Vector.fill(long)(random.nextInt(vals))
    v
  }

  def transpuesta(m: Matriz): Matriz = {
    val l = m.length
    Vector.tabulate(l, l)((i, j) => m(j)(i))
  }

  def prodPunto(v1: Vector[Int], v2: Vector[Int]): Int = {
    (v1 zip v2).map { case (i, j) => (i * j) }.sum
  }

  def prodPuntoParD(v1: ParVector[Int], v2: ParVector[Int]): Int = {
    // A ser usada en el punto 1.5
    (v1 zip v2).map { case (i, j) => (i * j) }.sum
  }

  // Ejercicio 1.1.1
  def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val m2Trans = transpuesta(m2)
    Vector.tabulate(m1.length, m2.length)((i,j) => {
      prodPunto(m1(i), m2Trans(j))
    })
  }

  // Ejercicio 1.1.2
  def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
    val m2Trans = transpuesta(m2)
    def recursiva(m1: Matriz, m2Trans: Matriz): Matriz = {
      val limite = 2
      val mitad = m1.length / 2
      if (mitad > limite) {
        val (pedazo1, pedazo2) = parallel(recursiva(m1.take(mitad), m2Trans), recursiva(m1.drop(mitad), m2Trans))
        pedazo1 ++ pedazo2
      }
      else {
        Vector.tabulate(m1.length, m2.length)((i, j) => {
          prodPunto(m1(i), m2Trans(j))
        })
      }
    }
    recursiva(m1, m2Trans)
  }

  def multMatrizParSheila(m1: Matriz, m2: Matriz): Matriz = {
    val l = m1.length
    val v = Vector.tabulate(l, l)((i, j) => task(prodPunto(m1(i), transpuesta(m2)(j))))
    v.map(x => x.map(y => y.join()))
  }


  // Ejercicio 1.2.1
  def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
    // Dada m, matriz cuadrada de NxN, i<=j<=N, i+l<=N, j+l<=N,
    // devuelve la submatriz de nxn correspondiente a m[i..i+(n-1), j..j+(n-1)]
    Vector.tabulate(l,l)((x,y)=>(m(i+x)(j+y)))
  }

  // Ejercicio 1.2.2
  def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
    // recibe m1 y m2 matrices cuadradas del mismo tamaño, potencia de 2
    // y devuelve la suma de las 2 matrices
    Vector.tabulate(m1.length, m2.length)((i,j) => {m1(i)(j) + m2(i)(j)})
  }

  // Ejercicio 1.2.3
  def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {
    // recibe m1 y m2 matrices cuadradas del mismo tamaño, potencia de 2
    // y devuelve la multiplicación de las 2 matrices
    val longitud = m1.length
    if(longitud > 1) {
      val mitad = m1.length / 2

      val a_11 = subMatriz(m1, 0, 0, mitad)
      val a_12 = subMatriz(m1, 0, mitad, mitad)
      val a_21 = subMatriz(m1, mitad, 0, mitad)
      val a_22 = subMatriz(m1, mitad, mitad, mitad)

      val b_11 = subMatriz(m2, 0, 0, mitad)
      val b_12 = subMatriz(m2, 0, mitad, mitad)
      val b_21 = subMatriz(m2, mitad, 0, mitad)
      val b_22 = subMatriz(m2, mitad, mitad, mitad)

      val c_11 = sumMatriz(multMatrizRec(a_11, b_11), multMatrizRec(a_12, b_21))
      val c_12 = sumMatriz(multMatrizRec(a_11, b_12), multMatrizRec(a_12, b_22))
      val c_21 = sumMatriz(multMatrizRec(a_21, b_11), multMatrizRec(a_22, b_21))
      val c_22 = sumMatriz(multMatrizRec(a_21, b_12), multMatrizRec(a_22, b_22))

      Vector.tabulate(longitud, longitud)((i,j) => {
        if (i < mitad && j < mitad) c_11(i)(j)
        else if (i < mitad && j >= mitad) c_12(i)(j - mitad)
        else if (i >= mitad && j < mitad) c_21(i - mitad)(j)
        else c_22(i - mitad)(j - mitad)
      })

    } else{
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    }
  }

  // Ejercicio 1.2.4
  def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {
    // recibe m1 y m2 matrices cuadradas del mismo tamaño, potencia de 2
    val longitud = m1.length
    val limite = 8
    if (longitud > limite) {
      val mitad = m1.length / 2

      val t1 = task(subMatriz(m1, 0, 0, mitad))
      val t2 = task(subMatriz(m1, 0, mitad, mitad))
      val t3 = task(subMatriz(m1, mitad, 0, mitad))
      val t4 = task(subMatriz(m1, mitad, mitad, mitad))

      val t5 = task(subMatriz(m2, 0, 0, mitad))
      val t6 = task(subMatriz(m2, 0, mitad, mitad))
      val t7 = task(subMatriz(m2, mitad, 0, mitad))
      val t8 = task(subMatriz(m2, mitad, mitad, mitad))

      val (a_11, a_12, a_21, a_22, b_11, b_12, b_21, b_22) =
        (t1.join(), t2.join(), t3.join(), t4.join(), t5.join(), t6.join(), t7.join(), t8.join())

      val (mult1, mult2) = parallel(multMatrizRec(a_11, b_11), multMatrizRec(a_12, b_21))
      val (mult3, mult4) = parallel(multMatrizRec(a_11, b_12), multMatrizRec(a_12, b_22))
      val (mult5, mult6) = parallel(multMatrizRec(a_21, b_11), multMatrizRec(a_22, b_21))
      val (mult7, mult8) = parallel(multMatrizRec(a_21, b_12), multMatrizRec(a_22, b_22))

      val c_11 = sumMatriz(mult1, mult2)
      val c_12 = sumMatriz(mult3, mult4)
      val c_21 = sumMatriz(mult5, mult6)
      val c_22 = sumMatriz(mult7, mult8)

      Vector.tabulate(longitud, longitud)((i, j) => {
        if (i < mitad && j < mitad) c_11(i)(j)
        else if (i < mitad && j >= mitad) c_12(i)(j - mitad)
        else if (i >= mitad && j < mitad) c_21(i - mitad)(j)
        else c_22(i - mitad)(j - mitad)
      })

    } else {
      multMatrizRec(m1, m2)
    }
  }





}
