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
      val limite = 4
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

      val a_11 = subMatriz(m1, 0, 0, mitad)
      val a_12 = subMatriz(m1, 0, mitad, mitad)
      val a_21 = subMatriz(m1, mitad, 0, mitad)
      val a_22 = subMatriz(m1, mitad, mitad, mitad)

      val b_11 = subMatriz(m2, 0, 0, mitad)
      val b_12 = subMatriz(m2, 0, mitad, mitad)
      val b_21 = subMatriz(m2, mitad, 0, mitad)
      val b_22 = subMatriz(m2, mitad, mitad, mitad)

      val t1 = task(parallel(multMatrizRec(a_11, b_11), multMatrizRec(a_12, b_21)))
      val t2 = task(parallel(multMatrizRec(a_11, b_12), multMatrizRec(a_12, b_22)))
      val t3 = task(parallel(multMatrizRec(a_21, b_11), multMatrizRec(a_22, b_21)))
      val t4 = task(parallel(multMatrizRec(a_21, b_12), multMatrizRec(a_22, b_22)))

      val (mult1, mult2) = t1.join()
      val (mult3, mult4) = t2.join()
      val (mult5, mult6) = t3.join()
      val (mult7, mult8) = t4.join()

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

  def restaMatriz(m1:Matriz,m2:Matriz): Matriz = {
    Vector.tabulate(m1.length, m2.length)((i, j) => {
      m1(i)(j) - m2(i)(j)
    })
  }

  def multStrassen(m1:Matriz,m2:Matriz): Matriz = {
    val longitud = m1.length
    if(longitud > 1){
      val mitad = m1.length / 2

      val a_11 = subMatriz(m1, 0, 0, mitad)
      val a_12 = subMatriz(m1, 0, mitad, mitad)
      val a_21 = subMatriz(m1, mitad, 0, mitad)
      val a_22 = subMatriz(m1, mitad, mitad, mitad)

      val b_11 = subMatriz(m2, 0, 0, mitad)
      val b_12 = subMatriz(m2, 0, mitad, mitad)
      val b_21 = subMatriz(m2, mitad, 0, mitad)
      val b_22 = subMatriz(m2, mitad, mitad, mitad)

      val s1 = restaMatriz(b_12,b_22)
      val s2 = sumMatriz(a_11,a_12)
      val s3 = sumMatriz(a_21,a_22)
      val s4 = restaMatriz(b_21,b_11)
      val s5 = sumMatriz(a_11,a_22)
      val s6 = sumMatriz(b_11,b_22)
      val s7 = restaMatriz(a_12,a_22)
      val s8 = sumMatriz(b_21,b_22)
      val s9 = restaMatriz(a_11,a_21)
      val s10 = sumMatriz(b_11,b_12)

      val p1 = multStrassen(a_11,s1)
      val p2 = multStrassen(s2,b_22)
      val p3 = multStrassen(s3,b_11)
      val p4 = multStrassen(a_22,s4)
      val p5 = multStrassen(s5,s6)
      val p6 = multStrassen(s7,s8)
      val p7 = multStrassen(s9,s10)

      val c_11 = sumMatriz(restaMatriz(sumMatriz(p5, p4), p2), p6)
      val c_12 = sumMatriz(p1,p2)
      val c_21 = sumMatriz(p3,p4)
      val c_22 = restaMatriz(restaMatriz(sumMatriz(p5, p1), p3), p7)

      Vector.tabulate(longitud, longitud)((i, j) => {
        if (i < mitad && j < mitad) c_11(i)(j)
        else if (i < mitad && j >= mitad) c_12(i)(j - mitad)
        else if (i >= mitad && j < mitad) c_21(i - mitad)(j)
        else c_22(i - mitad)(j - mitad)
      })


    }else{
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    }

  }

  def multStrassenPar(m1:Matriz,m2:Matriz): Matriz ={
    val longitud = m1.length
    val limite = 32
    if (longitud > limite) {
      val mitad = m1.length / 2

      val a_11 = subMatriz(m1, 0, 0, mitad)
      val a_12 = subMatriz(m1, 0, mitad, mitad)
      val a_21 = subMatriz(m1, mitad, 0, mitad)
      val a_22 = subMatriz(m1, mitad, mitad, mitad)

      val b_11 = subMatriz(m2, 0, 0, mitad)
      val b_12 = subMatriz(m2, 0, mitad, mitad)
      val b_21 = subMatriz(m2, mitad, 0, mitad)
      val b_22 = subMatriz(m2, mitad, mitad, mitad)

      val s1 = restaMatriz(b_12, b_22)
      val s2 = sumMatriz(a_11, a_12)
      val s3 = sumMatriz(a_21, a_22)
      val s4 = restaMatriz(b_21, b_11)
      val s5 = sumMatriz(a_11, a_22)
      val s6 = sumMatriz(b_11, b_22)
      val s7 = restaMatriz(a_12, a_22)
      val s8 = sumMatriz(b_21, b_22)
      val s9 = restaMatriz(a_11, a_21)
      val s10 = sumMatriz(b_11, b_12)

      val t1 = task(parallel(multStrassenPar(a_11, s1),multStrassenPar(s2, b_22)))
      val t2 = task(parallel(multStrassenPar(s3, b_11),multStrassenPar(a_22, s4)))
      val t3 = task(parallel(multStrassenPar(s5, s6),multStrassenPar(s7, s8)))
      val t4 = task(multStrassenPar(s9, s10))

      val (mult1, mult2) = t1.join()
      val (mult3, mult4) = t2.join()
      val (mult5, mult6) = t3.join()
      val mult7 = t4.join()

      val c_11 = sumMatriz(restaMatriz(sumMatriz(mult5, mult4), mult2), mult6)
      val c_12 = sumMatriz(mult1, mult2)
      val c_21 = sumMatriz(mult3, mult4)
      val c_22 = restaMatriz(restaMatriz(sumMatriz(mult5, mult1), mult3), mult7)

      Vector.tabulate(longitud, longitud)((i, j) => {
        if (i < mitad && j < mitad) c_11(i)(j)
        else if (i < mitad && j >= mitad) c_12(i)(j - mitad)
        else if (i >= mitad && j < mitad) c_21(i - mitad)(j)
        else c_22(i - mitad)(j - mitad)
      })


    } else {
      multStrassen(m1,m2)
    }
  }





}
