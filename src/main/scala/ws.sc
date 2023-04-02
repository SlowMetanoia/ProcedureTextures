import breeze.linalg.{ DenseMatrix, DenseVector }

val dm = DenseMatrix.zeros[Double](2,4)

val dm2:DenseMatrix[Double] = DenseMatrix((0.1,1.1),(0.0,2.9))

val t = DenseVector(1,2,3,4)
val s = DenseVector(1,10,100,1000)

s.dot(t)

def T1(t:Double): DenseVector[ Double ] = DenseVector(1, t, t * t, t * t * t)
def T(t:DenseVector[Double]):DenseMatrix[Double] = DenseMatrix.create(4,t.length,t.map(v=>T1(v)).map(_.toArray).toArray.flatten).t

T1(2.0)
T(DenseVector(2,3))
