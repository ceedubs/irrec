package ceedubs.irrec
package regex

// TODO document
trait CaptureAs[M] {
  type In
}

object CaptureAs {
  type Aux[M, In0] = CaptureAs[M] {
    type In = In0
  }

  private val witness: CaptureAs[Nothing] = new CaptureAs[Nothing] {
    type In = Nothing
  }

  def instance[M, In]: Aux[M, In] = witness.asInstanceOf[Aux[M, In]]

  implicit def matchCaptureAs[A]: Aux[Match[A], A] = instance
}
