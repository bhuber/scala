object Test {
  trait A
  trait B extends A
  trait C extends B

  object f1 {
    trait Covariant[+X]
    type Expected = Covariant[A]
    type CandidateB = Covariant[B]
    type CandidateC = Covariant[C]
    implicitly[CandidateB <:< Expected]
    implicitly[CandidateC <:< Expected]
    implicitly[CandidateC <:< CandidateB]

    implicit val cb: CandidateB = new CandidateB { override def toString = "cb" }
    implicit val cc: CandidateC = new CandidateC { override def toString = "cc" }
  }
  object f2 {
    trait Contravariant[-X]
    type Expected = Contravariant[C]
    type CandidateB = Contravariant[B]
    type CandidateA = Contravariant[A]
    implicitly[CandidateA <:< Expected]
    implicitly[CandidateB <:< Expected]
    implicitly[CandidateA <:< CandidateB]

    implicit val cb: CandidateB = new CandidateB { override def toString = "cb" }
    implicit val ca: CandidateA = new CandidateA { override def toString = "ca" }
  }

  def main(args: Array[String]): Unit = {
    // compiler chooses CandidateC because CandidateC <:< CandidateB
    { import f1._ ; println(implicitly[Expected]) ; assert(implicitly[Expected] eq cc) }

    // compiler chooses CandidateA because CandidateA <:< CandidateB.
    // not any more!
    { import f2._ ; println(implicitly[Expected]) ; assert(implicitly[Expected] eq cb) }
  }
}
