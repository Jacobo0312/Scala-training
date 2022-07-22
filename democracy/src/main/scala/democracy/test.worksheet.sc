enum Grade:
  case Bad, Mediocre, Inadequate, Passable, Good, VeryGood, Excellent


Grade.Bad.ordinal
Grade.Mediocre.ordinal
Grade.Inadequate.ordinal
Grade.Passable.ordinal
Grade.Good.ordinal
Grade.VeryGood.ordinal
Grade.Excellent.ordinal




val grades:Seq[Grade] = Seq(Grade.Bad,Grade.Bad,Grade.Bad,Grade.Mediocre, Grade.Inadequate, Grade.Passable, Grade.Good, Grade.VeryGood, Grade.Excellent,Grade.Bad,Grade.Bad,Grade.Passable,Grade.Passable)


  def median(grades: Seq[Grade]): Grade =
    val sortGrades=grades.sortBy(_.ordinal)
    val mid = sortGrades((sortGrades.size/2)) 
    mid
end Grade

grades.size

grades.size/2

median(grades)



case class Candidate(name: String)

case class Ballot(grades: Map[Candidate, Grade])

val tiramisu    = Candidate("Tiramisu")
val cremeBrulee = Candidate("Crème brûlée")
val cheesecake  = Candidate("Cheesecake")



val ballot1 = Ballot(
      Map(
        tiramisu    -> Grade.Excellent,
        cremeBrulee -> Grade.Good,
        cheesecake  -> Grade.Inadequate
      )
    )

val ballot2 = Ballot(
      Map(
        tiramisu    -> Grade.Excellent,
        cremeBrulee -> Grade.Passable,
        cheesecake  -> Grade.Good
      )
    )

    val ballot3 = Ballot(
      Map(
        tiramisu    -> Grade.VeryGood,
        cremeBrulee -> Grade.Inadequate,
        cheesecake  -> Grade.Good
      )
    )

val ballots = Seq(ballot1, ballot2, ballot3)



val allGrades: Seq[(Candidate, Grade)] =
  ballots.flatMap(ballot =>
    ballot.grades.map(candidateGrade =>
      (candidateGrade._1, candidateGrade._2)))

import scala.collection.mutable.ArrayBuffer

val example=ArrayBuffer(1,2)

example.flatMap(x => ArrayBuffer(x, x * 2,x*x))


val gradesPerCandidate: Map[Candidate, Seq[Grade]] =
allGrades.groupMap(_._1)(_._2)


      // Otherwise, find the highest median grade assigned to a candidate.
      // Use the operation `values` to select the collections of grades,
      // then use the operation `filter` to keep only the non empty grades,
      // then use the operation `map` to compute the median value of each collection
      // of grades, and finally use the operation `maxBy` to find the highest
      // median grade.

gradesPerCandidate.values  

val bestMedianGrade: Grade =gradesPerCandidate.values
  .filter(_.nonEmpty)
    .map(grades=>median(grades))
      .maxBy(_.ordinal)


gradesPerCandidate
      // Use the operation `filter` to select all the candidates that got the
      // same best median grade (as the case may be)
val bestCandidates: Map[Candidate, Seq[Grade]] =
  gradesPerCandidate.filter(_._2.contains(bestMedianGrade))


bestCandidates.head._1

        // Use the operation `map` to transform each element of the `bestCandidates`.
        // And use the operation `diff` to remove one `bestMedianGrade` from the
        // grades assigned to the candidates.
val bestCandidatesMinusOneMedianGrade: Map[Candidate, Seq[Grade]] =
  bestCandidates.map(candidateGrade=>(candidateGrade._1,candidateGrade._2.diff(Seq(bestMedianGrade))))
