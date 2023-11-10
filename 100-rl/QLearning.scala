package adbo.qlearning

type QTable[S, A] = Map[S, Map[A, Double]]

object QTable:
  def initialize[S, A](
      states: List[S],
      actions: List[A]
  ): QTable[S, A] =
    states
      .map(state => {
        val actionScores = actions
          .map(action => {
            (action, 0.0)
          })
          .toMap

        (state, actionScores)
      })
      .toMap

extension [S, A](qTable: QTable[S, A])
  def toPolicy: Policy[S, A] =
    qTable.map((state, actionScores) => {
      val (action, _) = actionScores.maxBy(_._2)
      (state, action)
    })

type Policy[S, A] = Map[S, A]

case class QLearning[S, A](
    initialQTable: QTable[S, A],
    startState: S,
    endState: S,
    stepSize: Double = 0.1,
    discountFactor: Double = 1.0,
    randomChoiceProbability: Double = 0.1,
    numIterations: Int = 400
):
  def epoggers(
      qTable: QTable[S, A]
  ): QTable[S, A] =
    ???
    qTable

  def learnPolicy: Policy[S, A] =
    val qTable = initialQTable
    val nextQTable = epoggers(qTable)

    nextQTable.toPolicy
