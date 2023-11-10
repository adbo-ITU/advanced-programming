package adbo.qlearning

enum Action:
  case Up, Down, Left, Right

type Coordinate = (Int, Int)

object Main:
  def main(args: Array[String]) =
    val (width, height) = (3, 3)

    val learner = QLearning(
      initialQTable = computeInitialQTable(width, height),
      (0, 0),
      (0, width - 1)
    )
    val policy = learner.learnPolicy

    println(
      s"α=${learner.stepSize}, ε=${learner.randomChoiceProbability}, γ=${learner.discountFactor}, and N=${learner.numIterations}"
    )
    println(policy)

def computeInitialQTable(
    width: Int,
    height: Int
): QTable[Coordinate, Action] =
  val states = for
    x <- 0 until width
    y <- 0 until height
  yield (x, y)

  val actions = List(Action.Up, Action.Down, Action.Left, Action.Right)

  val statesWithActions = states
    .map(state => {
      val possibleActions = actions
        .filter(action => {
          val (x, y) = state

          action match
            case Action.Up    => y < height - 1
            case Action.Down  => y > 0
            case Action.Left  => x > 0
            case Action.Right => x < width - 1
        })
        .map(action => (action, 0.0))
        .toMap

      (state, possibleActions)
    })
    .toMap

  statesWithActions
