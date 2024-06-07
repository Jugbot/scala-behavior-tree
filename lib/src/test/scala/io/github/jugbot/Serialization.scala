package io.github.jugbot

import io.circe.syntax._
import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.matchers.should.Matchers
import io.github.jugbot.{
  SelectorNode,
  state,
  Failure,
  ActionNode,
  SequenceNode,
  Success,
  Status
}
import io.circe.Encoder
import io.circe.generic.semiauto._
import io.circe.Codec
import io.circe.generic.auto.deriveEncoder
import io.circe.Json
import io.circe.derivation.Configuration

// implicit val encodeNode = new Encoder[Node[]] {
//   final def apply(a: Node[Any]): Json = a match {
//       case ActionNode(actionNode)     => actionNode.asJson
//       case SequenceNode(sequenceNode*) => sequenceNode.toList.asJson
//       case SelectorNode(selectorNode*) => selectorNode.toList.asJson
//   }
// }

given Configuration = Configuration.default
  .withDiscriminator("type")
  .withTransformConstructorNames(_.toUpperCase)

@RunWith(classOf[JUnitRunner])
class Serialization extends AnyFunSuite with Matchers {
  test("Converts to json") {
    // sealed trait ExampleBehavior
    // case object EAT extends ExampleBehavior
    // case object SLEEP extends ExampleBehavior
    // case object RAVE extends ExampleBehavior
    // case object RE_EAT extends ExampleBehavior

    enum ExampleBehaviors {
      case EAT
      case SLEEP
      case RAVE
      case RE_EAT
    }

    val bt = SequenceNode(
      ActionNode(ExampleBehaviors.EAT),
      ActionNode(ExampleBehaviors.RE_EAT)
    )

    def encodeHelper[T: Encoder](name: String, value: T) =
      Json.obj(
        ("type", Json.fromString("ActionNode")),
        ("value", value.asJson)
      )

    given [A: Encoder]: Encoder[ActionNode[A]] with
      def apply(a: ActionNode[A]): Json = a.action.asJson

    given [A: Encoder]: Encoder[SequenceNode[A]] with
      def apply(a: SequenceNode[A]): Json = a.children.asJson

    given [A: Encoder]: Encoder[SelectorNode[A]] with
      def apply(a: SelectorNode[A]): Json = a.children.asJson

    given [A: Encoder]: Encoder[Node[A]] with
      def apply(a: Node[A]): Json = a match
        case actionNode: ActionNode[A] =>
          summon[Encoder[ActionNode[A]]].apply(actionNode)
        case sequenceNode: SequenceNode[A] =>
          summon[Encoder[SequenceNode[A]]].apply(sequenceNode)
        case selectorNode: SelectorNode[A] =>
          summon[Encoder[SelectorNode[A]]].apply(selectorNode)

    val jsonBt = bt.asJson
    print(jsonBt)
  }

}
