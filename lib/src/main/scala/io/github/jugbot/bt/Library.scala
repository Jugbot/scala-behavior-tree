package io.github.jugbot.bt

// Define the Node and its types
sealed trait Node[A]
case class ActionNode[A](action: A) extends Node[A]
case class SequenceNode[A](children: Node[A]*) extends Node[A]
case class SelectorNode[A](children: Node[A]*) extends Node[A]

// Status Enum
sealed trait Status
case object Success extends Status
case object Failure extends Status

// Method for running nodes
def state[A](node: Node[A], action: Function[A, Status]): Status = node match {
  case ActionNode(a)           => action(a)
  case SequenceNode(children*) => runSequence(children, n => state(n, action))
  case SelectorNode(children*) => runSelector(children, n => state(n, action))
}

// Method for running sequence nodes
def runSequence[A](
    nodes: Seq[Node[A]],
    cb: Function[Node[A], Status]
): Status = {
  nodes.foldLeft[Status](Success) { (acc, node) =>
    if (acc == Success)
      cb(node)
    else Failure
  }
}

// Method for running selector nodes
def runSelector[A](
    nodes: Seq[Node[A]],
    cb: Function[Node[A], Status]
): Status = {
  nodes.foldLeft[Status](Failure) { (acc, node) =>
    if (acc == Failure)
      cb(node)
    else Success
  }
}
