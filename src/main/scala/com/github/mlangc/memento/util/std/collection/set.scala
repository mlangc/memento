package com.github.mlangc.memento.util.std.collection

import scala.annotation.tailrec

object set {
  implicit class SetOps[T](val set: Set[T]) extends AnyVal {
    def connectedSets(edgeExists: (T, T) => Boolean): Set[Set[T]] = {
      @tailrec
      def connectedSetFor(justAdded: Set[T], remaining: Set[T], previouslyAdded: Set[T] = Set()): (Set[T], Set[T]) = {
        if (justAdded.isEmpty) {
          (previouslyAdded, remaining)
        } else {
          val (newlyAdded, stillRemaining) = remaining.partition(elem => justAdded.exists(edgeExists(_, elem)))
          connectedSetFor(justAdded = newlyAdded, remaining = stillRemaining, previouslyAdded = previouslyAdded ++ justAdded)
        }
      }

      @tailrec
      def connectedSetsInternal(acc: Set[Set[T]] = Set(), remaining: Set[T] = set): Set[Set[T]] = {
        if (remaining.isEmpty) {
          acc
        } else {
          val (head, tail) = (Set(remaining.head), remaining.tail)
          val (newComponent, stillRemaining) = connectedSetFor(head, tail)
          connectedSetsInternal(acc + newComponent, stillRemaining)
        }
      }

      connectedSetsInternal()
    }
  }
}
