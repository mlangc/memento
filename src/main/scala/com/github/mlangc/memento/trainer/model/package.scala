package com.github.mlangc.memento.trainer

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval
import shapeless.{Witness => W}


package object model {
  type UnitIntervalRefinement = Interval.Closed[W.`0.0`.T, W.`1.0`.T]
  type RevealedRatio = Double Refined UnitIntervalRefinement
}
