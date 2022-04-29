package adrift

trait BodyState {
  val symptoms: Set[Symptom]
}

trait ThermalBodyState extends BodyState {}

case object SevereHeatstroke extends ThermalBodyState {
  val symptoms = Set(Exhaustion, Nauseous, Dizzy, HeartPounding, Convulsions)
}
case object HeatStroke extends ThermalBodyState {
  val symptoms = Set(Sweating, Flushed, Dizzy, Exhaustion)
}
case object Hot extends ThermalBodyState {
  val symptoms = Set(Sweating, Flushed)
}
case object Warm extends ThermalBodyState {
  val symptoms = Set(Sweating)
}
case object Comfortable extends ThermalBodyState {
  val symptoms: Set[Symptom] = Set.empty
}
case object Chilly extends ThermalBodyState {
  val symptoms = Set(Shivering)
}
case object Cold extends ThermalBodyState {
  val symptoms = Set(Shivering, Chattering)
}
case object Hypothermic extends ThermalBodyState {
  val symptoms = Set(Tingling, Numb, Exhaustion)
}
case object SevereHypothermic extends ThermalBodyState {
  val symptoms = Set(Numb, Sweating, Exhaustion, ColdHot)
}

trait BreathingBodyState extends BodyState {}
case object HyperOxygenated extends BreathingBodyState {
  val symptoms: Set[Symptom] = Set.empty
}
case object Normal extends BreathingBodyState {
  val symptoms: Set[Symptom] = Set.empty
}
case object Hypoxic extends BreathingBodyState {
  val symptoms = Set(Dizzy, Disoriented, Nauseous)
}

trait Symptom {
  val description: String
  val priority: Int
}
case object Disoriented extends Symptom {
  val description: String = "Disoriented"
  val priority = 10
}
case object Comatose extends Symptom {
  val description: String = "Comatose"
  val priority = 11
}
case object Convulsions extends Symptom {
  val description: String = "Convulsions"
  val priority = 10
}
case object Nauseous extends Symptom {
  val description: String = "Nauseous"
  val priority = 9
}
case object Sweating extends Symptom {
  val description: String = "Sweating"
  val priority = 2
}
case object Flushed extends Symptom {
  val description: String = "Flushed"
  val priority = 4
}
case object Exhaustion extends Symptom {
  val description: String = "Exhausted"
  val priority = 7
}
case object Dizzy extends Symptom {
  val description: String = "Nauseous"
  val priority = 7
}
case object HeartPounding extends Symptom {
  val description: String = "High Heart Rate"
  val priority = 8
}
case object Shivering extends Symptom {
  val description: String = "Shivering"
  val priority = 2
}
case object Chattering extends Symptom {
  val description: String = "Chattering"
  val priority = 5
}
case object Tingling extends Symptom {
  val description: String = "Tingling"
  val priority = 8
}
case object Numb extends Symptom {
  val description: String = "Numb"
  val priority = 9
}
case object ColdHot extends Symptom {
  val description: String = "Hot?"
  val priority = 10
}

