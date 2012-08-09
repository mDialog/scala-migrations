/* preamble goes here */

package com.imageworks.migration

import org.slf4j.LoggerFactory

/* add adapter param if it proves necessary to generate different sql for sequences */

class SequenceDefinition(sequence_name: String,
			 options: SequenceOption*)
{
  private final
  val logger = LoggerFactory.getLogger(this.getClass)

  private val incrementBy: Option[Int] = {
    None +: (options collect { case IncrementBy(x) => Some(x) }) last
  }

  private val startWith: Option[Int] = {
    None +: (options collect { case StartsWith(x) => Some(x) }) last
  }

  private val minValue: Option[Int] = {
    None +: (options collect { case MinValue(x) => Some(x) }) last
  }

  private val maxValue: Option[Int] = {
    None +: (options collect { case MaxValue(x) => Some(x) }) last
  }

  private val cacheSize: Option[Int] = {
    None +: (options collect { case Cache(x) => Some(x) }) last
  }
  
  private def parse_optional_bool(
    match_fragment: PartialFunction[SequenceOption, Option[Boolean]],
    error_fragment: String,
    default: Option[Boolean] = None): Option[Boolean] = {

    val values = options collect match_fragment
    if (values.length > 1)
      logger.warn("Sequence '{}' has multiple {} options",
	Array[AnyRef](sequence_name))

    (default +: values).last
  }

  private
  val noMaxValue: Option[Boolean] = parse_optional_bool({
    case NoMaxValue => Some(true)
    case MaxValue(_) => Some(false)
  }, "MaxValue/NoMaxValue")

  private
  val noMinValue: Option[Boolean] = parse_optional_bool({
    case NoMinValue => Some(true)
    case MinValue(_) => Some(false)
  }, "MinValue/NoMinValue")

  private
  val noCache: Option[Boolean] = parse_optional_bool({
    case NoCache => Some(true)
    case Cache(_) => Some(false)
  }, "Cache/NoCache")

  private
  val noCycle: Option[Boolean] = parse_optional_bool({
    case NoCycle => Some(true)
    case Cycle => Some(false)
  }, "Cycle/NoCycle")

  private
  val noOrder: Option[Boolean] = parse_optional_bool({
    case NoOrder => Some(true)
    case Order => Some(false)
  }, "Order/NoOrder")

  final
  def toSql: String = 
  {
    val sb = new java.lang.StringBuilder(512)
    
    if (incrementBy.isDefined) {
      sb.append(" INCREMENT BY ")
      sb.append(incrementBy.get)
    }

    if (startWith.isDefined) {
      sb.append(" START WITH ")
      sb.append(startWith.get)
    }

    if (noMinValue.isDefined) {
      if (noMinValue.get) {
	sb.append(" NO MINVALUE ")
      } else {
	sb.append(" MINVALUE ")
	sb.append(minValue.get)
      }
    }

    if (noMaxValue.isDefined) {
      if (noMaxValue.get) {
	sb.append(" NO MAXVALUE ")
      } else {
	sb.append(" MAXVALUE ")
	sb.append(maxValue.get)
      }
    }

    if (noCache.isDefined) {
      if (noCache.get) {
	sb.append(" NO CACHE ")
      } else {
	sb.append(" CACHE ")
	sb.append(cacheSize.get)
      }
    }

    if (noOrder.isDefined) {
      if (noOrder.get) {
	sb.append(" NO ORDER ")
      } else {
	sb.append(" ORDER ")
      }
    }

    sb.toString
  }
}
