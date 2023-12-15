package utils

import scala.io.Source

object Utils:
  def lines(file: String) = Source.fromFile(file).getLines()
  def commaSeparatedValues(file: String) = lines(file).take(1).flatMap(_.split("\\,"))
