package utils

import scala.io.Source

object Utils:
  def lines(file: String) = Source.fromFile(file).getLines()
