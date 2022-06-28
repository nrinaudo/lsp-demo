package parser

/** Used to keep track of token location in the original input. */
case class Location(offset: Int, length: Int):
  def mergeWith(other: Location): Location = Location(
    offset,
    other.offset + other.length - offset
  )
