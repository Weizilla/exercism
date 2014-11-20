class Bob {

  def hey(input: String): String = {
    val default = "Whatever."
    val shouting = "Whoa, chill out!"
    val question = "Sure."
    val silence = "Fine. Be that way!"

    input match {
      case _ if isSilence(input) => silence
      case _ if isQuestion(input) => question
      case _ if isShouting(input) => shouting
      case _ => default
    }
  }

  def isShouting(input: String): Boolean = {
    isAllCaps(input) && hasLetters(input)
  }

  def isQuestion(input: String): Boolean = {
    ! hasLetters(input) && input.last == '?' ||
    input.last == '?' && ! isShouting(input)
  }

  def isSilence(input: String): Boolean = {
    input.trim.isEmpty
  }

  def isAllCaps(input: String): Boolean = {
    "[a-z]+".r.findFirstIn(input).isEmpty
  }

  def hasLetters(input: String): Boolean = {
    "[a-zA-Z]+".r.findFirstIn(input).isDefined
  }
}
