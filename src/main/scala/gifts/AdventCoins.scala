package gifts

import java.security.MessageDigest

object AdventCoins {
  def md5Hash(input: String) = {
    val md: MessageDigest = MessageDigest.getInstance("MD5")
    md.digest(input.getBytes()).foldLeft("")(_+"%02x".format(_))
  }

  def magicHash(input: String, prefixExpected: String): Option[Int] = {
    (0 to 100000000).find(i ⇒ md5Hash(input + i).startsWith(prefixExpected))
  }
}
