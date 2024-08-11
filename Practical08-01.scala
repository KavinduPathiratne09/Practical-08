import scala.io.StdIn._

object CaesarCipher {

  def encrypt(text: String, shift: Int): String = {
    text.map {
      case c if c.isLetter =>
        val shiftAmount = if (c.isUpper) 'A' else 'a'
        ((c - shiftAmount + shift) % 26 + shiftAmount).toChar
      case c => c
    }
  }

  def decrypt(text: String, shift: Int): String = {
    encrypt(text, 26 - shift)
  }

  def cipher(text: String, shift: Int, func: (String, Int) => String): String = {
    func(text, shift)
  }

  def main(args: Array[String]): Unit = {
    print("Enter the text to be encrypted: ")
    val text = readLine()

    print("Enter the shift value for the encryption: ")
    val encryptionShift = readInt()

    val encryptedText = cipher(text, encryptionShift, encrypt)
    println(s"Encrypted Text: $encryptedText")

    print("Enter the text to be decrypted: ")
    val text2 = readLine()

    print("Enter the shift value: ")
    val decryptionShift = readInt()

    val decryptedText = cipher(text2, decryptionShift, decrypt)
    println(s"Decrypted Text: $decryptedText")
  }
}