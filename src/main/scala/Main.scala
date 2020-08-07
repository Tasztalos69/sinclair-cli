import java.net.{DatagramPacket, DatagramSocket, InetSocketAddress, SocketTimeoutException}

import scala.util.control.Breaks._
import org.rogach.scallop._
import spray.json._
import javax.crypto.Cipher
import org.apache.commons.codec.binary.Base64
import javax.crypto.spec.SecretKeySpec
import spray.json.DefaultJsonProtocol.{JsValueFormat, StringJsonFormat, mapFormat}

class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  version("v1.0.0 | MIT | Kiss Benedek Mate")
  val search = new Subcommand("search", "s") {

  }
  val get = new Subcommand("get", "g") {
    val help = opt[Boolean]()
    val prop = trailArg[String]()
  }
  addSubcommand(search)
  addSubcommand(get)
  verify()
}

object Main {
  val testArgs: Array[String] = Array("search")
  val GENERIC_KEY: String = "a3K8Bx%2r8Y7#xDh"

  def keyToSpec(key: String): SecretKeySpec = {
    var keyBytes = Array.fill[Byte](1024)(0)
    keyBytes = key.getBytes("UTF-8")
    new SecretKeySpec(keyBytes, "AES")
  }

  def create_cipher(key: String): Cipher = {
    val cipher: Cipher = Cipher.getInstance("AES/ECB/PKCS5PADDING")
    cipher.init(Cipher.DECRYPT_MODE, keyToSpec(key))
    cipher
  }

  def decrypt(pack_encoded: String, key: String): Array[Byte] = {
    val decryptor = create_cipher(key)
    var pack_decoded = Array.fill[Byte](1024)(0)
    pack_decoded = Base64.decodeBase64(pack_encoded)
    var pack_decrypted = Array.fill[Byte](1024)(0)
    pack_decrypted = decryptor.doFinal(pack_decoded)
    pack_decrypted
  }

  def decrypt_generic(pack_encoded: String): Array[Byte] = decrypt(pack_encoded, GENERIC_KEY)

  def main(args: Array[String]): Unit = {
    val conf = new Conf(testArgs)
    if (conf.subcommand.contains(conf.search)) {
      val msg = """{"t":"scan"}""".getBytes()
      val addr = new InetSocketAddress("192.168.0.255", 7000)
      val p = new DatagramPacket(msg, msg.length, addr)
      val s = new DatagramSocket()
      s.setSoTimeout(5000)
      s.setBroadcast(true)
      s.setReuseAddress(true)
      s.send(p)

      breakable {
        while (true) {
          val in = Array.fill[Byte](1024)(0)
          val recv = new DatagramPacket(in, in.length)
          try {
            println("Beginning receive")
            s.receive(recv)
            val jsonRecv = JsonParser(new String(recv.getData, 0, recv.getLength))
            val map = jsonRecv.convertTo[Map[String, JsValue]]
            val decrypted = decrypt_generic(map("pack").toString().substring(1, map("pack").toString().length - 1))
            val jsonDecrypted = JsonParser(new String(decrypted, 0, decrypted.length))
            println(jsonDecrypted.prettyPrint)
          } catch {
            case _: SocketTimeoutException =>
              println("Timeout reached")
              break()
          }
        }
      }

    }
    if (conf.subcommand.contains(conf.get)) println("get supplied")
  }
}
