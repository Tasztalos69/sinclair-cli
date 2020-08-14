import java.io.{File, FileNotFoundException, PrintWriter}
import java.net._

import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec
import org.apache.commons.codec.binary.Base64
import org.rogach.scallop._
import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.Console._
import scala.io.Source
import scala.io.StdIn.readLine
import scala.jdk.CollectionConverters._
import scala.util.control.Breaks._

// CLI Syntax constructor

class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  version("v1.0.0 | MIT | Kiss Benedek Máté")

  val search = new Subcommand("search", "s")
  val ac = new Subcommand("ac") {
    val prop = trailArg[String]("property", "The name of the requested key.", required = false)
    val value = trailArg[String]("value", "The value to set the AC to.", required = false)
  }

  val config = new Subcommand("config", "c") {
    val entry = trailArg[String]("entry", "The name of the config entry.", required = false)
    val value = trailArg[String]("value", "The value to set the config entry to.", required = false)
  }
  val devices = new Subcommand("devices", "d") {
    val command = trailArg[String]("entry", "The name of the devices command.", required = true)
    val value = trailArg[String]("value", "The value to set the devices entry to.", required = false)
  }
  val help = opt[Boolean](hidden = true)
  addSubcommand(config)
  addSubcommand(devices)
  addSubcommand(search)
  addSubcommand(ac)
  verify()
}

// Helper classes

class SearchResult(var incId: Int, var ip: String = "", var port: Int = 0, var id: String = "", var name: String = "Unknown")

object SearchResult {
  def apply(incId: Int, ip: String, port: Int, id: String, name: String): SearchResult = new SearchResult(incId, ip, port, id, name)
}

class BetterString(val s: String) {
  def stripQ: String = s.substring(1, s.length - 1)
}

object Implicits {
  implicit def stringToString(s: String): BetterString = new BetterString(s)
}

import Implicits._

object Main {
  val GENERIC_KEY: String = "a3K8Bx%2r8Y7#xDh"

  // Helper FNs

  def getDefaultBroadcast: Option[String] = {
    val interfaces = NetworkInterface.getNetworkInterfaces
    while (interfaces.hasMoreElements) {
      val itf = interfaces.nextElement()
      if (!itf.isLoopback) {
        val addresses = itf.getInterfaceAddresses.asScala
        for (addr: InterfaceAddress <- addresses) {
          val bc = addr.getBroadcast
          if (bc != null) {
            return Some(bc.toString.substring(1))
          }
        }
      }
    }
    None
  }

  def keyToSpec(key: String): SecretKeySpec = {
    var keyBytes = Array.fill[Byte](1024)(0)
    keyBytes = key.getBytes("UTF-8")
    new SecretKeySpec(keyBytes, "AES")
  }

  def create_cipher(key: String, mode: String): Cipher = {
    val cipher: Cipher = Cipher.getInstance("AES/ECB/PKCS5PADDING")
    if (mode == "decrypt") {
      cipher.init(Cipher.DECRYPT_MODE, keyToSpec(key))
    } else if (mode == "encrypt") {
      cipher.init(Cipher.ENCRYPT_MODE, keyToSpec(key))
    } else throw new Exception
    cipher
  }

  // Cryptography FNs

  def decrypt(pack_encoded: String, key: String): Array[Byte] = {
    val decryptor = create_cipher(key, "decrypt")
    val pack_decoded = Base64.decodeBase64(pack_encoded)
    val pack_decrypted = decryptor.doFinal(pack_decoded)
    pack_decrypted
  }

  def encrypt(pack: String, key: String): String = {
    val encryptor = create_cipher(key, "encrypt")
    val pack_encrypted = encryptor.doFinal(pack.getBytes)
    val pack_encoded = Base64.encodeBase64(pack_encrypted)
    pack_encoded.map(_.toChar).mkString("")
  }

  def decrypt_generic(pack_encoded: String): Array[Byte] = decrypt(pack_encoded, GENERIC_KEY)

  def encrypt_generic(pack: String): String = encrypt(pack, GENERIC_KEY)

  // Data exchange FNs

  def create_request(tcid: String = "0", pack_encrypted: String, i: Int = 0): String = {
    s"""{"cid":"app","i":${i.toString},"t":"pack","uid":0,"tcid":$tcid,"pack":"$pack_encrypted"}"""
  }

  def sendData(ip: String, port: Int, data: Array[Byte]): DatagramPacket = {
    val addr = new InetSocketAddress(ip, port)
    val p = new DatagramPacket(data, data.length, addr)
    val s = new DatagramSocket()
    s.setSoTimeout(5000)
    s.setReuseAddress(true)
    s.send(p)
    val in = Array.fill[Byte](1024)(0)
    val recv = new DatagramPacket(in, in.length)
    s.receive(recv)
    recv
  }

  def bind(res: SearchResult): Map[String, String] = {
    println(s"Binding to ${res.name} at ${res.ip}...")
    val pack = s"""{"mac":${res.id},"t":"bind","uid":0}"""
    val pack_encrypted = encrypt_generic(pack)
    val request = create_request(res.id, pack_encrypted, 1)
    val recv = sendData(res.ip, 7000, request.getBytes)
    val jsonRecv = JsonParser(new String(recv.getData, 0, recv.getLength))
    val mapRecv = jsonRecv.convertTo[Map[String, JsValue]]
    if (mapRecv("t").toString == """"pack"""") {
      val decrypted = decrypt_generic(mapRecv("pack").toString().stripQ)
      val jsonDecrypted = JsonParser(new String(decrypted, 0, decrypted.length))
      val mapDecrypted = jsonDecrypted.convertTo[Map[String, JsValue]]
      if (mapDecrypted("t").toString == """"bindok"""") {
        println("Bind successful.")
        val key = mapDecrypted("key")
        val data = Map[String, String]("incId" -> res.incId.toString, "id" -> res.id.stripQ, "ip" -> res.ip, "key" -> key.toString.stripQ, "default" -> "false")
        data
      } else {
        Map()
      }
    } else {
      Map()
    }
  }

  def printErr(in: String): Unit = {
    println(s"${RED}Error:$RESET $in")
    System.exit(1)
  }

  // CLI functions

  def search(config: JsObject): Unit = {
    if (config.fields.contains("devices")) {
      print("Warning: there are previously bound devices. Do you want to overwrite them? (y/N) ")
      val resp = readLine()
      if (resp.toUpperCase != "Y" && resp.toUpperCase != "YES") {
        println("Aborting.")
        System.exit(0)
      }
    }
    if (!config.fields.contains("broadcastAddress")) {
      println("Searching for default broadcast address...\n(Alternatively, you can set the broadcast address with scli config broadcast <ip>)")
      val bc = getDefaultBroadcast.getOrElse("")
      if (bc.isEmpty) {
        println("Error: cannot get default broadcast address.")
        System.exit(1)
      } else {
        println(s"Found broadcast address $bc, and setting it as default.")
        val pw = new PrintWriter(new File("config.json"))
        val newConfig = JsObject(config.fields + ("broadcastAddress" -> bc.toJson))
        pw.write(newConfig.prettyPrint)
        pw.close()
      }
    }
    println("Searching...")
    val msg = """{"t":"scan"}""".getBytes()
    val addr = new InetSocketAddress(config.fields("broadcastAddress").toString.stripQ, 7000)
    val p = new DatagramPacket(msg, msg.length, addr)
    val s = new DatagramSocket()
    s.setSoTimeout(5000)
    s.setBroadcast(true)
    s.setReuseAddress(true)
    s.send(p)

    var results: Array[SearchResult] = Array()
    var incId = 1
    breakable {
      while (true) {
        val in = Array.fill[Byte](1024)(0)
        val recv = new DatagramPacket(in, in.length)
        try {
          s.receive(recv)
          if (recv.getLength > 0) {
            val jsonRecv = JsonParser(new String(recv.getData, 0, recv.getLength))
            val recvMap = jsonRecv.convertTo[Map[String, JsValue]]
            val decrypted = decrypt_generic(recvMap("pack").toString().stripQ)
            val jsonDecrypted = JsonParser(new String(decrypted, 0, decrypted.length))
            val decryptedMap = jsonDecrypted.convertTo[Map[String, JsValue]]
            var addr = recv.getAddress.toString
            if (addr.startsWith("/")) addr = addr.substring(1)
            results = results :+ SearchResult(incId, addr, recv.getPort, decryptedMap("cid").toString, if (decryptedMap.contains("name")) decryptedMap("name").toString else "Unknown")
            incId = incId + 1
          }
        } catch {
          case _: SocketTimeoutException =>
            println(s"Found ${results.length} devices.")
            break()
        }
      }
    }
    if (results.length == 0) {
      System.exit(0)
    }
    var binds: Array[Map[String, String]] = Array()
    for (r <- results) {
      binds = binds :+ bind(r)
    }
    if (binds.length == 1) {
      println("Setting the device as default...")
      binds(0) = binds(0) + ("default" -> "true")
    } else {
      println(s"${YELLOW}Warning:$RESET Found multiple devices. To use the shorthand command (without specifying a device), use: scli config default <id>")
    }
    val pw = new PrintWriter(new File("config.json"))
    val newConfig = JsObject(config.fields + ("devices" -> binds.toJson))
    pw.write(newConfig.prettyPrint)
    pw.close()
    println(s"Saved ${binds.length} devices.")

  }

  def getParam(prop: ScallopOption[String]): String = prop.getOrElse("") match {
    case "power" => "Pow"
    case "mode" => "Mod"
    case "temp" => "SetTem"
    case "temp-unit" => "TemUn"
    case "fan" => "WdSpd"
    case "turbo" => "Tur"
    case "light" => "Lig"
    case "air" => "Air"
    case "health" => "Health"
    case "test" => "RoomWid"
  }

  def getValue(value: ScallopOption[String]): Int = value.getOrElse("") match {
    case "on" => 1
    case "off" => 0
    case "auto" => 0
    case "cool" => 1
    case "dry" => 2
    case "fan" => 3
    case "heat" => 4
    case "celsius" => 0
    case "fahrenheit" => 1
    case "low" => 1
    case "medium" => 3
    case "high" => 5
  }

  def ac(config: JsObject, prop: ScallopOption[String], value: ScallopOption[String]): Unit = {
    val device = config.fields("devices").convertTo[Array[Map[String, String]]].filter(p => p("default") == "true")(0)
    var setMode = false
    if (value.isDefined) setMode = true
    val param = getParam(prop)
    var pack: String = ""
    var numValue: Int = -1
    if (setMode) {
      if (param == "SetTem") {
        try {
          numValue = value.getOrElse("0").toInt
        } catch {
          case _: NumberFormatException =>
            printErr("Enter a number for temperature.")
        }
      } else {
        numValue = getValue(value)
      }
      pack = s"""{"opt":["$param"],"p":[$numValue],"t":"cmd"}"""
    } else {
      pack = s"""{"cols":["$param"],"mac":"${device("id")}","t":"status"}"""
    }
    val pack_encrypted = encrypt(pack, device("key"))

    val request = s"""{"cid":"app","i":0,"pack":"$pack_encrypted","t":"pack","tcid":"${device("id")}","uid":0}"""
    val recv = sendData(device("ip"), 7000, request.getBytes)
    val jsonRecv = JsonParser(new String(recv.getData, 0, recv.getLength))
    val mapRecv = jsonRecv.convertTo[Map[String, JsValue]]
    if (mapRecv("t").toString == """"pack"""") {
      val decrypted = decrypt(mapRecv("pack").toString().stripQ, device("key"))
      val jsonDecrypted = JsonParser(new String(decrypted, 0, decrypted.length))
      val mapDecrypted = jsonDecrypted.convertTo[Map[String, JsValue]]
      println(mapDecrypted)
    }
  }

  def config(currentConfig: JsObject, entryOpt: ScallopOption[String], value: ScallopOption[String]): Unit = {
    val allowedEntries: Array[String] = Array("broadcastAddress")
    var setMode: Boolean = false
    val entry = entryOpt.getOrElse("")
    if (!value.getOrElse("").isEmpty) setMode = true
    if (entry == "") {
      val configMap = currentConfig.convertTo[Map[String, JsValue]]
      println(configMap.map {
        case (key, value: JsString) => s"$key -> ${value.prettyPrint.stripQ}"
        case (_, _: JsArray) => ""
      }.mkString("\n"))
      System.exit(0)
    }
    if (!allowedEntries.contains(entry)) {
      printErr(s"$entry is not a valid config entry.")
    }

    if (setMode) {
      val pw = new PrintWriter(new File("config.json"))
      val newConfig = JsObject(currentConfig.fields + (entry -> value.getOrElse("").toJson))
      pw.write(newConfig.prettyPrint)
      pw.close()
      println()
    } else {
      println(currentConfig.fields(entry).prettyPrint.stripQ)
    }
  }

  def devices(currentConfig: JsObject, commandOpt: ScallopOption[String], valueOpt: ScallopOption[String]): Unit = {
    val devices = currentConfig.fields("devices").convertTo[Array[Map[String, String]]]
    val command = commandOpt.getOrElse("")
    val value = valueOpt.getOrElse("")
    if (command == "list") {
      devices.foreach(d => {
        println(s"ID: ${d("incId")}\n------------------")
        println(d filter (e => e._1 != "incId") map {
          case (key, value) => s"$key -> $value"
        } mkString "\n")
        println()
      })
    }
    if (command == "default") {
      if (value == "") printErr("Specify a device ID.")
      val selected = devices filter (e => e("incId") == value)
      if (selected.length == 0) printErr("Device not found.")

      val pw = new PrintWriter(new File("config.json"))
      val newConfig = JsObject(currentConfig.fields + ("devices" -> devices.map(
        d => {
          if (d("incId") == value) {
            d + ("default" -> "true")
          } else {
            d + ("default" -> "false")
          }
        }
      ).toJson))
      pw.write(newConfig.prettyPrint)
      pw.close()
      println(s"Set device with ID $value to default.")
    }
  }

  // Main

  val testArgs: Array[String] = Array("ac", "power", "off")

  def main(args: Array[String]): Unit = {
    val conf = new Conf(testArgs)
    var configObject: JsObject = null
    try {
      val src = Source.fromFile("config.json")
      configObject = JsonParser(src.getLines.mkString).asJsObject
      src.close()
    } catch {
      case _: FileNotFoundException =>
        val pw = new PrintWriter(new File("config.json"))
        pw.write("{}")
        pw.close()
    }

    if (conf.subcommand.contains(conf.search)) search(configObject)

    if (conf.subcommand.contains(conf.ac)) ac(configObject, conf.ac.prop, conf.ac.value)

    if (conf.subcommand.contains(conf.config)) config(configObject, conf.config.entry, conf.config.value)

    if (conf.subcommand.contains(conf.devices)) devices(configObject, conf.devices.command, conf.devices.value)
  }
}
