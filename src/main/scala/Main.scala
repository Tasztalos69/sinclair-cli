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
import scala.language.{implicitConversions, reflectiveCalls}
import scala.util.control.Breaks._
import scala.util.matching.Regex

// CLI Syntax constructor

class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  version(s"${RED}Sinclair CLI v1.0.0 $RESET\n${GREEN}MIT $RESET|$YELLOW Kiss Benedek Máté$RESET")
  banner(s"\n$BOLD${WHITE}Usage:$RESET$CYAN scli <command> [<args>]$RESET")
  footer(s"Use$CYAN scli <command> --help$RESET for additional help.")

  helpFormatter = new ScallopHelpFormatter {
    override def formatHelp(s: Scallop, subcommandPrefix: String): String =
      s"""
         |$BOLD${WHITE}Commands:$RESET
         |   command   short    args
         |   ${YELLOW}search    ${GREEN}s        $CYAN[-b]
         |   ${YELLOW}ac                 $CYAN[<property>] [<value>] [-d]
         |   ${YELLOW}devices   ${GREEN}d        $CYAN<command> [<value>]
         |   ${YELLOW}config    ${GREEN}c        $CYAN[<entry>] [<value>]$RESET
         |""".stripMargin

  }

  val search = new Subcommand("search", "s") {
    val broadcastAddress = opt[String]("broadcast-address", 'b', required = false)
    helpFormatter = new ScallopHelpFormatter {
      override def formatHelp(s: Scallop, subcommandPrefix: String): String =
        s"""$BOLD${WHITE}Command:$RESET$CYAN search
           |
           |$BOLD${WHITE}Description:$RESET Search for ACs on the network.
           |
           |$BOLD${WHITE}Arguments:$RESET
           |  $YELLOW-b --broadcast-address <ip>$RESET
           |  Specify a broadcast address.
           |  You can set a default broadcast address by typing$CYAN scli config broadcastAddress <ip>$RESET,
           |  or you can set it to default, and the program will try to obtain the broadcast address.
           |""".stripMargin
    }
  }
  val ac = new Subcommand("ac") {
    val prop = trailArg[String]("property", required = false)
    val value = trailArg[String]("value", required = false)
    val device = opt[String]("device", required = false)
    helpFormatter = new ScallopHelpFormatter {
      override def formatHelp(s: Scallop, subcommandPrefix: String): String =
        s"""$BOLD${WHITE}Command:$RESET$CYAN ac
           |
           |$BOLD${WHITE}Description:$RESET Set properties of the Air Conditioner.
           |If no property supplied, it prints the status of the AC.
           |
           |$BOLD${WHITE}Arguments:$RESET
           |  ${CYAN}property$RESET
           |  Get a specific property of the AC.
           |  If a value is supplied, set the property to the value.
           |
           |  ${CYAN}value$RESET
           |  Set a property to a value.
           |  Check available properties and values below.
           |
           |  $YELLOW-d --device <ID>$RESET
           |  Specify a device ID.
           |  You can see the available IDs by typing$CYAN scli devices list$RESET.
           |
           |$BOLD${WHITE}Properties:$RESET
           |  ${GREEN}power       ${YELLOW}on | off
           |  ${GREEN}mode        ${YELLOW}auto | cool | dry | fan | heat
           |  ${GREEN}temp        ${YELLOW}<degree>
           |  ${GREEN}temp-unit   ${YELLOW}celsius | fahrenheit
           |  ${GREEN}fan         ${YELLOW}auto | low | medium | height
           |  ${GREEN}turbo       ${YELLOW}on | off
           |  ${GREEN}light       ${YELLOW}on | off
           |  ${GREEN}air         ${YELLOW}on | off
           |  ${GREEN}health      ${YELLOW}on | off$RESET
           |""".stripMargin
    }
  }


  val config = new Subcommand("config", "c") {
    val entry = trailArg[String]("entry", required = false)
    val value = trailArg[String]("value", required = false)
    helpFormatter = new ScallopHelpFormatter {
      override def formatHelp(s: Scallop, subcommandPrefix: String): String =
        s"""$BOLD${WHITE}Command:$RESET$CYAN config
           |
           |$BOLD${WHITE}Description:$RESET Set config entries to values.
           |If no entry supplied, it prints the whole config.
           |
           |$BOLD${WHITE}Arguments:$RESET
           |  ${CYAN}entry$RESET
           |  Get a specific entry.
           |  If a value is supplied, set the entry to the value.
           |
           |  ${CYAN}value$RESET
           |  Set an entry to a value.
           |  Check available entries and values below.
           |
           |$BOLD${WHITE}Entries:$RESET
           |  ${GREEN}broadcastAddress       $YELLOW<ip> | default$RESET
           |""".stripMargin
    }
  }

  val devices = new Subcommand("devices", "d") {
    val command = trailArg[String]("command", required = true)
    val value = trailArg[String]("value", required = false)
    helpFormatter = new ScallopHelpFormatter {
      override def formatHelp(s: Scallop, subcommandPrefix: String): String =
        s"""$BOLD${WHITE}Command:$RESET$CYAN devices
           |
           |$BOLD${WHITE}Description:$RESET Manage previously fetched devices.
           |
           |$BOLD${WHITE}Arguments:$RESET
           |  ${CYAN}command$RESET (required)
           |  Execute a command. Check commands below.
           |
           |  ${CYAN}value$RESET
           |  Use together with ${CYAN}command$RESET.
           |
           |  $BOLD${WHITE}Commands:$RESET
           |  ${GREEN}list
           |  ${GREEN}default   $YELLOW<ID>$RESET
           |""".stripMargin
    }
  }
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
        println(s"${GREEN}Bind successful.$RESET")
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

  def search(configIn: JsObject, ba: ScallopOption[String]): Unit = {
    var config = configIn
    if (config.fields.contains("devices")) {
      print(s"${YELLOW}Warning:$RESET there are previously bound devices. Do you want to overwrite them? (y/N) ")
      val resp = readLine()
      if (resp.toUpperCase != "Y" && resp.toUpperCase != "YES") {
        println("Aborting.")
        System.exit(0)
      }
    }
    var bcAddr = ""
    if (ba.getOrElse("") != "") {
      val bAddr = ba.getOrElse("")
      val patt = new Regex("\\b(?:(?:2(?:[0-4][0-9]|5[0-5])|[0-1]?[0-9]?[0-9])\\.){3}(?:(?:2([0-4][0-9]|5[0-5])|[0-1]?[0-9]?[0-9]))\\b", "gi")
      if (patt.matches(bAddr)) bcAddr = bAddr else printErr("Invalid IP entered.")
    } else if (!config.fields.contains("broadcastAddress") || config.fields("broadcastAddress").prettyPrint.stripQ == "default") {
      println(s"Searching for default broadcast address...\n(Alternatively, you can set the broadcast address with ${CYAN}scli config broadcast <ip>$RESET)\n")
      val bc = getDefaultBroadcast.getOrElse("")
      if (bc.isEmpty) {
        printErr("cannot get default broadcast address.")
        System.exit(1)
      } else {
        println(s"Found broadcast address $bc, and setting it as default.\n")
        val pw = new PrintWriter(new File("config.json"))
        config = JsObject(config.fields + ("broadcastAddress" -> bc.toJson))
        pw.write(config.prettyPrint)
        pw.close()
        bcAddr = bc
      }
    }
    println("Searching...")
    val msg = """{"t":"scan"}""".getBytes()
    val addr = new InetSocketAddress(if (config.fields.contains("broadcastAddress")) config.fields("broadcastAddress").toString.stripQ else bcAddr, 7000)
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
      println(s"${YELLOW}Warning:$RESET Found multiple devices. To use the shorthand command (without specifying a device), use:$CYAN scli config default <id>$RESET")
    }
    val pw = new PrintWriter(new File("config.json"))
    config = JsObject(config.fields + ("devices" -> binds.toJson))
    pw.write(config.prettyPrint)
    pw.close()
    println(s"${GREEN}Saved ${binds.length} devices.$RESET")

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
  }

  def getValue(propOpt: ScallopOption[String], valueOpt: ScallopOption[String]): Int = {
    val prop = propOpt.getOrElse("")
    val value = valueOpt.getOrElse("")
    prop match {
      case "power" | "light" | "air" | "health" | "turbo" => value match {
        case "on" => 1
        case "off" => 0
      }
      case "mode" => value match {
        case "auto" => 0
        case "cool" => 1
        case "dry" => 2
        case "fan" => 3
        case "heat" => 4
      }
      case "temp-unit" => value match {
        case "celsius" => 0
        case "fahrenheit" => 1
      }
      case "fan" => value match {
        case "auto" => 0
        case "low" => 1
        case "medium" => 3
        case "high" => 5
      }
    }
  }

  def getPrintableValue(prop: String, value: Int): String = {
    prop match {
      case "Pow" | "Lig" | "Air" | "Health" | "Tur" => value match {
        case 1 => "on"
        case 0 => "off"
      }
      case "Mod" => value match {
        case 0 => "auto"
        case 1 => "cool"
        case 2 => "dry"
        case 3 => "fan"
        case 4 => "heat"
      }
      case "TemUn" => value match {
        case 0 => "°C"
        case 1 => "°F"
      }
      case "WdSpd" => value match {
        case 0 => "auto"
        case 1 => "low"
        case 3 => "medium"
        case 5 => "high"
      }
      case "SetTem" => value.toString
    }
  }

  def ac(config: JsObject, prop: ScallopOption[String], value: ScallopOption[String], deviceFlag: ScallopOption[String]): Unit = {
    var device: Map[String, String] = Map()
    if (deviceFlag.getOrElse("") != "") {
      try {
        device = config.fields("devices").convertTo[Array[Map[String, String]]].filter(p => p("incId") == deviceFlag.getOrElse(""))(0)
      } catch {
        case _: IndexOutOfBoundsException => printErr(s"Device not found. Run$CYAN scli devices list$RESET for available devices.")
      }
    } else {
      device = config.fields("devices").convertTo[Array[Map[String, String]]].filter(p => p("default") == "true")(0)
    }
    var setMode = false
    if (!prop.isDefined) {
      val pack = s"""{"cols":["Pow","SetTem","Mod","WdSpd","TemUn", "Air","Tur","Health","Lig"], "mac":"${device("id")}","t":"status"}"""
      val pack_encrypted = encrypt(pack, device("key"))
      val request = s"""{"cid":"app","i":0,"pack":"$pack_encrypted","t":"pack","tcid":"${device("id")}","uid":0}"""
      val recv = sendData(device("ip"), 7000, request.getBytes)
      val jsonRecv = JsonParser(new String(recv.getData, 0, recv.getLength))
      val mapRecv = jsonRecv.convertTo[Map[String, JsValue]]
      if (mapRecv("t").toString == """"pack"""") {
        val decrypted = decrypt(mapRecv("pack").toString().stripQ, device("key"))
        val jsonDecrypted = JsonParser(new String(decrypted, 0, decrypted.length))
        val mapDecrypted = jsonDecrypted.convertTo[Map[String, JsValue]]
        val dat = mapDecrypted("dat").convertTo[Array[Int]]
        val cols = mapDecrypted("cols").convertTo[Array[String]]
        val out =
          s"""${WHITE}Name: $YELLOW${device("id")}$RESET
             |---------------------
             |${YELLOW}Power:  $GREEN${getPrintableValue(cols(0), dat(0))}$RESET
             |${YELLOW}Mode:   $GREEN${getPrintableValue(cols(2), dat(2))}$RESET
             |${YELLOW}Temp:   $GREEN${getPrintableValue(cols(1), dat(1))} ${getPrintableValue(cols(4), dat(4))}$RESET
             |${YELLOW}Fan:    $GREEN${getPrintableValue(cols(3), dat(3))}$RESET
             |${YELLOW}Turbo:  $GREEN${getPrintableValue(cols(6), dat(6))}$RESET
             |${YELLOW}Air:    $GREEN${getPrintableValue(cols(5), dat(5))}$RESET
             |${YELLOW}Health: $GREEN${getPrintableValue(cols(7), dat(7))}$RESET
             |${YELLOW}Light:  $GREEN${getPrintableValue(cols(8), dat(8))}$RESET
             |""".stripMargin
        println(out)
      }
      System.exit(0)
    }
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
        numValue = getValue(prop, value)
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
      println(mapDecrypted) // TODO print output & error handling
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
        case (k, v) => s"$YELLOW$k$RESET -> $GREEN$v$RESET"
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
        println(s"${GREEN}ID: ${d("incId")}$RESET\n------------------")
        println(d filter (e => e._1 != "incId") map {
          case (key, value) => s"$YELLOW$key -> $GREEN$value$RESET"
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

  def main(args: Array[String]): Unit = {
    val conf = new Conf(args.toIndexedSeq)
    var configObject: JsObject = null
    try {
      val src = Source.fromFile("config.json")
      configObject = JsonParser(src.getLines().mkString).asJsObject
      src.close()
    } catch {
      case _: FileNotFoundException =>
        val pw = new PrintWriter(new File("config.json"))
        pw.write("{}")
        pw.close()
        configObject = new JsObject(Map())
    }

    if (conf.subcommand.contains(conf.search)) search(configObject, conf.search.broadcastAddress)

    if (conf.subcommand.contains(conf.ac)) ac(configObject, conf.ac.prop, conf.ac.value, conf.ac.device)

    if (conf.subcommand.contains(conf.config)) config(configObject, conf.config.entry, conf.config.value)

    if (conf.subcommand.contains(conf.devices)) devices(configObject, conf.devices.command, conf.devices.value)
    if (conf.subcommands.isEmpty) conf.printHelp()

  }
}
