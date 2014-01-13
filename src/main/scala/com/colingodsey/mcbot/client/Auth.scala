package com.colingodsey.mcbot.client

import java.security._
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.crypto.{KeyGenerationParameters, CipherKeyGenerator}
import javax.crypto.spec.SecretKeySpec
import javax.crypto.{SecretKey, Cipher}
import com.colingodsey.mcbot.protocol.login
import spray.json.{JsString, JsObject}
import java.security.spec.X509EncodedKeySpec
import spray.http.HttpRequest
import scala.concurrent.Future
import spray.client.pipelining._
import spray.http.HttpRequest
import spray.httpx.encoding.Deflate

object Auth {
	Security.addProvider(new BouncyCastleProvider)

	def createSecretKey = {
		val generator = new CipherKeyGenerator
		generator.init(new KeyGenerationParameters(new SecureRandom, 128))
		new SecretKeySpec(generator.generateKey, "AES")
	}

	def cipher(method: Int, key: Key, data: Array[Byte]) = {
		val cipher = Cipher.getInstance(key.getAlgorithm)
		cipher.init(method, key)
		cipher.doFinal(data)
	}

	def generateClientHash(username: String, secretKey: SecretKey,
			req: login.client.EncryptionRequest) = {
		val digest = {
			val md = MessageDigest.getInstance("SHA-1")
			md update req.serverID.getBytes("UTF8")
			md update secretKey.getEncoded
			md update req.publicKey.toArray
			md.digest
		}
		val digestString = BigInt(digest).toString(16)

		//log.info("Server ID: " + req.serverID)
		//log.info("Server key - " + new String(req.publicKey.toArray, "ASCII"))
		//log.info("Digest string: " + digestString)

		val json = JsObject(Map(
			"accessToken" -> JsString(digestString),
			"selectedProfile" -> JsString(username /*"<selectedProfile>"*/),
			"serverId" -> JsString(req.serverID)
		))

		val spec = new X509EncodedKeySpec(req.publicKey.toArray)
		val factory = KeyFactory.getInstance("RSA")
		val pubKey = factory.generatePublic(spec)

		//val skeySpec = new SecretKeySpec(req.publicKey.toArray, "AES")
		val aesCipher = Cipher.getInstance("AES/CFB8/PKCS5Padding", "BC") //PKCS1Padding
		val rsaCipher = Cipher.getInstance("RSA", "BC")

		//val pubKey = converter.getPublicKey(kp)

		aesCipher.init(Cipher.ENCRYPT_MODE, secretKey)
		rsaCipher.init(Cipher.ENCRYPT_MODE, pubKey)

		val verifyToken = cipher(Cipher.ENCRYPT_MODE, pubKey, req.verifyToken.toArray)
		val encryptedSecret = cipher(Cipher.ENCRYPT_MODE, pubKey, secretKey.getEncoded)
		//val verifyToken = rsaCipher.doFinal(req.verifyToken.toArray)

		//log.info("Session request json " + json)

		/*val pipeline: HttpRequest => Future[String] =
			sendReceive ~> decode(Deflate) ~> unmarshal[String]

		pipeline(Post(sessionUrl, json)) onComplete {
			case Success(x) => println(x)
			case Failure(t) =>
				log.error(t, "Session request failed")
		}*/
	}
}
