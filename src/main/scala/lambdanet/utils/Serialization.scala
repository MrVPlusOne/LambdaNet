package lambdanet.utils

import java.io.{File, FileInputStream, FileOutputStream, ObjectOutputStream, Serializable}
import java.nio.ByteBuffer

object Serialization {
  def writeByteBuffer(path: File)(bytes: ByteBuffer): Unit = {
    val oos = new FileOutputStream(path)
    try {
      oos.write(bytes.array())
    } finally {
      oos.close()
    }
  }

  def readByteBuffer(path: File): ByteBuffer = {
    val fIn = new FileInputStream(path)
    val fChan = fIn.getChannel

    try {
      val fSize = fChan.size
      val mBuf = ByteBuffer.allocate(fSize.toInt)
      fChan.read(mBuf)
      mBuf.rewind()
      mBuf
    } finally {
      fChan.close()
      fIn.close()
    }
  }

}
