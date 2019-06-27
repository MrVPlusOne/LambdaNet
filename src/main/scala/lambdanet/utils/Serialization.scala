// add to sbt:  "io.suzaku" %% "boopickle" % "1.3.1",

//package lambdanet.utils
//
//import java.io.{File, FileInputStream, FileOutputStream, ObjectOutputStream, Serializable}
//import java.nio.ByteBuffer
//
//import ammonite.ops
//import ammonite.ops.{Path, RelPath}
//import boopickle.PicklerHelper
//import lambdanet.{Annot, LibDefs, ProjectPath}
//import lambdanet.translation.ImportsResolution.NameDef
//import lambdanet.translation.PAnnot
//import lambdanet.translation.PredicateGraph.{PNode, PType}
//
//object Serialization {
//  def writeByteBuffer(path: File)(bytes: ByteBuffer): Unit = {
//    val oos = new FileOutputStream(path)
//    try {
//      oos.write(bytes.array())
//    } finally {
//      oos.close()
//    }
//  }
//
//  def readByteBuffer(path: File): ByteBuffer = {
//    val fIn = new FileInputStream(path)
//    val fChan = fIn.getChannel
//
//    try {
//      val fSize = fChan.size
//      val mBuf = ByteBuffer.allocate(fSize.toInt)
//      fChan.read(mBuf)
//      mBuf.rewind()
//      mBuf
//    } finally {
//      fChan.close()
//      fIn.close()
//    }
//  }
//
//  object Serializer{
//    type Repl = ByteBuffer
//  }
//
//  import Serializer.Repl
//  trait Serializer[T] {
//    def serialize(value: T): Repl
//
//    def deserialize(byteBuffer: Repl): T
//
//    def saveToFile(path: Path, value: T): Unit = {
//      val data = serialize(value)
//      writeByteBuffer(path.toIO)(data)
////      ops.write(path, data)
//    }
//
//    def readFromFile(path: Path): T = {
//      val data = readByteBuffer(path.toIO)
////      val data = ops.read(path)
//      deserialize(data)
//    }
//  }
//
//  import boopickle.Default._
//  import upickle.default._
//
//  implicit val symbolPickler: Pickler[Symbol] =
//    transformPickler(Symbol.apply)(_.name)
//  implicit val pNodePickler: Pickler[PNode] =
//    transformPickler(PNode.fromTuple)(PNode.toTuple)
//  implicit val pAnnotPickler: Pickler[PAnnot] =
//    transformPickler((x: Annot[PType]) => x: PAnnot)(x => x)
//  implicit val projPathPickler: Pickler[ProjectPath] =
//    transformPickler((x: RelPath) => x: ProjectPath)(x => x)
//
//  val LibDefs = new Serializer[LibDefs] {
//    def serialize(value: LibDefs): Repl = {
////      write(value)
//      Pickle.intoBytes(value)
//    }
//
//    def deserialize(data: Repl): LibDefs = {
//      Unpickle.apply[LibDefs].fromBytes(data)
////      read[LibDefs](data)
//    }
//  }
//
//}
