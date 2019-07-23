package lambdanet.architecture

import botkop.numsca
import botkop.numsca.Tensor
import funcdiff.{CompNode, LayerFactory, SymbolPath, TensorExtension}

trait ArchitectureHelper {
  def dimEmbedding: Int
  def layerFactory: LayerFactory

  private val normalizeFactor = 0.1 / math.sqrt(dimEmbedding)
  def randomVec(): Tensor = {
    numsca.randn(1, dimEmbedding) * normalizeFactor
  }

  def randomVar(name: SymbolPath): CompNode = {
    layerFactory.getVar(name)(randomVec())
  }

  def randomUnitVec(): Tensor = {
    TensorExtension.randomUnitVec(dimEmbedding).reshape(1, dimEmbedding)
  }

  def randomUnitVar(name: SymbolPath): CompNode = {
    layerFactory.getVar(name)(randomUnitVec())
  }

  def zeroVec() = numsca.zeros(1, dimEmbedding)
}
