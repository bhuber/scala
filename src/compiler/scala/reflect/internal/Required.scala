package scala.reflect
package internal

import settings.MutableSettings

trait Required { self: SymbolTable =>

  type AbstractFileType >: Null <: {
    def path: String
  }

  def isSameFile(f1: AbstractFileType, f2: AbstractFileType): Boolean

  def picklerPhase: Phase

  val gen: TreeGen { val global: Required.this.type }

  def settings: MutableSettings

  def forInteractive: Boolean

  def forScaladoc: Boolean
}
