package retro.nes

/** An iNES image: PRG ROM, CHR ROM (or 8 KB of CHR RAM when it has none), mapper and nametable
  * mirroring. Mappers supported: 0 (NROM) and 3 (CNROM). Like most emulators, NROM gets 8 KB of
  * PRG RAM at 6000, which test ROMs use to report results. */
case class NesCartridge(prg: Array[Int], chr: Array[Int], chrRam: Boolean, mapper: Int, verticalMirroring: Boolean) {
  require(mapper == 0 || mapper == 3, s"mapper $mapper is not supported")
  def prgMask = prg.length - 1
  def chrMask = chr.length - 1
  /** Nametable address (2000-3FFF) to an index into the 2 KB of console VRAM. */
  def ciram(address: Int): Int = {
    val a = address & 0xFFF
    if (verticalMirroring) a & 0x7FF else ((a >> 1) & 0x400) | (a & 0x3FF)
  }
}

object NesCartridge {
  def load(image: Array[Int]): NesCartridge = {
    require(image.length >= 16 && image(0) == 'N' && image(1) == 'E' && image(2) == 'S' && image(3) == 0x1A, "not an iNES image")
    val prgSize = image(4) * 0x4000
    val chrSize = image(5) * 0x2000
    val trainer = if ((image(6) & 4) != 0) 512 else 0
    val prg = image.slice(16 + trainer, 16 + trainer + prgSize)
    val chr = if (chrSize == 0) Array.fill(0x2000)(0) else image.slice(16 + trainer + prgSize, 16 + trainer + prgSize + chrSize)
    NesCartridge(prg, chr, chrSize == 0, (image(6) >> 4) | (image(7) & 0xF0), (image(6) & 1) != 0)
  }
  def load(file: java.io.File): NesCartridge = load(java.nio.file.Files.readAllBytes(file.toPath).map(_ & 0xFF))
}
