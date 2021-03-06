package OpenSoC

import Chisel._

object AXI4
{
  // These are all fixed by the AXI4 standard:
  val lenBits   = 8
  val sizeBits  = 3
  val burstBits = 2
  val lockBits  = 1
  val cacheBits = 4
  val protBits  = 3
  val qosBits   = 4
  val respBits  = 2

  def CACHE_RALLOCATE  = UInt(8, width = cacheBits)
  def CACHE_WALLOCATE  = UInt(4, width = cacheBits)
  def CACHE_MODIFIABLE = UInt(2, width = cacheBits)
  def CACHE_BUFFERABLE = UInt(1, width = cacheBits)

  def PROT_PRIVILEDGED = UInt(1, width = protBits)
  def PROT_INSECURE    = UInt(2, width = protBits)
  def PROT_INSTRUCTION = UInt(4, width = protBits)

  def BURST_FIXED = UInt(0, width = burstBits)
  def BURST_INCR  = UInt(1, width = burstBits)
  def BURST_WRAP  = UInt(2, width = burstBits)

  def RESP_OKAY   = UInt(0, width = respBits)
  def RESP_EXOKAY = UInt(1, width = respBits)
  def RESP_SLVERR = UInt(2, width = respBits)
  def RESP_DECERR = UInt(3, width = respBits)
}

class AXI4(parms: Parameters) extends Bundle {
    // Parameters
    val packetIDWidth   = parms.get[Int]("packetIDWidth")
    val dataWidth = parms.get[Int]("AXIDataWidth")

    // AXI Write Address Channel Signals
    val AWVALID = Bool(INPUT)
    val AWREADY = Bool(OUTPUT)
    val AWID    = UInt(INPUT, packetIDWidth)
    val AWADDR  = UInt(INPUT, 32)
    val AWLEN   = UInt(INPUT, 8)
    val AWSIZE  = UInt(INPUT, 3)
    val AWBURST = UInt(INPUT, 2)

    // AXI Write Data Channel Signals
    val WVALID  = Bool(INPUT)
    val WREADY  = Bool(OUTPUT)
    val WDATA   = UInt(INPUT, dataWidth)
    val WSTRB   = Bool(INPUT)

    // AXI Write Response Channel Signals
    val BVALID  = Bool(OUTPUT)
    val BREADY  = Bool(INPUT)
    val BRESP   = UInt(OUTPUT, 2)
    val BID     = UInt(OUTPUT, packetIDWidth)

    // AXI Read Address Channel Signals
    val ARVALID = Bool(INPUT)
    val ARREADY = Bool(OUTPUT)
    val ARID    = UInt(INPUT, packetIDWidth)
    val ARADDR  = UInt(INPUT, 32)
    val ARLEN   = UInt(INPUT, 8)
    val ARSIZE  = UInt(INPUT, 3)
    val ARBURST = UInt(INPUT, 2)

    // AXI Read Data Channel Signals
    val RVALID  = Bool(OUTPUT)
    val RREADY  = Bool(INPUT)
    val RID     = UInt(OUTPUT, packetIDWidth)
    val RDATA   = UInt(OUTPUT, dataWidth)
    val RRESP   = UInt(OUTPUT, 2)
}

/*

abstract class AXI extends Bundle {
    // AXI Global Signals
    val ACLK =      Bool(INPUT)
    val ARESETn =   Bool(INPUT)

    // AXI Write Address Channel Signals
    val AWID    i
    val AWADDR  i
    val AWLEN   i
    val AWSIZE  i
    val AWBURST i
    val AWLOCK  i
    val AWCACHE i
    val AWPROT =    UInt(INPUT, 3)
    val AWCACHE =   UInt(INPUT, 4)
    val AWQOS   i
    val AWREGION    i
    val AWUSER  i
    val AWVALID i
    val AWREADY o

    // AXI Write Data Channel Signals
    val WID     i
    val WDATA   i
    val WSTRB   i
    val WLAST   i
    val WUSER   i
    val WVALID  i
    val WREADY  o

    // AXI Write Response Channel Signals
    val BID o
    val BRESP   o
    val BUSER   o
    val BVALID  o
    val BREADY  i

    // AXI Read Address Channel Signals
    val ARID    i
    val ARADDR  i
    val ARLEN   i
    val ARSIZE  i
    val ARBURST i
    val ARLOCK  i
    val ARCACHE =   UInt(INPUT, 4)
    val ARPROT =    UInt(INPUT, 3)
    val ARQOS   i
    val ARREGION    i
    val ARUSER  i
    val ARVALID i
    val ARREADY o

    // AXI Read Data Channel Signals
    val RID o
    val RDATA   o
    val RRESP   o
    val RLAST   o
    val RUSER   o
    val RVALID  o
    val RREADY  i

    // AXI Low-Power Interface Signals
    val CSYSREQ i
    val CSYSACK o
    val CACTIVE o
}
*/
/* vim: set shiftwidth=4 tabstop=4 softtabstop=4 expandtab: */
