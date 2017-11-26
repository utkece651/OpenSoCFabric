package OpenSoC

import Chisel._

/*
abstract class NetworkInterfaceParams {
    val NumOutChanPerEnd = UInt()
    val NumInChanPerEnd = UInt()
}
*/

abstract class NetworkInterface(parms: Parameters) extends Module(parms) {
    val io = new Bundle {
        val AXI = new AXI4Lite32()
        val out = new Channel(parms).flip()
        val in = new Channel(parms)
    }
}

abstract class InputNetworkInterface[T <: Data](parms : Parameters, tGen : Parameters => T) extends Module(parms) {
    val io = new Bundle {
        val in = new DecoupledIO[T](tGen(parms)).flip()
        val out = new Channel(parms).flip()
    }
}

class InputPacketInterface[T <: Data](parms: Parameters, tGen : Parameters => T) extends InputNetworkInterface[T](parms, tGen) {

    val flitizer = parms.get[Parameters=>InputToFlit[T]]("InputFlitizer")
    val queueDepth = parms.get[Int]("queueDepth")

    val creditCon = Chisel.Module ( new CreditCon( parms.child("MyCon", Map(
        ("numCreds"->Soft(queueDepth))))) )
    val packet2Flit = Chisel.Module( flitizer(parms) )

    io.in.ready                     := packet2Flit.io.packetReady
    packet2Flit.io.packet           := io.in.bits
    packet2Flit.io.packetValid      := io.in.valid

    creditCon.io.inCredit <> io.out.credit
    io.out.flit := packet2Flit.io.flit
    creditCon.io.inConsume := packet2Flit.io.flitValid
    io.out.flitValid := packet2Flit.io.flitValid
    packet2Flit.io.flitReady := creditCon.io.outCredit
}

class AXINetworkInterface(parms: Parameters) extends NetworkInterface(parms) {

    val maxPacketLength = parms.get[Int]("packetMaxLength")
    val packetWidth     = parms.get[Int]("packetWidth")
    val packetIDWidth   = parms.get[Int]("packetIDWidth")
    val payloadWidth    = parms.get[Int]("payloadWidth")
    val flitIDWidth     = parms.get[Int]("flitIDWidth")
    val packetTypeWidth = parms.get[Int]("packetTypeWidth")
    val destCordWidth   = parms.get[Int]("destCordWidth")
    val numVCs          = parms.get[Int]("numVCs")
    val Dim             = parms.get[Int]("TopologyDimension")
    val C               = parms.get[Int]("Concentration") // Processors (endpoints) per router.

    val queueDepth = parms.get[Int]("queueDepth")
    val creditCon = Chisel.Module ( new CreditCon( parms.child("MyCon", Map(
        ("numCreds"->Soft(queueDepth))))) )

    val flitWidth = Flit.fromBits(UInt(0), parms).getWidth()

    // Converters from Head/Body to Flit
    val headBundle2Flit = Chisel.Module( new HeadBundle2Flit(parms) )
    val bodyBundle2Flit = Chisel.Module( new BodyBundle2Flit(parms) )

    // Create States for the State Machine
    val sw_idle :: sw_createHeadFlit :: sw_waitForData :: sw_createBodyFlit :: sw_sendResponse :: Nil = Enum(UInt(), 5)
    val sw_state = Reg(init = sw_idle)

    // Output flit
    val flitOut = Decoupled(Flit.fromBits(UInt(0), parms))

    creditCon.io.inCredit <> io.out.credit
    io.out.flit := flitOut.bits
    creditCon.io.inConsume := flitOut.valid
    io.out.flitValid := flitOut.valid
    flitOut.ready := creditCon.io.outCredit

    // Flit width must be equal to axi bus width.
    // assert(flitWidth == 32)

    when( sw_state === sw_idle ) {
        when (io.AXI.AWVALID) { sw_state := sw_createHeadFlit }
        .otherwise            { sw_state := sw_idle }
        io.AXI.AWREADY            := Bool(false)
        io.AXI.WREADY             := Bool(false)
        io.AXI.BRESP              := UInt(0)
        io.AXI.BVALID             := Bool(false)
        io.AXI.ARREADY            := Bool(false)
        io.AXI.RVALID             := Bool(false)
        io.AXI.RDATA              := UInt(0, 32)
        io.AXI.RRESP              := Bool(false)
        flitOut.valid             := Bool(false)
        flitOut.bits              := new Flit(parms).fromBits(UInt(0))
        headBundle2Flit.io.inHead := new HeadFlit(parms).fromBits(UInt(0))
        bodyBundle2Flit.io.inBody := new BodyFlit(parms).fromBits(UInt(0))

    }.elsewhen( sw_state === sw_createHeadFlit ) {
        when (flitOut.ready) { sw_state := sw_waitForData
                               io.AXI.AWREADY := Bool(true)  }
        .otherwise           { sw_state := sw_createHeadFlit
                               io.AXI.AWREADY := Bool(false) }
        flitOut.bits := CreateHeadFlit(io.AXI.AWADDR, UInt(0), parms)
        flitOut.valid := Bool(true)
        bodyBundle2Flit.io.inBody   := new BodyFlit(parms).fromBits(UInt(0))
        io.AXI.WREADY               := Bool(false)
        io.AXI.BRESP                := UInt(0)
        io.AXI.BVALID               := Bool(false)
        io.AXI.ARREADY              := Bool(false)
        io.AXI.RVALID               := Bool(false)
        io.AXI.RDATA                := UInt(0, 32)
        io.AXI.RRESP                := Bool(false)

    }.elsewhen( sw_state === sw_waitForData ) {
        when (io.AXI.WVALID)  { sw_state := sw_createBodyFlit }
        .otherwise            { sw_state := sw_waitForData }
        io.AXI.AWREADY            := Bool(false)
        io.AXI.WREADY             := Bool(false)
        io.AXI.BRESP              := UInt(0)
        io.AXI.BVALID             := Bool(false)
        io.AXI.ARREADY            := Bool(false)
        io.AXI.RVALID             := Bool(false)
        io.AXI.RDATA              := UInt(0, 32)
        io.AXI.RRESP              := Bool(false)
        flitOut.valid             := Bool(false)
        flitOut.bits              := new Flit(parms).fromBits(UInt(0))
        headBundle2Flit.io.inHead := new HeadFlit(parms).fromBits(UInt(0))
        bodyBundle2Flit.io.inBody := new BodyFlit(parms).fromBits(UInt(0))

    }.elsewhen( sw_state === sw_createBodyFlit ) {
        when (flitOut.ready) { sw_state := sw_sendResponse
                               io.AXI.WREADY := Bool(true)  }
        .otherwise           { sw_state := sw_createBodyFlit
                               io.AXI.WREADY := Bool(false)  }
        flitOut.bits := CreateBodyFlit(io.AXI.WDATA, UInt(0), UInt(0), Bool(true), parms)
        flitOut.valid := Bool(true)
        headBundle2Flit.io.inHead   := new HeadFlit(parms).fromBits(UInt(0))
        io.AXI.AWREADY              := Bool(false)
        io.AXI.BRESP                := UInt(0)
        io.AXI.BVALID               := Bool(false)
        io.AXI.ARREADY              := Bool(false)
        io.AXI.RVALID               := Bool(false)
        io.AXI.RDATA                := UInt(0, 32)
        io.AXI.RRESP                := Bool(false)

    }.elsewhen( sw_state === sw_sendResponse ) {
        when (io.AXI.BREADY) { sw_state := sw_idle         }
        .otherwise           { sw_state := sw_sendResponse }
        io.AXI.BVALID             := Bool(true)
        io.AXI.BRESP              := AXI4Parameters.RESP_OKAY

        io.AXI.AWREADY            := Bool(false)
        io.AXI.WREADY             := Bool(false)
        io.AXI.ARREADY            := Bool(false)
        io.AXI.RVALID             := Bool(false)
        io.AXI.RDATA              := UInt(0, 32)
        io.AXI.RRESP              := Bool(false)
        flitOut.valid             := Bool(false)
        flitOut.bits              := new Flit(parms).fromBits(UInt(0))
        headBundle2Flit.io.inHead := new HeadFlit(parms).fromBits(UInt(0))
        bodyBundle2Flit.io.inBody := new BodyFlit(parms).fromBits(UInt(0))

    }.otherwise{
        // Set default values
        io.AXI.AWREADY := Bool(false)
        io.AXI.WREADY  := Bool(false)
        io.AXI.BRESP   := Bool(false)
        io.AXI.BVALID  := Bool(false)
        io.AXI.ARREADY := Bool(false)
        io.AXI.RVALID  := Bool(false)
        io.AXI.RDATA   := UInt(0, 32)
        io.AXI.RRESP   := Bool(false)
        headBundle2Flit.io.inHead := new HeadFlit(parms).fromBits(UInt(0))
        bodyBundle2Flit.io.inBody := new BodyFlit(parms).fromBits(UInt(0))
        flitOut.valid             := Bool(false)
        flitOut.bits              := new Flit(parms).fromBits(UInt(0))
    }


    def CreateHeadFlit(destAddress : UInt, packetID : UInt, parms : Parameters) : Flit =  {
        val offset          = 16
        headBundle2Flit.io.inHead.isTail        := Bool(false)
        headBundle2Flit.io.inHead.packetID      := packetID
        headBundle2Flit.io.inHead.packetType    := UInt(0)
        for (d <- 0 until Dim){
            headBundle2Flit.io.inHead.destination(d)    := destAddress(offset+(d*8)+log2Up(destCordWidth), offset+(d*8))
        }
        headBundle2Flit.io.inHead.vcPort        := UInt(0)

        val flit = headBundle2Flit.io.outFlit
        flit
    }

    def CreateBodyFlit(payload : UInt, packetID : UInt, flitID : UInt, isTail : Bool, parms : Parameters) : Flit = {

        bodyBundle2Flit.io.inBody.isTail    := isTail
        bodyBundle2Flit.io.inBody.packetID  := packetID
        bodyBundle2Flit.io.inBody.vcPort    := UInt(0)
        bodyBundle2Flit.io.inBody.flitID    := flitID
        bodyBundle2Flit.io.inBody.payload   := payload

        val flit = bodyBundle2Flit.io.outFlit
        flit
    }
}

class AXINetworkInterfaceTest(c: AXINetworkInterface) extends Tester(c) {
    poke (c.io.AXI.AWVALID, 0)
    poke (c.io.AXI.AWADDR, 0)
    poke (c.io.AXI.AWPROT, 0)
    poke (c.io.AXI.WVALID, 0)
    poke (c.io.AXI.WDATA, 0)
    poke (c.io.AXI.WSTRB, 0)
    poke (c.io.AXI.BREADY, 0)
    poke (c.io.AXI.ARVALID, 0)
    poke (c.io.AXI.ARADDR, 0)
    poke (c.io.AXI.ARPROT, 0)
    poke (c.io.AXI.RREADY, 0)
    poke (c.io.out.credit.grant, 0)
    poke (c.io.in.flit.x, 0)
    poke (c.io.in.flitValid, 0)
    reset(1)
    step(1) // 1

    // Write Data
    poke (c.io.out.credit.grant, 1)
    poke (c.io.AXI.WDATA, 10)
    poke (c.io.AXI.WVALID, 1)
    step(1) // 2
    poke (c.io.out.credit.grant, 0)
    expect(c.io.AXI.WREADY, 0)
    poke (c.io.AXI.AWADDR, 1)
    poke (c.io.AXI.AWVALID, 1)
    step(1) // 3
    expect(c.io.AXI.AWREADY, 1)
    expect(c.io.AXI.WREADY, 0)
    step(1)
    poke (c.io.AXI.AWVALID, 0)
    step(1) // 4
    expect(c.io.AXI.WREADY, 1)
    step(1) // 5
    poke (c.io.AXI.WVALID, 0)
    poke (c.io.AXI.BREADY, 1)
    expect(c.io.AXI.BVALID, 1)
    step(1) // 6
    expect(c.io.AXI.BRESP, 0)
    step(10)

    // Write Address

    // Expect Output

}
/* vim: set shiftwidth=4 tabstop=4 softtabstop=4 expandtab: */
