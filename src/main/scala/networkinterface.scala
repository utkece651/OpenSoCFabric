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
        val AXI = new AXI4(parms)
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

class AXI4NetworkInterface(parms: Parameters) extends NetworkInterface(parms) {

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

    val r_queue = Chisel.Module ( new Chisel.Queue( new Flit(parms), queueDepth) )

    val flitWidth = Flit.fromBits(UInt(0), parms).getWidth()

    // Converters from Head/Body to Flit
    val headBundle2Flit = Chisel.Module( new HeadBundle2Flit(parms) )
    val bodyBundle2Flit = Chisel.Module( new BodyBundle2Flit(parms) )

    // Create States for the State Machine
    val sw_idle :: sw_createHeadFlit :: sw_createBodyFlit :: sw_sendResponse :: Nil = Enum(UInt(), 4)
    val sw_state = Reg(init = sw_idle)

    // Output flit
    val flitOut = Decoupled(Flit.fromBits(UInt(0), parms))

    creditCon.io.inCredit <> io.out.credit
    io.out.flit := flitOut.bits
    creditCon.io.inConsume := flitOut.valid
    io.out.flitValid := flitOut.valid
    flitOut.ready := creditCon.io.outCredit

    // *********************** <default outputs> *******************************
    // Set default values
    io.AXI.AWREADY := Bool(false)
    io.AXI.WREADY  := Bool(false)
    io.AXI.BRESP   := UInt(0)
    io.AXI.BID     := UInt(0)
    io.AXI.BVALID  := Bool(false)
    io.AXI.ARREADY := Bool(false)
    io.AXI.RVALID  := Bool(false)
    io.AXI.RDATA   := UInt(0, 32)
    io.AXI.RRESP   := UInt(0)
    headBundle2Flit.io.inHead := new HeadFlit(parms).fromBits(UInt(0))
    bodyBundle2Flit.io.inBody := new BodyFlit(parms).fromBits(UInt(0))
    flitOut.valid             := Bool(false)
    flitOut.bits              := new Flit(parms).fromBits(UInt(0))
    r_queue.io.deq.ready := Bool(false)
    // *********************** </default outputs> ******************************

    // ************************* <flit reading> ********************************
    r_queue.io.enq.valid := io.in.flitValid
    r_queue.io.enq.bits := io.in.flit
    io.in.credit.grant := r_queue.io.deq.valid && r_queue.io.deq.ready

    val rlen = Reg(init=UInt(0, 8))
    val wlen = Reg(init=UInt(0, 8))

    val readValidReg = Reg(init=Bool(false))
    when ( !readValidReg ) { readValidReg := io.AXI.ARVALID }
    .otherwise {
        when(io.AXI.RREADY) {
            when(wlen === UInt(0)) {
                readValidReg := Bool(false)
            }
            .otherwise{
                wlen := wlen - UInt(1)
            }
        }
    }
    io.AXI.ARREADY := Bool(true)
    io.AXI.RVALID  := readValidReg
    io.AXI.RDATA   := UInt(0)
    val readAddrReg = Reg(init = UInt(0, 32))
    when (io.AXI.ARVALID) {
        readAddrReg := io.AXI.ARADDR
        when (io.AXI.ARADDR(0)){
            wlen := io.AXI.ARLEN
        }
        .otherwise {
            wlen := UInt(0)
        }
    }
    io.AXI.RDATA   := Mux(readAddrReg(0), r_queue.io.deq.bits.asBody().payload, Mux(r_queue.io.deq.valid && r_queue.io.deq.bits.isBody(), rlen, UInt(0)))
    io.AXI.RID   := Mux(readAddrReg(0), r_queue.io.deq.bits.asBody().packetID, Mux(r_queue.io.deq.valid && r_queue.io.deq.bits.isBody(), r_queue.io.deq.bits.asBody().packetID, UInt(0)))
    io.AXI.RRESP   := Mux((r_queue.io.deq.valid && r_queue.io.deq.bits.isBody()) || ~Bool(readAddrReg(0)), AXI4.RESP_OKAY, AXI4.RESP_SLVERR)
    when ((io.AXI.RVALID && r_queue.io.deq.valid && io.AXI.RREADY && readAddrReg(0)) || (r_queue.io.deq.valid && r_queue.io.deq.bits.isHead())) {
        when (r_queue.io.deq.valid && r_queue.io.deq.bits.isHead()) {
            rlen := r_queue.io.deq.bits.asHead().packetType
        }
        r_queue.io.deq.ready := Bool(true)
    }
    // ************************* </flit reading> *******************************

    // ************************** <write flit> *********************************
    val cur_data_pkt = Reg(init=UInt(0, 8))
    val data_len     = Reg(init=UInt(0, 8))
    val wid          = Reg(init=UInt(0, packetIDWidth))

    when( sw_state === sw_idle ) {
        when (io.AXI.AWVALID) { sw_state := sw_createHeadFlit }
        .otherwise            { sw_state := sw_idle }
        io.AXI.AWREADY            := Bool(false)
        io.AXI.WREADY             := Bool(false)
        io.AXI.BRESP              := UInt(0)
        io.AXI.BVALID             := Bool(false)
        flitOut.valid             := Bool(false)
        flitOut.bits              := new Flit(parms).fromBits(UInt(0))
        headBundle2Flit.io.inHead := new HeadFlit(parms).fromBits(UInt(0))
        bodyBundle2Flit.io.inBody := new BodyFlit(parms).fromBits(UInt(0))

    }.elsewhen( sw_state === sw_createHeadFlit ) {
        when (flitOut.ready) { sw_state := sw_createBodyFlit
                               io.AXI.AWREADY := Bool(true) }
        .otherwise           { sw_state := sw_createHeadFlit
                               io.AXI.AWREADY := Bool(false) }
        flitOut.bits := CreateHeadFlit(io.AXI.AWADDR, io.AXI.AWID, io.AXI.AWLEN + UInt(1), parms)
        flitOut.valid := Bool(true)
        bodyBundle2Flit.io.inBody   := new BodyFlit(parms).fromBits(UInt(0))
        io.AXI.WREADY               := Bool(false)
        io.AXI.BRESP                := UInt(0)
        io.AXI.BVALID               := Bool(false)
        data_len                    := io.AXI.AWLEN + UInt(1)
        cur_data_pkt                := UInt(0)
        wid                         := io.AXI.AWID

    }.elsewhen( sw_state === sw_createBodyFlit ) {
        when (io.AXI.WVALID) {
            when (flitOut.ready) {
                when (cur_data_pkt + UInt(1) === data_len) {
                    sw_state := sw_sendResponse
                }
                .otherwise {
                    sw_state := sw_createBodyFlit
                }
                io.AXI.WREADY := Bool(true)
                cur_data_pkt := cur_data_pkt + UInt(1)
            }
            .otherwise           { sw_state := sw_createBodyFlit
                                io.AXI.WREADY := Bool(false)  }
            flitOut.bits := CreateBodyFlit(io.AXI.WDATA, wid, wid, (cur_data_pkt + UInt(1) === data_len), parms)
            flitOut.valid := Bool(true)
            headBundle2Flit.io.inHead   := new HeadFlit(parms).fromBits(UInt(0))
            io.AXI.AWREADY              := Bool(false)
            io.AXI.BRESP                := UInt(0)
            io.AXI.BVALID               := Bool(false)
        }
        .otherwise {
            io.AXI.AWREADY            := Bool(false)
            io.AXI.WREADY             := Bool(false)
            io.AXI.BRESP              := UInt(0)
            io.AXI.BVALID             := Bool(false)
            flitOut.valid             := Bool(false)
            flitOut.bits              := new Flit(parms).fromBits(UInt(0))
            headBundle2Flit.io.inHead := new HeadFlit(parms).fromBits(UInt(0))
            bodyBundle2Flit.io.inBody := new BodyFlit(parms).fromBits(UInt(0))
        }

    }.elsewhen( sw_state === sw_sendResponse ) {
        when (io.AXI.BREADY) { sw_state := sw_idle         }
        .otherwise           { sw_state := sw_sendResponse }
        io.AXI.BVALID             := Bool(true)
        io.AXI.BRESP              := AXI4.RESP_OKAY
        io.AXI.BID                := wid

        io.AXI.AWREADY            := Bool(false)
        io.AXI.WREADY             := Bool(false)
        flitOut.valid             := Bool(false)
        flitOut.bits              := new Flit(parms).fromBits(UInt(0))
        headBundle2Flit.io.inHead := new HeadFlit(parms).fromBits(UInt(0))
        bodyBundle2Flit.io.inBody := new BodyFlit(parms).fromBits(UInt(0))

    }
    // ************************** </write flit> ********************************


    // *********************** <helper functions> ******************************
    def CreateHeadFlit(destAddress : UInt, packetID : UInt, packetLen : UInt, parms : Parameters) : Flit =  {
        val offset          = 16
        headBundle2Flit.io.inHead.isTail        := Bool(false)
        headBundle2Flit.io.inHead.packetID      := packetID
        headBundle2Flit.io.inHead.packetType    := packetLen
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
    // *********************** </helper functions> *****************************
}

class AXI4NetworkInterfaceWriteTest(c: AXI4NetworkInterface) extends Tester(c) {
    poke (c.io.AXI.AWVALID, 0)
    poke (c.io.AXI.AWID, 0)
    poke (c.io.AXI.AWADDR, 0)
    poke (c.io.AXI.AWLEN, 0)
    poke (c.io.AXI.AWSIZE, 6)
    poke (c.io.AXI.AWBURST, 0)
    poke (c.io.AXI.WVALID, 0)
    poke (c.io.AXI.WDATA, 0)
    poke (c.io.AXI.WSTRB, 0)
    poke (c.io.AXI.BREADY, 0)
    poke (c.io.AXI.ARVALID, 0)
    poke (c.io.AXI.ARADDR, 0)
    poke (c.io.AXI.RREADY, 0)
    poke (c.io.out.credit.grant, 0)
    poke (c.io.in.flit.x, 0)
    poke (c.io.in.flitValid, 0)
    reset(1)
    step(1) // 1

    // Write Data, Addr First
    println("**** Write Packet with Addr Available First ****")
    poke (c.io.AXI.AWADDR, 0x10000)
    poke (c.io.AXI.AWID, 1)
    poke (c.io.AXI.AWLEN, 0)
    poke (c.io.AXI.AWVALID, 1)
    expect(c.io.out.flitValid, 0)
    step(1) // 3
    poke (c.io.AXI.WDATA, 20)
    poke (c.io.AXI.WVALID, 1)
    expect(c.io.AXI.AWREADY, 1)
    expect(c.io.AXI.WREADY, 0)
    expect(c.io.out.flitValid, 1)
    expect(c.io.out.flit.x, 0x00000010411L)
    step(1) // 2
    poke (c.io.AXI.AWVALID, 0)
    expect(c.io.AXI.AWREADY, 0)
    expect(c.io.AXI.WREADY, 1)
    poke (c.io.AXI.WVALID, 1)
    expect(c.io.out.flitValid, 1)
    expect(c.io.out.flit.x, 0x0c200000028L)
    step(1) // 5
    poke (c.io.AXI.WVALID, 0)
    expect(c.io.AXI.WREADY, 0)
    poke (c.io.AXI.BREADY, 1)
    expect(c.io.AXI.BVALID, 1)
    expect(c.io.AXI.BID, 1)
    expect(c.io.out.flitValid, 0)
    step(1) // 6
    expect(c.io.AXI.BRESP, 0)
    poke (c.io.out.credit.grant, 1)
    step(2)
    poke (c.io.out.credit.grant, 0)
    step(2)

    // Write Data, Data First
    println("**** Write Packet with Data Available First ****")
    poke (c.io.AXI.WDATA, 10)
    poke (c.io.AXI.WVALID, 1)
    step(1) // 2
    expect(c.io.AXI.WREADY, 0)
    poke (c.io.AXI.AWADDR, 0x1000000)
    poke (c.io.AXI.AWID, 2)
    poke (c.io.AXI.AWLEN, 0)
    poke (c.io.AXI.AWVALID, 1)
    expect(c.io.out.flitValid, 0)
    step(1) // 3
    expect(c.io.AXI.AWREADY, 1)
    expect(c.io.AXI.WREADY, 0)
    expect(c.io.out.flitValid, 1)
    expect(c.io.out.flit.x, 0x00000020441L)
    step(1)
    poke (c.io.AXI.AWVALID, 0)
    expect(c.io.AXI.WREADY, 1)
    expect(c.io.out.flitValid, 1)
    expect(c.io.out.flit.x, 0x14400000014L)
    step(1) // 5
    poke (c.io.AXI.WVALID, 0)
    poke (c.io.AXI.BREADY, 1)
    expect(c.io.AXI.BVALID, 1)
    expect(c.io.AXI.BID, 2)
    expect(c.io.out.flitValid, 0)
    step(1) // 6
    expect(c.io.AXI.BRESP, 0)
    poke (c.io.out.credit.grant, 1)
    step(2)
    poke (c.io.out.credit.grant, 0)
    step(2)

    // Write Multiple Data, Addr First
    println("**** Write Packet with multiple data bursts ****")
    poke (c.io.AXI.AWADDR, 0x10000)
    poke (c.io.AXI.AWID, 3)
    poke (c.io.AXI.AWLEN, 3)
    poke (c.io.AXI.AWVALID, 1)
    expect(c.io.out.flitValid, 0)
    step(1) // 3
    poke (c.io.AXI.WDATA, 20)
    poke (c.io.AXI.WVALID, 1)
    expect(c.io.AXI.AWREADY, 1)
    expect(c.io.AXI.WREADY, 0)
    expect(c.io.out.flitValid, 1)
    expect(c.io.out.flit.x, 0x00000031011L)
    step(1) // 2
    poke (c.io.AXI.AWVALID, 0)
    expect(c.io.AXI.AWREADY, 0)
    expect(c.io.AXI.WREADY, 1)
    expect(c.io.out.flitValid, 1)
    expect(c.io.out.flit.x, 0x18600000028L)
    step(1) // 5
    poke (c.io.AXI.WVALID, 0)
    expect(c.io.out.flitValid, 0)
    step(1)
    poke (c.io.AXI.WDATA, 21)
    poke (c.io.AXI.WVALID, 1)
    expect(c.io.out.flitValid, 1)
    expect(c.io.out.flit.x, 0x1860000002aL)
    step(1) // 2
    expect(c.io.AXI.WREADY, 1)
    poke (c.io.AXI.WDATA, 22)
    expect(c.io.out.flitValid, 1)
    expect(c.io.out.flit.x, 0x1860000002cL)
    step(1) // 5
    expect(c.io.AXI.WREADY, 1)
    poke (c.io.AXI.WDATA, 23)
    expect(c.io.out.flitValid, 1)
    expect(c.io.out.flit.x, 0x1c60000002eL)
    step(1) // 5
    poke (c.io.AXI.WVALID, 0)
    expect(c.io.AXI.WREADY, 0)
    poke (c.io.AXI.BREADY, 1)
    expect(c.io.AXI.BVALID, 1)
    expect(c.io.AXI.BID, 3)
    expect(c.io.out.flitValid, 0)
    step(1) // 6
    expect(c.io.AXI.BRESP, 0)
    poke (c.io.out.credit.grant, 1)
    step(5)
    poke (c.io.out.credit.grant, 0)
    step(2)
}

class AXI4NetworkInterfaceReadTest(c: AXI4NetworkInterface) extends Tester(c) {
    poke (c.io.AXI.AWVALID, 0)
    poke (c.io.AXI.AWADDR, 0)
    poke (c.io.AXI.WVALID, 0)
    poke (c.io.AXI.WDATA, 0)
    poke (c.io.AXI.WSTRB, 0)
    poke (c.io.AXI.BREADY, 0)
    poke (c.io.AXI.ARVALID, 0)
    poke (c.io.AXI.ARADDR, 0)
    poke (c.io.AXI.RREADY, 0)
    poke (c.io.out.credit.grant, 0)
    poke (c.io.in.flit.x, 0)
    poke (c.io.in.flitValid, 0)
    reset(1)
    step(1) // 1

    // Read to see if packet available (Packet should be unavailable)
    println("**** Read Packet Availability (Packet Unavailable) ****")
    expect(c.io.AXI.ARREADY, 1)
    poke (c.io.AXI.ARADDR, 0)
    poke (c.io.AXI.ARVALID, 1)
    step(1)
    poke (c.io.AXI.ARVALID, 0)
    expect(c.io.AXI.ARREADY, 1)
    step(1)
    expect(c.io.AXI.RVALID, 1)
    expect(c.io.AXI.RDATA, 0)
    expect(c.io.AXI.RRESP, 0)
    poke(c.io.AXI.RREADY, 1)
    step(1)
    expect(c.io.AXI.RVALID, 0)
    step(1)

    // Add head flit
    println("**** Receive Head Flit ****")
    poke (c.io.in.flit.x, 0x10411L)
    poke (c.io.in.flitValid, 1)
    step(1)
    expect(c.io.in.credit.grant, 1)
    poke (c.io.in.flitValid, 0)
    step(1)

    // Read to see if packet availible (Packet should be unavailible)
    println("**** Read Packet Availability (Packet Unavailable) ****")
    expect(c.io.AXI.ARREADY, 1)
    poke (c.io.AXI.ARADDR, 0)
    poke (c.io.AXI.ARVALID, 1)
    step(1)
    poke (c.io.AXI.ARVALID, 0)
    expect(c.io.AXI.ARREADY, 1)
    expect(c.io.AXI.RVALID, 1)
    expect(c.io.AXI.RDATA, 0)
    expect(c.io.AXI.RRESP, 0)
    expect(c.io.AXI.RID, 0)
    expect(c.io.in.credit.grant, 0)
    step(1)
    poke(c.io.AXI.RREADY, 1)
    step(1)
    expect(c.io.AXI.RVALID, 0)
    step(1)

    // Add Body flit
    println("**** Receive Body Flit ****")
    poke (c.io.in.flit.x, 0xc200000028L)
    poke (c.io.in.flitValid, 1)
    step(1)
    poke (c.io.in.flitValid, 0)
    step(1)

    // Read to see if packet availible (Packet should be availible)
    println("**** Read Packet Availability (Packet Available) ****")
    expect(c.io.AXI.ARREADY, 1)
    poke (c.io.AXI.ARADDR, 0)
    poke (c.io.AXI.ARVALID, 1)
    step(1)
    poke (c.io.AXI.ARVALID, 0)
    expect(c.io.AXI.ARREADY, 1)
    expect(c.io.AXI.RVALID, 1)
    expect(c.io.AXI.RDATA, 1)
    expect(c.io.AXI.RID, 1)
    expect(c.io.AXI.RRESP, 0)
    step(1)
    poke(c.io.AXI.RREADY, 1)
    step(1)

    // Read packet
    println("**** Read Packet Data ****")
    expect(c.io.AXI.ARREADY, 1)
    poke (c.io.AXI.ARADDR, 1)
    poke (c.io.AXI.ARVALID, 1)
    step(1)
    poke (c.io.AXI.ARVALID, 0)
    expect(c.io.AXI.ARREADY, 1)
    expect(c.io.AXI.RVALID, 1)
    expect(c.io.AXI.RDATA, 20)
    expect(c.io.AXI.RID, 1)
    expect(c.io.AXI.RRESP, 0)
    expect(c.io.in.credit.grant, 1)
    step(1)
    expect(c.io.AXI.RVALID, 0)
    step(1)
    poke(c.io.AXI.RREADY, 1)
    step(1)
    expect(c.io.AXI.RVALID, 0)
    step(1)

    // Read invalid packet
    println("**** Read Invalid Packet ****")
    expect(c.io.AXI.ARREADY, 1)
    poke (c.io.AXI.ARADDR, 1)
    poke (c.io.AXI.ARVALID, 1)
    step(1)
    poke (c.io.AXI.ARVALID, 0)
    expect(c.io.AXI.ARREADY, 1)
    expect(c.io.AXI.RVALID, 1)
    expect(c.io.AXI.RID, 0)
    expect(c.io.AXI.RRESP, 2)
    step(1)
    poke(c.io.AXI.RREADY, 1)
    step(1)
    expect(c.io.AXI.RVALID, 0)
    step(1)

    // Read to see if packet availible (Packet should be unavailible)
    println("**** Read Packet Availability (Packet Unavailable) ****")
    expect(c.io.AXI.ARREADY, 1)
    poke (c.io.AXI.ARADDR, 0)
    poke (c.io.AXI.ARVALID, 1)
    step(1)
    poke (c.io.AXI.ARVALID, 0)
    expect(c.io.AXI.ARREADY, 1)
    expect(c.io.AXI.RVALID, 1)
    expect(c.io.AXI.RDATA, 0)
    expect(c.io.AXI.RRESP, 0)
    expect(c.io.AXI.RID, 0)
    expect(c.io.in.credit.grant, 0)
    step(1)
    poke(c.io.AXI.RREADY, 1)
    step(1)
    expect(c.io.AXI.RVALID, 0)
    step(1)

    // Read Multiple Data, Addr First
    println("**** Send Flits with multiple body flits ****")

    // Add head flit
    println("**** Send Head Flit ****")
    poke (c.io.in.flit.x, 0x00000031011L)
    poke (c.io.in.flitValid, 1)
    step(1)
    expect(c.io.in.credit.grant, 1)
    poke (c.io.in.flitValid, 0)
    step(1)
    poke (c.io.AXI.AWADDR, 0x10000)

    // Add Body flit
    println("**** Send Body Flit ****")
    poke(c.io.in.flit.x, 0x00000031011L)
    poke (c.io.in.flitValid, 1)
    step(1)
    poke(c.io.in.flit.x, 0x18600000028L)
    poke (c.io.in.flitValid, 1)
    step(1)
    poke(c.io.in.flit.x, 0x1860000002aL)
    poke (c.io.in.flitValid, 1)
    step(1)
    poke(c.io.in.flit.x, 0x1860000002cL)
    poke (c.io.in.flitValid, 1)
    step(1)
    poke(c.io.in.flit.x, 0x1c60000002eL)
    poke (c.io.in.flitValid, 1)
    step(1)
    poke (c.io.in.flitValid, 0)
    step(1)

    // Read to see if packet availible (Packet should be availible)
    println("**** Read Packet Availability (Packet Available) ****")
    expect(c.io.AXI.ARREADY, 1)
    poke (c.io.AXI.ARADDR, 0)
    poke (c.io.AXI.ARVALID, 1)
    step(1)
    poke (c.io.AXI.ARVALID, 0)
    expect(c.io.AXI.ARREADY, 1)
    expect(c.io.AXI.RVALID, 1)
    expect(c.io.AXI.RDATA, 4)
    expect(c.io.AXI.RID, 3)
    expect(c.io.AXI.RRESP, 0)
    step(1)
    poke(c.io.AXI.RREADY, 1)
    step(1)

    // Read packet
    println("**** Read Packet with multiple data bursts ****")
    expect(c.io.AXI.ARREADY, 1)
    poke (c.io.AXI.ARADDR, 1)
    poke (c.io.AXI.ARVALID, 1)
    poke (c.io.AXI.ARLEN, 3)
    step(1)
    poke (c.io.AXI.ARVALID, 0)
    expect(c.io.AXI.ARREADY, 1)
    expect(c.io.AXI.RVALID, 1)
    expect(c.io.AXI.RDATA, 20)
    expect(c.io.AXI.RRESP, 0)
    expect(c.io.AXI.RID, 3)
    expect(c.io.in.credit.grant, 1)
    step(1)
    expect(c.io.AXI.ARREADY, 1)
    expect(c.io.AXI.RVALID, 1)
    expect(c.io.AXI.RDATA, 21)
    expect(c.io.AXI.RRESP, 0)
    expect(c.io.AXI.RID, 3)
    expect(c.io.in.credit.grant, 1)
    step(1)
    expect(c.io.AXI.ARREADY, 1)
    expect(c.io.AXI.RVALID, 1)
    expect(c.io.AXI.RDATA, 22)
    expect(c.io.AXI.RRESP, 0)
    expect(c.io.AXI.RID, 3)
    expect(c.io.in.credit.grant, 1)
    step(1)
    expect(c.io.AXI.ARREADY, 1)
    expect(c.io.AXI.RVALID, 1)
    expect(c.io.AXI.RDATA, 23)
    expect(c.io.AXI.RRESP, 0)
    expect(c.io.AXI.RID, 3)
    expect(c.io.in.credit.grant, 1)
    step(1)
    expect(c.io.AXI.RVALID, 0)
    step(1)


}
/* vim: set shiftwidth=4 tabstop=4 softtabstop=4 expandtab: */
