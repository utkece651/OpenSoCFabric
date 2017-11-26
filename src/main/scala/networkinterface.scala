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
		val AXIPort = new AXI4Lite32()
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

	io.in.ready 					:= packet2Flit.io.packetReady
	packet2Flit.io.packet 			:= io.in.bits
	packet2Flit.io.packetValid 		:= io.in.valid

	creditCon.io.inCredit <> io.out.credit
	io.out.flit := packet2Flit.io.flit
	creditCon.io.inConsume := packet2Flit.io.flitValid
	io.out.flitValid := packet2Flit.io.flitValid
	packet2Flit.io.flitReady := creditCon.io.outCredit
}

class AXINetworkInterface(parms: Parameters) extends NetworkInterface(parms) {
	val queueDepth = parms.get[Int]("queueDepth")
	val creditCon = Chisel.Module ( new CreditCon( parms.child("MyCon", Map(
		("numCreds"->Soft(queueDepth))))) )

   val flitWidth = Flit.fromBits(UInt(0), parms).getWidth()

   // Converters from Head/Body to Flit
   val headBundle2Flit = Chisel.Module( new HeadBundle2Flit(parms) )
   val BodyBundle2Flit = Chisel.Module( new BodyBundle2Flit(parms) )

   // Create States for the State Machine
   val s_idle :: s_createHeadFlit :: s_createBodyFlit :: Nil = Enum(UInt(), 3)

   val state = Reg(init = s_idle)

   // Output flit
   val flitOut = Decoupled(Flit.fromBits(UInt(0), parms))

	creditCon.io.inCredit <> io.out.credit
   io.out.flit := flitOut.bits
	creditCon.io.inConsume := flitOut.valid
   io.out.flitValid := flitOut.valid
	flitOut.ready := creditCon.io.outCredit

   // Flit width must be equal to axi bus width.
   // assert(flitWidth == 32)

   flitOut.valid := Bool(false)

}

class AXINetworkInterfaceTest(c: AXINetworkInterface) extends Tester(c) {
  reset(1)
}
