package adrift

import java.nio.file.Paths

import adrift.display.{Display, GLFWDisplay}
import adrift.worldgen.WorldGen
import javax.sound.sampled.AudioSystem

import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    //jsyn(); return

    //javax(); return

    val data = Data.parse(Paths.get("data"))
    implicit val random: Random = new Random(52)
    val state = WorldGen(data).generateWorld
    state.recalculateFOV()
    val display: Display = new GLFWDisplay
    display.init()
    display.update(state)
    while (display.running) {
      val action = display.waitForAction
      state.receive(action)
      display.update(state)
    }
  }

  private def javax() = {
    val sound = AudioSystem.getAudioInputStream(Paths.get("hum.wav").toFile)
    val clip = AudioSystem.getClip

    clip.open(sound)
    clip.start()

    val sound2 = AudioSystem
      .getAudioInputStream(Paths.get("264014__cell31-sound-productions__door-metal-groans-ext.wav").toFile)
    val clip2 = AudioSystem.getClip
    clip2.open(sound2)
    clip2.start()

    Thread.sleep(clip.getMicrosecondLength / 1000)
  }

  private def jsyn() = {
    import com.jsyn.JSyn
    import com.jsyn.unitgen._
    import com.jsyn.util._
    val synth = JSyn.createSynthesizer()

    synth.start()

    val out = new LineOut
    synth.add(out)

    val sample = SampleLoader.loadFloatSample(Paths.get("155720__chrisw92__machine-hum-clean.wav").toFile)
    val player = new FixedRateStereoReader
    synth.add(player)
    player.dataQueue.queue(sample)

    val filter = new FilterStateVariable
    synth.add(filter)
    filter.frequency.set(660)
    filter.resonance.set(1)

    player.output.connect(0, filter.input, 0)
    filter.output.connect(0, out.input, 0)
    filter.output.connect(0, out.input, 1)


    val sample2 = SampleLoader
      .loadFloatSample(Paths.get("264014__cell31-sound-productions__door-metal-groans-ext.wav").toFile)
    val player2 = new VariableRateStereoReader
    player2.rate.set(sample2.getFrameRate)
    synth.add(player2)
    player2.dataQueue.queue(sample2)
    player2.output.connect(0, out.input, 0)
    player2.output.connect(0, out.input, 1)
    /*

    val noise = new WhiteNoise
    synth.add(noise)
    val filter = new FilterStateVariable
    synth.add(filter)

    noise.output.connect(filter.input)
    filter.output.connect(0, out.input, 0)
    filter.output.connect(0, out.input, 1)

    */
    out.start()

    Thread.sleep((sample.getNumFrames / sample.getFrameRate * 1000).ceil.toLong)

    synth.stop()
  }
}
