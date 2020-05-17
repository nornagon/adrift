package adrift

import java.nio.file.Paths

import javax.sound.sampled.AudioFormat.Encoding
import javax.sound.sampled.{AudioFormat, AudioSystem}

object SoundTest {
  def main(args: Array[String]): Unit = {
    if (args.contains("--jsyn"))
      jsynbased()
    else if (args.contains("--javax"))
      javaxbased()
    else
      throw new RuntimeException("Pass --jsyn or --javax")
  }

  private def getOutFormat(inFormat: AudioFormat) = {
    val ch = inFormat.getChannels
    val rate = inFormat.getSampleRate
    new AudioFormat(Encoding.PCM_SIGNED, rate, 16, ch, ch * 2, rate, false)
  }

  private def javaxbased() = {
    // Modified from https://odoepner.wordpress.com/2013/07/19/play-mp3-or-ogg-using-javax-sound-sampled-mp3spi-vorbisspi/
    val in = AudioSystem.getAudioInputStream(Paths.get("tng_door_open.mp3").toFile)
    val outFormat = getOutFormat(in.getFormat)
    val clip = AudioSystem.getClip
    clip.open(AudioSystem.getAudioInputStream(outFormat, in))
    clip.start()
    Thread.sleep(clip.getMicrosecondLength / 1000)
  }

  private def jsynbased() = {
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
