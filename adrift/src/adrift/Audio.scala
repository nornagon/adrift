package adrift

import java.nio.file.Paths

import javax.sound.sampled.AudioFormat.Encoding
import javax.sound.sampled.{AudioFormat, AudioSystem}

object Audio {
  private def getOutFormat(inFormat: AudioFormat) = {
    val ch = inFormat.getChannels
    val rate = inFormat.getSampleRate
    new AudioFormat(Encoding.PCM_SIGNED, rate, 16, ch, ch * 2, rate, false)
  }

  def play(soundPath: String) = {
    // Modified from https://odoepner.wordpress.com/2013/07/19/play-mp3-or-ogg-using-javax-sound-sampled-mp3spi-vorbisspi/
    val in = AudioSystem.getAudioInputStream(Paths.get(soundPath).toFile)
    val outFormat = getOutFormat(in.getFormat)
    val clip = AudioSystem.getClip
    clip.open(AudioSystem.getAudioInputStream(outFormat, in))
    clip.start()
  }
}
