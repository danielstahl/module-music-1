package net.soundmining
import Note._
import net.soundmining.Instrument.{EFFECT, TAIL_ACTION, setupNodes}
import net.soundmining.Instruments._
import net.soundmining.ModularInstrument.{AudioInstrument, StaticAudioBusInstrument}
import net.soundmining.Spectrum._
import net.soundmining.Utils.absoluteTimeToMillis
import Melody._

object Modular1 {
  val majorSidebands = makeFmSynthesis(noteToHertz('c3), noteToHertz('c2), 50)
  val majorSpectrum = majorSidebands.map(_._1)
  val mirroredSpectrum = majorSidebands.map(_._2)

  def playChordProgression()(implicit player: MusicPlayer): Unit = {
    val chords = Seq(
      (majorSpectrum(3), majorSpectrum(4)),
      (majorSpectrum(5), majorSpectrum(7)),
      (majorSpectrum(6), majorSpectrum(9)),
      (majorSpectrum(10), majorSpectrum(13)),
      (majorSpectrum(11), majorSpectrum(12)),
      (majorSpectrum(14), majorSpectrum(8)),
      (majorSpectrum(12), majorSpectrum(7)),
      (majorSpectrum(10), majorSpectrum(6)),
      (majorSpectrum(8), majorSpectrum(5)))

    chords.zipWithIndex.foreach {
      case (chord, index) => {
        println(s"Chord $index $chord (${hertzToNote(chord._1)}, ${hertzToNote(chord._2)})")
        playChord(chord, index)
      }
    }

    def playChord(chord: (Float, Float), index: Int): Unit = {
      play(index * 13f, 13, sine(0.5f, 5, chord._1), (-0.1f, -0.3f))
      play(index * 13f + 5, 13, sine(0.5f, 5, chord._2), (0.1f, 0.3f))
    }
  }

  def exposition(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    val time = (majorSpectrum(3) / 1000f, majorSpectrum(4) / 1000f)
    val (time1, time2) = time

    val pulseStartTime = time1 * 13
    val pulseDuration = time2 * 13 * 13 * 13
    val delayAudioBus = staticAudioBus()
    val delay = monoDelay(delayAudioBus, delayTime = time2, decayTime = time2 * 13)
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)

    val pan = panning(delay, staticControl(0f))
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)

    pan.getOutputBus.staticBus(0)
    val graph = pan.buildGraph(pulseStartTime, pulseDuration, pan.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(pulseStartTime), graph)

    val rythm = absolute(startTime, Seq(
      // Main theme
      time1 * 13, time1 * 13 + time2 * 5,
      time1 * 13, time1 * 13 + time2 * 5,
      time1 * 13, time1 * 13 + time2 * 5,
      time1 * 13, time1 * 13 + time2 * 5,
      // Second theme
      time2 * 8, time2 * 8, time2 * 8 + time2 * 8,
      time2 * 8, time2 * 8, time2 * 8 + time2 * 8,
      time2 * 8, time2 * 8, time2 * 8 + time2 * 8,
      // Main theme
      time1 * 13, time1 * 13 + time2 * 5,
      time1 * 13, time1 * 13 + time2 * 5,
      time1 * 13, time1 * 13 + time2 * 5
      ))
    val duration = Seq(
      // Main theme
      time1 * 13, time1 * 13,
      time1 * 13, time1 * 13,
      time1 * 13, time1 * 13,
      time1 * 13, time1 * 13,
      // Second theme
      time2 * 8, time2 * 8, time2 * 8,
      time2 * 8, time2 * 8, time2 * 8,
      time2 * 8, time2 * 8, time2 * 8,
      // Main theme
      time1 * 13, time1 * 13,
      time1 * 13, time1 * 13,
      time1 * 13, time1 * 13
    )
    val attack = Seq(
      // Main theme
      time1 * 5, time1 * 8,
      time1 * 5, time1 * 8,
      time1 * 5, time1 * 8,
      time1 * 5, time1 * 8,
      // Second theme
      time2 * 4, time2 * 4, time2 * 4,
      time2 * 4, time2 * 4, time2 * 4,
      time2 * 4, time2 * 4, time2 * 4,
      // Main theme
      time1 * 5, time1 * 8,
      time1 * 5, time1 * 8,
      time1 * 5, time1 * 8
    )

    // Main theme
    val chord1 = (majorSpectrum(3), majorSpectrum(4))

    play(rythm.head, duration.head, simpleFm(0.7f, attack.head, chord1._2, chord1._1, (0, 130), time1 * 8), (-0.7f, 0.7f))
    play(rythm(1), duration(1), simpleFm(0.5f, attack(1), chord1._1, chord1._2, (0, 130), time1 * 5), (0.7f, -0.7f))

    play(pulseStartTime, 0.1f, shortFm( 0.7f, 0.01f, chord1._2, chord1._1, delayAudioBus))

    val chord2 = (majorSpectrum(5), majorSpectrum(7))

    play(rythm(2), duration(2), simpleFm(0.7f, attack(2), chord2._2, chord2._1, (0, 130), time1 * 8), (0.7f, -0.7f))
    play(rythm(3), duration(3), simpleFm(0.5f, attack(3), chord2._1, chord2._2, (0, 130), time1 * 5), (-0.7f, 0.7f))

    play(rythm(3), 0.1f, shortFm( 0.7f, 0.01f, chord2._2, chord2._1, delayAudioBus))

    val chord3 = (majorSpectrum(6), majorSpectrum(9))

    play(rythm(4), duration(4), simpleFm(0.7f, attack(4), chord3._2, chord3._1, (0, 130), time1 * 8), (0.7f, -0.7f))
    play(rythm(5), duration(5), simpleFm(0.5f, attack(5), chord3._1, chord3._2, (0, 130), time1 * 5), (-0.7f, 0.7f))

    play(rythm(5), 0.1f, shortFm( 0.7f, 0.01f, chord3._2, chord3._1, delayAudioBus))

    val chord4 = (majorSpectrum(10), majorSpectrum(13))

    play(rythm(6), duration(6), simpleFm(0.7f, attack(6), chord4._2, chord4._1, (0, 130), time1 * 8), (0.7f, -0.7f))
    play(rythm(7), duration(7), simpleFm(0.5f, attack(7), chord4._1, chord4._2, (0, 130), time1 * 5), (-0.7f, 0.7f))

    play(rythm(7), 0.1f, shortFm( 0.7f, 0.01f, chord4._2, chord4._1, delayAudioBus))

    // Second theme
    val subChord1 = (majorSpectrum(11), majorSpectrum(12))

    play(rythm(8), duration(8), simpleFm(0.4f, attack(8), subChord1._2, subChord1._1, (500, 1500), time2 * 4), (0.2f, -0.2f))
    play(rythm(9), duration(9), simpleFm(0.7f, attack(9), subChord1._1, subChord1._2, (500, 1500), time2 * 4), (0.2f, -0.2f))
    play(rythm(10), duration(10), simpleFm(0.3f, attack(10), subChord1._2, subChord1._1, (500, 1500), time2 * 4), (0.2f, -0.2f))

    val subChord2 = (majorSpectrum(14), majorSpectrum(8))

    play(rythm(11), duration(11), simpleFm(0.4f, attack(11), subChord2._2, subChord2._1, (500, 1500), time2 * 4), (0.2f, -0.2f))
    play(rythm(12), duration(12), simpleFm(0.7f, attack(12), subChord2._1, subChord2._2, (500, 1500), time2 * 4), (0.2f, -0.2f))
    play(rythm(13), duration(13), simpleFm(0.3f, attack(13), subChord2._2, subChord2._1, (500, 1500), time2 * 4), (0.2f, -0.2f))

    val subChord3 = (majorSpectrum(12), majorSpectrum(7))

    play(rythm(14), duration(14), simpleFm(0.4f, attack(14), subChord3._2, subChord3._1, (500, 1500), time2 * 4), (0.2f, -0.2f))
    play(rythm(15), duration(15), simpleFm(0.7f, attack(15), subChord3._1, subChord3._2, (500, 1500), time2 * 4), (0.2f, -0.2f))
    play(rythm(16), duration(16), simpleFm(0.3f, attack(16), subChord3._2, subChord3._1, (500, 1500), time2 * 4), (0.2f, -0.2f))

    // Main theme
    val chord5 = (majorSpectrum(10), majorSpectrum(6))

    play(rythm(17), duration(17), simpleFm(0.7f, attack(17), chord5._2, chord5._1, (0, 130), time1 * 8), (0.7f, -0.7f))
    play(rythm(18), duration(18), simpleFm(0.5f, attack(18), chord5._1, chord5._2, (0, 130), time1 * 5), (-0.7f, 0.7f))

    play(rythm(18), 0.1f, shortFm( 0.7f, 0.01f, chord5._2, chord5._1, delayAudioBus))

    val chord6 = (majorSpectrum(8), majorSpectrum(5))

    play(rythm(19), duration(19), simpleFm(0.7f, attack(19), chord6._2, chord6._1, (0, 130), time1 * 8), (0.7f, -0.7f))
    play(rythm(20), duration(20), simpleFm(0.5f, attack(20), chord6._1, chord6._2, (0, 130), time1 * 5), (-0.7f, 0.7f))

    play(rythm(20), 0.1f, shortFm( 0.7f, 0.01f, chord6._2, chord6._1, delayAudioBus))

    play(rythm(21), duration(21), simpleFm(0.7f, attack(21), chord1._2, chord1._1, (0, 130), time1 * 8), (0.7f, -0.7f))
    play(rythm(22), duration(22), simpleFm(0.5f, attack(22), chord1._1, chord1._2, (0, 130), time1 * 5), (-0.7f, 0.7f))

    play(rythm(22), 0.1f, shortFm( 0.7f, 0.01f, chord1._2, chord1._1, delayAudioBus))
  }

  def sine(ampValue: Float, attackTime: Float, freq: Float): SineOsc = {
    val amp = percControl(0.001f, ampValue, attackTime, Right(Instrument.LINEAR))
    sineOsc(amp, staticControl(freq)).addAction(TAIL_ACTION)
  }

  def pulse(ampValue: Float, attackTime: Float, freq: Float): PulseOsc = {
    val amp = percControl(0, ampValue, attackTime, Right(Instrument.SINE))
    pulseOsc(amp, staticControl(freq)).addAction(TAIL_ACTION)
  }

  def ringTriangle(ampValue: Float, attackTime: Float, modFreq: Float, carrierFreq: Float): RingModulate = {
    val amp = percControl(0.001f, ampValue, attackTime, Right(Instrument.SINE))
    val carrierFreqBus = staticControl(carrierFreq)
    val sineCarrier = sineOsc(amp, carrierFreqBus)
    val triangleCarrier = triangleOsc(amp, carrierFreqBus)
    val xfadeCarrier = xfade(sineCarrier, triangleCarrier, lineControl(1, -1)).addAction(TAIL_ACTION)
    ringModulate(xfadeCarrier, staticControl(modFreq)).addAction(TAIL_ACTION)
  }

  def ringFm(ampValue: Float, carrierFreq: Float, fmModulatorFreq: Float, ringModulatorFreq: Float, attackTime: Float): RingModulate = {
    val amp = percControl(0, ampValue, attackTime, Right(Instrument.SINE))
    val modulator = sineOsc(lineControl(300, 3000), staticControl(fmModulatorFreq))
    val fm = fmSineModulate(staticControl(carrierFreq), modulator, amp).addAction(TAIL_ACTION)
    ringModulate(fm, staticControl(ringModulatorFreq)).addAction(TAIL_ACTION)
  }

  def pulseTriangleFm(ampValue: Float, attackTime: Float, modFreq: Float, carrierFreq: Float): FmModulate = {
    val amp = percControl(0.001f, ampValue, attackTime, Right(Instrument.SINE))
    val modulator = pulseOsc(lineControl(300, 1000), staticControl(modFreq))
    fmTriangleModulate(staticControl(carrierFreq), modulator, amp).addAction(TAIL_ACTION)
  }

  def simpleFm(ampValue: Float, attackTime: Float, modFreq: Float, carrierFreq: Float, modAmount: (Float, Float), modAttackTime: Float): FmModulate = {
    val amp = percControl(0.001f, ampValue, attackTime, Right(Instrument.SINE))
    val modAmountControl = percControl(modAmount._1, modAmount._2, modAttackTime, Right(Instrument.SINE))
    val modulator = sineOsc(modAmountControl, staticControl(modFreq))
    fmSineModulate(staticControl(carrierFreq), modulator, amp).addAction(TAIL_ACTION)
  }

  def shortFm(ampValue: Float, attackTime: Float, modFreq: Float, carrierFreq: Float, output: StaticAudioBusInstrument): FmSineModulate = {
    val amp = percControl(0.001f, ampValue, attackTime, Right(Instrument.EXPONENTIAL))
    val modulator = sineOsc(lineControl(300, 1000), staticControl(modFreq))
    val fmSine = fmSineModulate(staticControl(carrierFreq), modulator, amp)
      .withOutput(output)
      .addAction(TAIL_ACTION)

    fmSine
  }

  def sineRing(ampValue: Float, attackTime: Float, modFreq: Float, carrierFreq: Float): RingModulate = {
    val amp = percControl(0.001f, ampValue, attackTime, Right(Instrument.SINE))
    val carrier = sineOsc(amp, staticControl(carrierFreq)).addAction(TAIL_ACTION)
    ringModulate(carrier, staticControl(modFreq)).addAction(TAIL_ACTION)
  }

  def pulseRing(ampValue: Float, attackTime: Float, modFreq: Float, carrierFreq: Float): RingModulate = {
    val amp = percControl(0.001f, ampValue, attackTime, Right(Instrument.SINE))
    val carrier = pulseOsc(amp, staticControl(carrierFreq)).addAction(TAIL_ACTION)
    ringModulate(carrier, staticControl(modFreq)).addAction(TAIL_ACTION)
  }

  def play(startTime: Float, duration: Float, audio: AudioInstrument, panValue: (Float, Float))(implicit player: MusicPlayer): Unit = {
    val pan = panning(audio, lineControl(panValue._1, panValue._2))
      .addAction(TAIL_ACTION)
    pan.getOutputBus.staticBus(0)
    val graph = pan.buildGraph(startTime, duration, pan.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(startTime), graph)
  }

  def play(startTime: Float, duration: Float, audio: AudioInstrument)(implicit player: MusicPlayer): Unit = {

    val graph = audio.buildGraph(startTime, duration, audio.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(startTime), graph)
  }

  def main(args: Array[String]): Unit = {

    implicit val player: MusicPlayer = MusicPlayer()
    player.startPlay()
    setupNodes(player)

    println("spectrum")
    println(majorSpectrum.zipWithIndex.mkString(", "))

    println("mirrored spectrum")
    println(mirroredSpectrum.zipWithIndex.mkString(", "))

    println("spectrum")
    println(majorSpectrum.map(hertzToNote).zipWithIndex.mkString(", "))

    println("mirrored spectrum")
    println(mirroredSpectrum.map(hertzToNote).zipWithIndex.mkString(", "))

    exposition(0f)
  }

}
