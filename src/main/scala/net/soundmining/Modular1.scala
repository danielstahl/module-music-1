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

    val rhythm = absolute(startTime, Seq(
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

    play(rhythm.head, duration.head, simpleFm(0.7f, attack.head, chord1._2, chord1._1, (0, 130), time1 * 8), (-0.7f, 0.7f))
    play(rhythm(1), duration(1), simpleFm(0.5f, attack(1), chord1._1, chord1._2, (0, 130), time1 * 5), (0.7f, -0.7f))

    play(pulseStartTime, 0.1f, shortFm( 0.7f, 0.01f, chord1._2, chord1._1, delayAudioBus))

    val chord2 = (majorSpectrum(5), majorSpectrum(7))

    play(rhythm(2), duration(2), simpleFm(0.7f, attack(2), chord2._2, chord2._1, (0, 130), time1 * 8), (0.7f, -0.7f))
    play(rhythm(3), duration(3), simpleFm(0.5f, attack(3), chord2._1, chord2._2, (0, 130), time1 * 5), (-0.7f, 0.7f))

    play(rhythm(3), 0.1f, shortFm( 0.7f, 0.01f, chord2._2, chord2._1, delayAudioBus))

    val chord3 = (majorSpectrum(6), majorSpectrum(9))

    play(rhythm(4), duration(4), simpleFm(0.7f, attack(4), chord3._2, chord3._1, (0, 130), time1 * 8), (0.7f, -0.7f))
    play(rhythm(5), duration(5), simpleFm(0.5f, attack(5), chord3._1, chord3._2, (0, 130), time1 * 5), (-0.7f, 0.7f))

    play(rhythm(5), 0.1f, shortFm( 0.7f, 0.01f, chord3._2, chord3._1, delayAudioBus))

    val chord4 = (majorSpectrum(10), majorSpectrum(13))

    play(rhythm(6), duration(6), simpleFm(0.7f, attack(6), chord4._2, chord4._1, (0, 130), time1 * 8), (0.7f, -0.7f))
    play(rhythm(7), duration(7), simpleFm(0.5f, attack(7), chord4._1, chord4._2, (0, 130), time1 * 5), (-0.7f, 0.7f))

    play(rhythm(7), 0.1f, shortFm( 0.7f, 0.01f, chord4._2, chord4._1, delayAudioBus))

    // Second theme
    val subChord1 = (majorSpectrum(11), majorSpectrum(12))

    play(rhythm(8), duration(8), simpleFm(0.4f, attack(8), subChord1._2, subChord1._1, (500, 1500), time2 * 4), (0.2f, -0.2f))
    play(rhythm(9), duration(9), simpleFm(0.7f, attack(9), subChord1._1, subChord1._2, (500, 1500), time2 * 4), (-0.2f, 0.2f))
    play(rhythm(10), duration(10), simpleFm(0.3f, attack(10), subChord1._2, subChord1._1, (500, 1500), time2 * 4), (0.2f, -0.2f))

    val subChord2 = (majorSpectrum(14), majorSpectrum(8))

    play(rhythm(11), duration(11), simpleFm(0.4f, attack(11), subChord2._2, subChord2._1, (500, 1500), time2 * 4), (0.2f, -0.2f))
    play(rhythm(12), duration(12), simpleFm(0.7f, attack(12), subChord2._1, subChord2._2, (500, 1500), time2 * 4), (-0.2f, 0.2f))
    play(rhythm(13), duration(13), simpleFm(0.3f, attack(13), subChord2._2, subChord2._1, (500, 1500), time2 * 4), (0.2f, -0.2f))

    val subChord3 = (majorSpectrum(12), majorSpectrum(7))

    play(rhythm(14), duration(14), simpleFm(0.4f, attack(14), subChord3._2, subChord3._1, (500, 1500), time2 * 4), (0.2f, -0.2f))
    play(rhythm(15), duration(15), simpleFm(0.7f, attack(15), subChord3._1, subChord3._2, (500, 1500), time2 * 4), (-0.2f, 0.2f))
    play(rhythm(16), duration(16), simpleFm(0.3f, attack(16), subChord3._2, subChord3._1, (500, 1500), time2 * 4), (0.2f, -0.2f))

    // Main theme
    val chord5 = (majorSpectrum(10), majorSpectrum(6))

    play(rhythm(17), duration(17), simpleFm(0.7f, attack(17), chord5._2, chord5._1, (0, 130), time1 * 8), (0.7f, -0.7f))
    play(rhythm(18), duration(18), simpleFm(0.5f, attack(18), chord5._1, chord5._2, (0, 130), time1 * 5), (-0.7f, 0.7f))

    play(rhythm(18), 0.1f, shortFm( 0.7f, 0.01f, chord5._2, chord5._1, delayAudioBus))

    val chord6 = (majorSpectrum(8), majorSpectrum(5))

    play(rhythm(19), duration(19), simpleFm(0.7f, attack(19), chord6._2, chord6._1, (0, 130), time1 * 8), (0.7f, -0.7f))
    play(rhythm(20), duration(20), simpleFm(0.5f, attack(20), chord6._1, chord6._2, (0, 130), time1 * 5), (-0.7f, 0.7f))

    play(rhythm(20), 0.1f, shortFm( 0.7f, 0.01f, chord6._2, chord6._1, delayAudioBus))

    // This should perhaps be in the development
/*
    play(rythm(21), duration(21), simpleFm(0.7f, attack(21), chord1._2, chord1._1, (0, 130), time1 * 8), (0.7f, -0.7f))
    play(rythm(22), duration(22), simpleFm(0.5f, attack(22), chord1._1, chord1._2, (0, 130), time1 * 5), (-0.7f, 0.7f))

    play(rythm(22), 0.1f, shortFm( 0.7f, 0.01f, chord1._2, chord1._1, delayAudioBus))*/
  }

  def development1(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    val time = (majorSpectrum(3) / 1000f, majorSpectrum(4) / 1000f)
    val (time1, time2) = time

    val chord1 = (majorSpectrum(3), majorSpectrum(4))

    val chord1Sidebands = makeFmSynthesis(chord1._1, chord1._2, 50)
    val chord1Spectrum = majorSidebands.map(_._1)

    println(s"Chord1 spectrum $chord1Spectrum")

    val longRhythm = absolute(startTime, Seq(time2 * 21, time2 * 21, time2 * 21, time2 * 21))
    val longDuration = Seq(time2 * 21, time2 * 21, time2 * 21, time2 * 21)
    val longAttack = Seq(time2 * 13, time2 * 13, time2 * 13, time2 * 13)

    play(longRhythm.head, longDuration.head, sineRing(1.1f, longAttack.head, chord1Spectrum(8), chord1Spectrum(5)), (0.7f, -0.7f))
    play(longRhythm(1), longDuration(1), sineRing(1.1f, longAttack(1), chord1Spectrum(10), chord1Spectrum(6)), (-0.7f, 0.7f))
    play(longRhythm(2), longDuration(2), sineRing(1.1f, longAttack(2), chord1Spectrum(9), chord1Spectrum(7)), (0.7f, -0.7f))
    play(longRhythm(3), longDuration(3), sineRing(1.1f, longAttack(3), chord1Spectrum(11), chord1Spectrum(8)), (-0.7f, 0.7f))

    val shortRhythm = absolute(startTime, Seq(time1 * 8, time1 * 8, time1 * 8, time1 * 8, time1 * 8, time1 * 8, time1 * 8, time1 * 8, time1 * 8, time1 * 8, time1 * 8, time1 * 8))
    val shortDuration = Seq(time1 * 13, time1 * 21, time1 * 13, time1 * 21, time1 * 13, time1 * 21, time1 * 13, time1 * 21, time1 * 13, time1 * 21, time1 * 13, time1 * 21)
    val shortAttack = Seq(time1 * 8, time1 * 5, time1 * 8, time1 * 5, time1 * 8, time1 * 5, time1 * 8, time1 * 5, time1 * 8, time1 * 5, time1 * 8, time1 * 5)

    play(shortRhythm.head, shortDuration.head, sineFm(0.5f, shortAttack.head, chord1Spectrum(3), sineModulator(chord1Spectrum(4), (100, 1000), time1 * 8)), (-0.3f, 0.3f))
    play(shortRhythm(1), shortDuration(1), triangleFm(0.5f, shortAttack(1), chord1Spectrum(4), pulseModulator(chord1Spectrum(3), (100, 900), time1 * 5)), (0.3f, -0.3f))

    play(shortRhythm(2), shortDuration(2), sineFm(0.5f, shortAttack(2), chord1Spectrum(5), triangleModulator(chord1Spectrum(7), (100, 1000), time1 * 8)), (0.3f, -0.3f))
    play(shortRhythm(3), shortDuration(3), triangleFm(0.5f, shortAttack(3), chord1Spectrum(7), sineModulator(chord1Spectrum(5), (100, 900), time1 * 5)), (-0.3f, 0.3f))

    play(shortRhythm(4), shortDuration(4), sineFm(0.5f, shortAttack(4), chord1Spectrum(6), pulseModulator(chord1Spectrum(9), (100, 900), time1 * 5)), (0.3f, -0.3f))
    play(shortRhythm(5), shortDuration(5), triangleFm(0.5f, shortAttack(5), chord1Spectrum(9), sineModulator(chord1Spectrum(6), (100, 900), time1 * 8)), (-0.3f, 0.3f))

    play(shortRhythm(6), shortDuration(6), sineFm(0.5f, shortAttack(6), chord1Spectrum(13), triangleModulator(chord1Spectrum(10), (100, 1000), time1 * 8)), (0.3f, -0.3f))
    play(shortRhythm(7), shortDuration(7), triangleFm(0.5f, shortAttack(7), chord1Spectrum(10), pulseModulator(chord1Spectrum(13), (100, 900), time1 * 5)), (-0.3f, 0.3f))

    play(shortRhythm(8), shortDuration(8), sineFm(0.5f, shortAttack(8), chord1Spectrum(12), sineModulator(chord1Spectrum(11), (100, 1100), time1 * 5)), (0.3f, -0.3f))
    play(shortRhythm(9), shortDuration(9), triangleFm(0.5f, shortAttack(9), chord1Spectrum(11), triangleModulator(chord1Spectrum(12), (100, 1100), time1 * 8)), (-0.3f, 0.3f))

    play(shortRhythm(10), shortDuration(10), sineFm(0.5f, shortAttack(10), chord1Spectrum(14), pulseModulator(chord1Spectrum(8), (100, 1100), time1 * 5)), (0.3f, -0.3f))
    play(shortRhythm(11), shortDuration(11), triangleFm(0.5f, shortAttack(11), chord1Spectrum(8), sineModulator(chord1Spectrum(14), (100, 1100), time1 * 8)), (-0.3f, 0.3f))
  }

  def sine(ampValue: Float, attackTime: Float, freq: Float): SineOsc = {
    val amp = percControl(0.001f, ampValue, attackTime, Right(Instrument.LINEAR))
    sineOsc(amp, staticControl(freq)).addAction(TAIL_ACTION)
  }

  def pulse(ampValue: Float, attackTime: Float, freq: Float): PulseOsc = {
    val amp = percControl(0, ampValue, attackTime, Right(Instrument.SINE))
    pulseOsc(amp, staticControl(freq)).addAction(TAIL_ACTION)
  }

  def ringSine(ampValue: Float, attackTime: Float, modFreq: Float, carrierFreq: Float): RingModulate = {
    val amp = percControl(0.001f, ampValue, attackTime, Right(Instrument.SINE))
    val carrierFreqBus = staticControl(carrierFreq)
    val sineCarrier = sineOsc(amp, carrierFreqBus)
    ringModulate(sineCarrier, staticControl(modFreq)).addAction(TAIL_ACTION)
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

  def simpleFm(ampValue: Float, attackTime: Float, modFreq: Float, carrierFreq: Float, modAmount: (Float, Float), modAttackTime: Float): FmModulate = {
    val amp = percControl(0.001f, ampValue, attackTime, Right(Instrument.SINE))
    val modAmountControl = percControl(modAmount._1, modAmount._2, modAttackTime, Right(Instrument.SINE))
    val modulator = sineOsc(modAmountControl, staticControl(modFreq))
    fmSineModulate(staticControl(carrierFreq), modulator, amp).addAction(TAIL_ACTION)
  }

  def pulseModulator(modFreq: Float, modAmount: (Float, Float), modAttackTime: Float): PulseOsc = {
    val modAmountControl = percControl(modAmount._1, modAmount._2, modAttackTime, Right(Instrument.SINE))
    pulseOsc(modAmountControl, staticControl(modFreq))
  }

  def sineModulator(modFreq: Float, modAmount: (Float, Float), modAttackTime: Float): SineOsc = {
    val modAmountControl = percControl(modAmount._1, modAmount._2, modAttackTime, Right(Instrument.SINE))
    sineOsc(modAmountControl, staticControl(modFreq))
  }

  def triangleModulator(modFreq: Float, modAmount: (Float, Float), modAttackTime: Float): TriangleOsc = {
    val modAmountControl = percControl(modAmount._1, modAmount._2, modAttackTime, Right(Instrument.SINE))
    triangleOsc(modAmountControl, staticControl(modFreq))
  }

  def sineFm(ampValue: Float, attackTime: Float, carrierFreq: Float, modulator: AudioInstrument): FmSineModulate = {
    val amp = percControl(0.001f, ampValue, attackTime, Right(Instrument.SINE))
    fmSineModulate(staticControl(carrierFreq), modulator, amp).addAction(TAIL_ACTION)
  }

  def triangleFm(ampValue: Float, attackTime: Float, carrierFreq: Float, modulator: AudioInstrument): FmTriangleModulate = {
    val amp = percControl(0.001f, ampValue, attackTime, Right(Instrument.SINE))
    fmTriangleModulate(staticControl(carrierFreq), modulator, amp).addAction(TAIL_ACTION)
  }

  def pulseFm(ampValue: Float, attackTime: Float, carrierFreq: Float, modulator: AudioInstrument): FmPulseModulate = {
    val amp = percControl(0.001f, ampValue, attackTime, Right(Instrument.SINE))
    fmPulseModulate(staticControl(carrierFreq), modulator, amp).addAction(TAIL_ACTION)
  }

  def xfadeModulateFm(ampValue: Float, attackTime: Float, modFreq: Float, carrierFreq: Float, modAmount: (Float, Float), modAttackTime: Float): FmModulate = {
    val amp = percControl(0.001f, ampValue, attackTime, Right(Instrument.SINE))
    val modAmountControl = percControl(modAmount._1, modAmount._2, modAttackTime, Right(Instrument.SINE))
    val modFreqControl = staticControl(modFreq)

    val modulator = xfade(
      sineOsc(modAmountControl, modFreqControl),
      pulseOsc(modAmountControl, modFreqControl),
      lineControl(-1, 1)).addAction(TAIL_ACTION)

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
    development1(104.71566f)
  }

}
