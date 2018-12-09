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

  // (130.8128,0),
  // (196.21921,1),
  // (261.6256,2),
  // (327.032,3),
  // (392.43842,4),
  // (457.84482,5),
  // (523.2512,6),
  // (588.6576,7),
  // (654.064,8),
  // (719.4704,9),
  // (784.87683,10),
  // (850.28326,11),
  // (915.68964,12),
  // (981.096,13),
  // (1046.5024,14),
  // (1111.9089,15),
  // (1177.3152,16),
  // (1242.7217,17),
  // (1308.1279,18),
  // (1373.5344,19),
  // (1438.9409,20),
  def chord1(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    val chord = (majorSpectrum(3), majorSpectrum(4))
    val (note1, note2) = chord

    val time = (majorSpectrum(3) / 1000f, majorSpectrum(4) / 1000f)
    val (time1, time2) = time

    println(s"chord $chord time $time")

    val rythm1 = absolute(startTime, Seq(time1 * 13, time1 * 13))
    val duration1 = Seq(time1 * 34, time1 * 34)
    val attack1 = Seq(time1 * 21, time1 * 13)

    play(rythm1.head, duration1.head, ringTriangle(1, attack1.head, note1, note2), (-0.7f, 0.7f))
    play(rythm1(1), duration1(1), ringFm(0.8f, note1, note2,  note2, attack1(1)), (0.5f, 0.9f))

    val rythm2 = absolute(startTime, Seq(time2 * 8, time2 * 21, time2 * 21, time2 * 21))
    val duration2 = Seq(time2 * 34, time2 * 13, time2 * 21)
    val attack2 = Seq(time2 * 13, time2 * 8, time2 * 13)

    play(rythm2.head, duration2.head, sine( 0.1f, attack2.head, note1), (-0.1f, -0.3f))
    play(rythm2.head, duration2.head, pulse( 0.05f, attack2.head, note1), (-0.5f, -0.2f))

    play(rythm2(1), duration2(1), sine( 0.1f, attack2(1), note2), (0.1f, 0.3f))
    play(rythm2(1), duration2(1), pulse( 0.05f, attack2(1), note2), (0.5f, 0.2f))

    play(rythm2(2), duration2(2), pulseTriangleFm(0.4f, attack2(2), note2,  note1), (-0.2f, -0.6f))

    chord2(rythm2(3))
  }

  /*
  * We should try to make pulses also. Alternate between pulses and long chords.
  * Think Donna Summer I feel love. Delay? Long notes with Moog Filter.
  *
  * We could set up different effects that have long durations. Delays,
  * flanger, moog filters etc. Then we could send different tones via
  * different effects. E.g one note via delay and flanger, another
  * note via moog filter and reverb. Also pan could be treated the same.
  * */
  def pulse1(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    val chord = (majorSpectrum(3), majorSpectrum(4))
    val (note1, note2) = chord

    val time = (majorSpectrum(3) / 1000f, majorSpectrum(4) / 1000f)
    val (time1, time2) = time

    println(s"chord $chord time $time")

    val rythm1 = absolute(startTime, Seq(time1 * 2, time1 * 2, time1 * 2, time1 * 2))
    val duration1 = Seq(time1, time1, time1, time1)

    val delayAudioBus = staticAudioBus()
    val delay = monoDelay(delayAudioBus, delayTime = time2 / 2, decayTime = time2 * 13)
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)

    //val pan = panning(delay, lineControl(-0.5f, 0.5f))
    val pan = panning(delay, sineControl(staticControl(time2 / 2), -0.5f, 0.5f))
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)

    pan.getOutputBus.staticBus(0)
    val graph = pan.buildGraph(startTime, 20, pan.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(startTime), graph)

    play(rythm1.head, duration1.head, shortFm( 0.2f, 0.01f, note2, note1, delayAudioBus))
    play(rythm1(1), duration1(1), shortFm( 0.4f, 0.01f, note2, note1, delayAudioBus))
    play(rythm1(2), duration1(2), shortFm( 0.6f, 0.01f, note2, note1, delayAudioBus))
    play(rythm1(3), duration1(3), shortFm( 0.8f, 0.01f, note2, note1, delayAudioBus))
  }

  def mainTheme1(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    val chord = (majorSpectrum(3), majorSpectrum(4))
    val (note1, note2) = chord

    val time = (majorSpectrum(3) / 1000f, majorSpectrum(4) / 1000f)
    val (time1, time2) = time

    println(s"chord $chord time $time")

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

    play(pulseStartTime, 0.1f, shortFm( 0.7f, 0.01f, note2, note1, delayAudioBus))

    val rythm1 = absolute(startTime, Seq(
      time1 * 13, time1 * 13 + time2 * 5,
      time1 * 13, time1 * 13 + time2 * 5,
      time1 * 13, time1 * 13 + time2 * 5,
      time1 * 13, time1 * 13 + time2 * 5, time1 * 13))
    val duration1 = Seq(
      time1 * 13, time1 * 13,
      time1 * 13, time1 * 13,
      time1 * 13, time1 * 13,
      time1 * 13, time1 * 13)
    val attack1 = Seq(
      time1 * 5, time1 * 8,
      time1 * 5, time1 * 8,
      time1 * 5, time1 * 8,
      time1 * 5, time1 * 8)

    println(rythm1)

    play(rythm1.head, duration1.head, simpleFm(0.7f, attack1.head, note2, note1, (0, 130), time1 * 8), (-0.7f, 0.7f))
    play(rythm1(1), duration1(1), simpleFm(0.5f, attack1(1), note1, note2, (0, 130), time1 * 5), (0.7f, -0.7f))

    val chord2 = (majorSpectrum(5), majorSpectrum(7))

    play(rythm1(2), duration1(2), simpleFm(0.7f, attack1(2), chord2._2, chord2._1, (0, 130), time1 * 8), (0.7f, -0.7f))
    play(rythm1(3), duration1(3), simpleFm(0.5f, attack1(3), chord2._1, chord2._2, (0, 130), time1 * 5), (-0.7f, 0.7f))

    play(rythm1(3), 0.1f, shortFm( 0.7f, 0.01f, chord2._2, chord2._1, delayAudioBus))

    val chord3 = (majorSpectrum(6), majorSpectrum(9))

    play(rythm1(4), duration1(4), simpleFm(0.7f, attack1(4), chord3._2, chord3._1, (0, 130), time1 * 8), (0.7f, -0.7f))
    play(rythm1(5), duration1(5), simpleFm(0.5f, attack1(5), chord3._1, chord3._2, (0, 130), time1 * 5), (-0.7f, 0.7f))

    play(rythm1(5), 0.1f, shortFm( 0.7f, 0.01f, chord3._2, chord3._1, delayAudioBus))

    val chord4 = (majorSpectrum(10), majorSpectrum(13))

    play(rythm1(6), duration1(6), simpleFm(0.7f, attack1(6), chord4._2, chord4._1, (0, 130), time1 * 8), (0.7f, -0.7f))
    play(rythm1(7), duration1(7), simpleFm(0.5f, attack1(7), chord4._1, chord4._2, (0, 130), time1 * 5), (-0.7f, 0.7f))

    play(rythm1(7), 0.1f, shortFm( 0.7f, 0.01f, chord4._2, chord4._1, delayAudioBus))

    // 85.8132, 3.1395073

    //10 6  8 5

    val rythm2 = absolute(85.8132f, Seq(
      time1 * 13, time1 * 13 + time2 * 5,
      time1 * 13, time1 * 13 + time2 * 5,
      time1 * 13, time1 * 13 + time2 * 5))
    val duration2 = Seq(
      time1 * 13, time1 * 13,
      time1 * 13, time1 * 13,
      time1 * 13, time1 * 13)
    val attack2 = Seq(
      time1 * 5, time1 * 8,
      time1 * 5, time1 * 8,
      time1 * 5, time1 * 8)

    val chord5 = (majorSpectrum(10), majorSpectrum(6))

    play(rythm2.head, duration2.head, simpleFm(0.7f, attack2.head, chord5._2, chord5._1, (0, 130), time1 * 8), (0.7f, -0.7f))
    play(rythm2(1), duration2(1), simpleFm(0.5f, attack2(1), chord5._1, chord5._2, (0, 130), time1 * 5), (-0.7f, 0.7f))

    play(rythm2(1), 0.1f, shortFm( 0.7f, 0.01f, chord5._2, chord5._1, delayAudioBus))

    val chord6 = (majorSpectrum(8), majorSpectrum(5))

    play(rythm2(2), duration2(2), simpleFm(0.7f, attack2(2), chord6._2, chord6._1, (0, 130), time1 * 8), (0.7f, -0.7f))
    play(rythm2(3), duration2(3), simpleFm(0.5f, attack2(3), chord6._1, chord6._2, (0, 130), time1 * 5), (-0.7f, 0.7f))

    play(rythm2(3), 0.1f, shortFm( 0.7f, 0.01f, chord6._2, chord6._1, delayAudioBus))

    play(rythm2(4), duration2(4), simpleFm(0.7f, attack2(4), note2, note1, (0, 130), time1 * 8), (0.7f, -0.7f))
    play(rythm2(5), duration2(5), simpleFm(0.5f, attack2(5), note1, note2, (0, 130), time1 * 5), (-0.7f, 0.7f))

    play(rythm2(5), 0.1f, shortFm( 0.7f, 0.01f, note2, note1, delayAudioBus))

  }

  def subTheme1(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    val chord = (majorSpectrum(11), majorSpectrum(12))
    val (note1, note2) = chord

    val time = (majorSpectrum(3) / 1000f, majorSpectrum(4) / 1000f)
    val (time1, time2) = time

    println(s"chord $chord time $time")

    val rythm1 = absolute(startTime, Seq(
      time2 * 8, time2 * 8, time2 * 8 + time2 * 8,
      time2 * 8, time2 * 8, time2 * 8 + time2 * 8,
      time2 * 8, time2 * 8, time2 * 8 + time2 * 8,
      time2 * 8, time2 * 8, time2 * 8 + time2 * 8))
    val duration1 = Seq(
      time2 * 8, time2 * 8, time2 * 8,
      time2 * 8, time2 * 8, time2 * 8,
      time2 * 8, time2 * 8, time2 * 8,
      time2 * 8, time2 * 8, time2 * 8)
    val attack1 = Seq(
      time2 * 4, time2 * 4, time2 * 4,
      time2 * 4, time2 * 4, time2 * 4,
      time2 * 4, time2 * 4, time2 * 4,
      time2 * 4, time2 * 4, time2 * 4)

    println(s"Subtheme times $rythm1 durations $duration1")

    play(rythm1.head, duration1.head, simpleFm(0.4f, attack1.head, note2, note1, (500, 1500), time2 * 4), (0.2f, -0.2f))
    play(rythm1(1), duration1(1), simpleFm(0.7f, attack1(1), note1, note2, (500, 1500), time2 * 4), (0.2f, -0.2f))
    play(rythm1(2), duration1(2), simpleFm(0.3f, attack1(2), note2, note1, (500, 1500), time2 * 4), (0.2f, -0.2f))

    val chord2 = (majorSpectrum(14), majorSpectrum(8))

    play(rythm1(3), duration1(3), simpleFm(0.4f, attack1(3), chord2._2, chord2._1, (500, 1500), time2 * 4), (0.2f, -0.2f))
    play(rythm1(4), duration1(4), simpleFm(0.7f, attack1(4), chord2._1, chord2._2, (500, 1500), time2 * 4), (0.2f, -0.2f))
    play(rythm1(5), duration1(5), simpleFm(0.3f, attack1(5), chord2._2, chord2._1, (500, 1500), time2 * 4), (0.2f, -0.2f))

    val chord3 = (majorSpectrum(12), majorSpectrum(7))

    play(rythm1(6), duration1(6), simpleFm(0.4f, attack1(3), chord3._2, chord3._1, (500, 1500), time2 * 4), (0.2f, -0.2f))
    play(rythm1(7), duration1(7), simpleFm(0.7f, attack1(4), chord3._1, chord3._2, (500, 1500), time2 * 4), (0.2f, -0.2f))
    play(rythm1(8), duration1(8), simpleFm(0.3f, attack1(5), chord3._2, chord3._1, (500, 1500), time2 * 4), (0.2f, -0.2f))
  }

  def chord2(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    val chord = (majorSpectrum(5), majorSpectrum(7))
    val (note1, note2) = chord

    val time = (majorSpectrum(5) / 1000f, majorSpectrum(7) / 1000f)
    val (time1, time2) = time

    val rythm1 = absolute(startTime, Seq(time1 * 5, time1 * 5))
    val duration1 = Seq(time1 * 13, time1 * 21)
    val attack1 = Seq(time1 * 5, time1 * 8)

    play(rythm1.head, duration1.head, simpleFm(0.5f, attack1.head, note2, note1, (300, 1000), attack1.head), (-0.7f, 0.7f))
    play(rythm1(1), duration1(1), pulseRing(1f, attack1(1), note1, note2), (0.7f, -0.7f))
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

    //playChordProgression()
    //chord1(0f)

    //chord1(0f)
    //pulse1(0f)
    mainTheme1(0f)
    subTheme1(41.860096f)
  }

}
