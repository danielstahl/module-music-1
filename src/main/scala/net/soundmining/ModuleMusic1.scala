package net.soundmining
import Note._
import net.soundmining.synth.Instrument.{EFFECT, EnvCurve, TAIL_ACTION, setupNodes}
import net.soundmining.modular.ModularSynth._
import net.soundmining.modular.ModularInstrument.{AudioInstrument, StaticAudioBusInstrument}
import net.soundmining.Spectrum._
import net.soundmining.synth.Utils.absoluteTimeToMillis
import Melody._
import net.soundmining.synth._
import net.soundmining.synth.Instrument.{EnvCurve, TAIL_ACTION, setupNodes, EFFECT}
import SuperColliderClient._

object ModuleMusic1 {
  val SYNTH_DIR = "/Users/danielstahl/Documents/Projects/soundmining-modular/src/main/sc/synths"
  val NUM_OUTPUT_BUSES: Int = 16
  //val NUM_OUTPUT_BUSES: Int = 2
  val STATIC_AMP = 1.0

  implicit val client: SuperColliderClient = SuperColliderClient(NUM_OUTPUT_BUSES)

  val majorSidebands = makeFmSynthesis(noteToHertz('c3), noteToHertz('c2), 50)
  val majorSpectrum = majorSidebands.map(_._1)
  val mirroredSpectrum = majorSidebands.map(_._2)

  def init(): Unit = {
        println("Starting up SupderCollider client")
        client.start
        Instrument.setupNodes(client)
        client.send(loadDir(SYNTH_DIR))
  }

  def stop(): Unit = {
      println("Stopping Supercollider client")
      client.stop
  }

  def playChordProgression(): Unit = {
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

    def playChord(chord: (Double, Double), index: Int): Unit = {
      play(index * 13f, 13, sine(0.5, 5, chord._1), (-0.1f, -0.3f))
      play(index * 13f + 5, 13, sine(0.5, 5, chord._2), (0.1f, 0.3f))
    }
  }

  def exposition(startTime: Double = 0)(implicit client: SuperColliderClient): Unit = {
    val time = (majorSpectrum(3) / 1000, majorSpectrum(4) / 1000)
    val (time1, time2) = time

    val pulseStartTime = time1 * 13
    val pulseDuration = time2 * 13 * 13 * 13
    val delayAudioBus = staticAudioBus()
    val delay = monoDelay(delayAudioBus, staticControl(1.0 * STATIC_AMP), delayTime = time2, decayTime = time2 * 13)
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)

    val pan = panning(delay, staticControl(0f))
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)

    pan.getOutputBus.staticBus(client.getRealOutputBus(0))
    val graph = pan.buildGraph(pulseStartTime, pulseDuration, pan.graph(Seq()))
    client.send(client.newBundle(absoluteTimeToMillis(pulseStartTime), graph))

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

    play(rhythm.head, duration.head, simpleFm(0.7f, attack.head, chord1._2, chord1._1, (0, 130), time1 * 8), (-0.7f, 0.7f), 2)
    play(rhythm(1), duration(1), simpleFm(0.5f, attack(1), chord1._1, chord1._2, (0, 130), time1 * 5), (0.7f, -0.7f), 2)
    println(s"exp short pulse start $pulseStartTime")
    play(pulseStartTime, 0.1f, shortFm( 0.7f, 0.01f, chord1._2, chord1._1, delayAudioBus))

    val chord2 = (majorSpectrum(5), majorSpectrum(7))

    play(rhythm(2), duration(2), simpleFm(0.7f, attack(2), chord2._2, chord2._1, (0, 130), time1 * 8), (0.7f, -0.7f), 2)
    play(rhythm(3), duration(3), simpleFm(0.5f, attack(3), chord2._1, chord2._2, (0, 130), time1 * 5), (-0.7f, 0.7f), 2)

    play(rhythm(3), 0.1f, shortFm( 0.7f, 0.01f, chord2._2, chord2._1, delayAudioBus))

    val chord3 = (majorSpectrum(6), majorSpectrum(9))

    play(rhythm(4), duration(4), simpleFm(0.7f, attack(4), chord3._2, chord3._1, (0, 130), time1 * 8), (0.7f, -0.7f), 2)
    play(rhythm(5), duration(5), simpleFm(0.5f, attack(5), chord3._1, chord3._2, (0, 130), time1 * 5), (-0.7f, 0.7f), 2)

    play(rhythm(5), 0.1f, shortFm( 0.7f, 0.01f, chord3._2, chord3._1, delayAudioBus))

    val chord4 = (majorSpectrum(10), majorSpectrum(13))

    play(rhythm(6), duration(6), simpleFm(0.7f, attack(6), chord4._2, chord4._1, (0, 130), time1 * 8), (0.7f, -0.7f), 2)
    play(rhythm(7), duration(7), simpleFm(0.5f, attack(7), chord4._1, chord4._2, (0, 130), time1 * 5), (-0.7f, 0.7f), 2)

    play(rhythm(7), 0.1f, shortFm( 0.7f, 0.01f, chord4._2, chord4._1, delayAudioBus))

    // Second theme
    val subChord1 = (majorSpectrum(11), majorSpectrum(12))

    play(rhythm(8), duration(8), simpleFm(0.4f, attack(8), subChord1._2, subChord1._1, (500, 1500), time2 * 4), (0.2f, -0.2f), 2)
    play(rhythm(9), duration(9), simpleFm(0.7f, attack(9), subChord1._1, subChord1._2, (500, 1500), time2 * 4), (-0.2f, 0.2f), 2)
    play(rhythm(10), duration(10), simpleFm(0.3f, attack(10), subChord1._2, subChord1._1, (500, 1500), time2 * 4), (0.2f, -0.2f), 2)

    val subChord2 = (majorSpectrum(14), majorSpectrum(8))

    play(rhythm(11), duration(11), simpleFm(0.4f, attack(11), subChord2._2, subChord2._1, (500, 1500), time2 * 4), (0.2f, -0.2f), 2)
    play(rhythm(12), duration(12), simpleFm(0.7f, attack(12), subChord2._1, subChord2._2, (500, 1500), time2 * 4), (-0.2f, 0.2f), 2)
    play(rhythm(13), duration(13), simpleFm(0.3f, attack(13), subChord2._2, subChord2._1, (500, 1500), time2 * 4), (0.2f, -0.2f), 2)

    val subChord3 = (majorSpectrum(12), majorSpectrum(7))

    play(rhythm(14), duration(14), simpleFm(0.4f, attack(14), subChord3._2, subChord3._1, (500, 1500), time2 * 4), (0.2f, -0.2f), 2)
    play(rhythm(15), duration(15), simpleFm(0.7f, attack(15), subChord3._1, subChord3._2, (500, 1500), time2 * 4), (-0.2f, 0.2f), 2)
    play(rhythm(16), duration(16), simpleFm(0.3f, attack(16), subChord3._2, subChord3._1, (500, 1500), time2 * 4), (0.2f, -0.2f), 2)

    // Main theme
    val chord5 = (majorSpectrum(10), majorSpectrum(6))

    play(rhythm(17), duration(17), simpleFm(0.7f, attack(17), chord5._2, chord5._1, (0, 130), time1 * 8), (0.7f, -0.7f), 2)
    play(rhythm(18), duration(18), simpleFm(0.5f, attack(18), chord5._1, chord5._2, (0, 130), time1 * 5), (-0.7f, 0.7f), 2)

    play(rhythm(18), 0.1f, shortFm( 0.7f, 0.01f, chord5._2, chord5._1, delayAudioBus))

    val chord6 = (majorSpectrum(8), majorSpectrum(5))

    play(rhythm(19), duration(19), simpleFm(0.7f, attack(19), chord6._2, chord6._1, (0, 130), time1 * 8), (0.7f, -0.7f), 2)
    play(rhythm(20), duration(20), simpleFm(0.5f, attack(20), chord6._1, chord6._2, (0, 130), time1 * 5), (-0.7f, 0.7f), 2)

    play(rhythm(20), 0.1f, shortFm( 0.7f, 0.01f, chord6._2, chord6._1, delayAudioBus))
  }

  def development1(startTime: Double = 0)(implicit client: SuperColliderClient): Unit = {
    val time = (majorSpectrum(3) / 1000f, majorSpectrum(4) / 1000f)
    val (time1, time2) = time

    val chord1 = (majorSpectrum(3), majorSpectrum(4))

    val chord1Sidebands = makeFmSynthesis(chord1._1, chord1._2, 50)
    val chord1Spectrum = majorSidebands.map(_._1)

    val longRhythm = absolute(startTime, Seq(time2 * 21, time2 * 21, time2 * 21, time2 * 21, time2 * 21))
    val longDuration = Seq(time2 * 21, time2 * 21, time2 * 21, time2 * 21)
    val longAttack = Seq(time2 * 13, time2 * 13, time2 * 13, time2 * 13)

    println(s"Development1 longRhythm $longRhythm")
    println(s"Development1 long start ${longDuration.head}")
    
    play(longRhythm.head, longDuration.head, sineRing(1.1f, longAttack.head, chord1Spectrum(8), chord1Spectrum(5)), (0.7f, -0.7f), 0)
    play(longRhythm(1), longDuration(1), sineRing(1.1f, longAttack(1), chord1Spectrum(10), chord1Spectrum(6)), (-0.7f, 0.7f), 0)
    play(longRhythm(2), longDuration(2), sineRing(1.1f, longAttack(2), chord1Spectrum(9), chord1Spectrum(7)), (0.7f, -0.7f), 0)
    play(longRhythm(3), longDuration(3), sineRing(1.1f, longAttack(3), chord1Spectrum(11), chord1Spectrum(8)), (-0.7f, 0.7f), 0)

    val shortRhythm = absolute(startTime, Seq(time1 * 8, time1 * 8, time1 * 8, time1 * 8, time1 * 8, time1 * 8, time1 * 8, time1 * 8, time1 * 8, time1 * 8, time1 * 8, time1 * 8, time1 * 8, time1 * 8, time1 * 8))
    val shortDuration = Seq(time1 * 13, time1 * 21, time1 * 13, time1 * 21, time1 * 13, time1 * 21, time1 * 13, time1 * 21, time1 * 13, time1 * 21, time1 * 13, time1 * 21)
    val shortAttack = Seq(time1 * 8, time1 * 5, time1 * 8, time1 * 5, time1 * 8, time1 * 5, time1 * 8, time1 * 5, time1 * 8, time1 * 5, time1 * 8, time1 * 5)

    println(s"Development1 shortRhythm $shortRhythm")

    println(s"Short track 1 ${shortRhythm.head}")
    println(s"Short track 2 ${shortRhythm(1)}")

    play(shortRhythm.head, shortDuration.head, sineFm(0.5f, shortAttack.head, chord1Spectrum(3), sineModulator(chord1Spectrum(4), (100, 1000), time1 * 8)), (-0.3f, 0.3f), 2)
    play(shortRhythm(1), shortDuration(1), triangleFm(0.50f, shortAttack(1), chord1Spectrum(4), pulseModulator(chord1Spectrum(3), (100, 200), time1 * 5)), (0.3f, -0.3f), 4)

    play(shortRhythm(2), shortDuration(2), sineFm(0.5f, shortAttack(2), chord1Spectrum(5), triangleModulator(chord1Spectrum(7), (100, 1000), time1 * 8)), (0.3f, -0.3f), 2)
    play(shortRhythm(3), shortDuration(3), triangleFm(0.45f, shortAttack(3), chord1Spectrum(7), pulseModulator(chord1Spectrum(5), (100, 200), time1 * 5)), (-0.3f, 0.3f), 4)

    play(shortRhythm(4), shortDuration(4), sineFm(0.5f, shortAttack(4), chord1Spectrum(6), pulseModulator(chord1Spectrum(9), (100, 900), time1 * 5)), (0.3f, -0.3f), 2)
    play(shortRhythm(5), shortDuration(5), triangleFm(0.5f, shortAttack(5), chord1Spectrum(9), sineModulator(chord1Spectrum(6), (100, 900), time1 * 8)), (-0.3f, 0.3f), 4)

    play(shortRhythm(6), shortDuration(6), sineFm(0.5f, shortAttack(6), chord1Spectrum(13), triangleModulator(chord1Spectrum(10), (100, 1000), time1 * 8)), (0.3f, -0.3f), 2)
    play(shortRhythm(7), shortDuration(7), triangleFm(0.5f, shortAttack(7), chord1Spectrum(10), pulseModulator(chord1Spectrum(13), (100, 900), time1 * 5)), (-0.3f, 0.3f), 4)

    play(shortRhythm(8), shortDuration(8), sineFm(0.5f, shortAttack(8), chord1Spectrum(12), sineModulator(chord1Spectrum(11), (100, 1100), time1 * 5)), (0.3f, -0.3f), 2)
    play(shortRhythm(9), shortDuration(9), triangleFm(0.5f, shortAttack(9), chord1Spectrum(11), triangleModulator(chord1Spectrum(12), (100, 1100), time1 * 8)), (-0.3f, 0.3f), 4)

    play(shortRhythm(10), shortDuration(10), sineFm(0.5f, shortAttack(10), chord1Spectrum(14), pulseModulator(chord1Spectrum(8), (100, 1100), time1 * 5)), (0.3f, -0.3f), 2)
    play(shortRhythm(11), shortDuration(11), triangleFm(0.5f, shortAttack(11), chord1Spectrum(8), sineModulator(chord1Spectrum(14), (100, 1100), time1 * 8)), (-0.3f, 0.3f), 4)
  }

  def development2(startTime: Double = 0)(implicit client: SuperColliderClient): Unit = {
    val time = (majorSpectrum(3) / 1000f, majorSpectrum(4) / 1000f)
    val (time1, time2) = time

    val chord1 = (majorSpectrum(11), majorSpectrum(12))

    val spectrum = majorSidebands.map(_._1)

    val rhythm = absolute(startTime, Seq(
      time2 * 8, time2 * 8, time2 * 13,
      time2 * 8, time2 * 8, time2 * 13,
      time2 * 8, time2 * 8, time2 * 13,
      time2 * 8, time2 * 8, time2 * 13))
    val duration = Seq(time2 * 8, time2 * 8, time2 * 8, time2 * 8, time2 * 8, time2 * 8, time2 * 8, time2 * 8, time2 * 8, time2 * 8, time2 * 8, time2 * 8)
    val attack = Seq(time2 * 4, time2 * 4, time2 * 4, time2 * 4, time2 * 4, time2 * 4, time2 * 4, time2 * 4, time2 * 4, time2 * 4, time2 * 4, time2 * 4)

    println(s"Development2 rhythm $rhythm")

    play(rhythm.head, duration.head, sineFm(0.4f, attack.head, spectrum(11), sineModulator(spectrum(12), (100, 1500), time2 * 4)), (-0.4f, -0.2f), 0)
    play(rhythm(1), duration(1), sineFm(0.7f, attack(1), spectrum(12), sineModulator(spectrum(11), (100, 1500), time2 * 4)), (-0.2f, 0.2f), 0)
    play(rhythm(2), duration(2), sineFm(0.3f, attack(2), spectrum(11), sineModulator(spectrum(12), (100, 1500), time2 * 4)), (0.2f, 0.4f), 0)

    play(rhythm(3), duration(3), sineFm(0.4f, attack(3), spectrum(8), triangleModulator(spectrum(14), (100, 1500), time2 * 4)), (0.4f, 0.2f), 0)
    play(rhythm(4), duration(4), sineFm(0.7f, attack(4), spectrum(14), triangleModulator(spectrum(8), (100, 1500), time2 * 4)), (0.2f, -0.2f), 0)
    play(rhythm(5), duration(5), sineFm(0.3f, attack(5), spectrum(8), triangleModulator(spectrum(14), (100, 1500), time2 * 4)), (-0.2f, -0.4f), 0)

    play(rhythm(6), duration(6), sineFm(0.4f, attack(6), spectrum(7), pulseModulator(spectrum(12), (100, 1500), time2 * 4)), (-0.4f, -0.2f), 0)
    play(rhythm(7), duration(7), sineFm(0.7f, attack(7), spectrum(12), pulseModulator(spectrum(7), (100, 1500), time2 * 4)), (-0.2f, 0.2f), 0)
    play(rhythm(8), duration(8), sineFm(0.3f, attack(8), spectrum(7), pulseModulator(spectrum(12), (100, 1500), time2 * 4)), (0.2f, 0.4f), 0)

    play(rhythm(9), duration(9), sineFm(0.4f, attack(9), spectrum(6), triangleModulator(spectrum(10), (100, 1500), time2 * 4)), (0.4f, 0.2f), 0)
    play(rhythm(10), duration(10), sineFm(0.7f, attack(10), spectrum(10), triangleModulator(spectrum(6), (100, 1500), time2 * 4)), (0.2f, -0.2f), 0)
    play(rhythm(11), duration(11), sineFm(0.3f, attack(11), spectrum(6), triangleModulator(spectrum(10), (100, 1500), time2 * 4)), (-0.2f, -0.4f), 0)

    val chord1Sidebands = makeFmSynthesis(chord1._1, chord1._2, 50)
    val chord1Spectrum = chord1Sidebands.map(_._1)

    play(rhythm.head, time2 * 13, ring(chord1Spectrum(2), sineFm(0.4f, attack.head, spectrum(11), sineModulator(spectrum(12), (100, 1500), time2 * 4))), (0.4f, 0.2f), 2)
    play(rhythm(1), time2 * 13, ring(chord1Spectrum(2), sineFm(0.7f, attack(1), spectrum(12), sineModulator(spectrum(11), (100, 1500), time2 * 4))), (0.2f, -0.2f), 2)
    play(rhythm(2), time2 * 13, ring(chord1Spectrum(2), sineFm(0.3f, attack(2), spectrum(11), sineModulator(spectrum(12), (100, 1500), time2 * 4))), (-0.2f, -0.4f), 2)

    val chord2Sidebands = makeFmSynthesis(spectrum(8), spectrum(14), 50)
    val chord2Spectrum = chord2Sidebands.map(_._1)

    play(rhythm(3), time2 * 13, ring(chord2Spectrum(2), sineFm(0.4f, attack(3), spectrum(8), triangleModulator(spectrum(14), (100, 1500), time2 * 4))), (-0.4f, -0.2f), 2)
    play(rhythm(4), time2 * 13, ring(chord2Spectrum(2), sineFm(0.7f, attack(4), spectrum(14), triangleModulator(spectrum(8), (100, 1500), time2 * 4))), (-0.2f, 0.2f), 2)
    play(rhythm(5), time2 * 13, ring(chord2Spectrum(2), sineFm(0.3f, attack(5), spectrum(8), triangleModulator(spectrum(14), (100, 1500), time2 * 4))), (0.2f, 0.4f), 2)

    val chord3Sidebands = makeFmSynthesis(spectrum(7), spectrum(12), 50)
    val chord3Spectrum = chord3Sidebands.map(_._1)

    play(rhythm(6), time2 * 13, ring(chord3Spectrum(2), sineFm(0.4f, attack(6), spectrum(7), pulseModulator(spectrum(12), (100, 1500), time2 * 4))), (0.4f, 0.2f), 2)
    play(rhythm(7), time2 * 13, ring(chord3Spectrum(2), sineFm(0.7f, attack(7), spectrum(12), pulseModulator(spectrum(7), (100, 1500), time2 * 4))), (0.2f, -0.2f), 2)
    play(rhythm(8), time2 * 13, ring(chord3Spectrum(2), sineFm(0.3f, attack(8), spectrum(7), pulseModulator(spectrum(12), (100, 1500), time2 * 4))), (-0.2f, -0.4f), 2)

    val chord4Sidebands = makeFmSynthesis(spectrum(6), spectrum(10), 50)
    val chord4Spectrum = chord4Sidebands.map(_._1)

    play(rhythm(9), time2 * 13, ring(chord4Spectrum(2), sineFm(0.4f, attack(9), spectrum(6), triangleModulator(spectrum(10), (100, 1500), time2 * 4))), (-0.4f, -0.2f), 2)
    play(rhythm(10), time2 * 13, ring(chord4Spectrum(2), sineFm(0.7f, attack(10), spectrum(10), triangleModulator(spectrum(6), (100, 1500), time2 * 4))), (-0.2f, 0.2f), 2)
    play(rhythm(11), time2 * 13, ring(chord4Spectrum(2), sineFm(0.3f, attack(11), spectrum(6), triangleModulator(spectrum(10), (100, 1500), time2 * 4))), (0.2f, 0.4f), 2)

  }

  def development3(startTime: Double = 0f)(implicit client: SuperColliderClient): Unit = {
    val time = (majorSpectrum(3) / 1000f, majorSpectrum(4) / 1000f)
    val (time1, time2) = time

    val pulseDuration = time2 * 13 * 13 * 13

    val delayAudioBus1 = staticAudioBus()
    val delay1 = monoDelay(delayAudioBus1, staticControl(1.0), delayTime = time2, decayTime = time2 * 13)
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)

    val pan1 = panning(delay1, sineControl(staticControl(0.1f), -0.5f, 0.5f))
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)

    pan1.getOutputBus.staticBus(client.getRealOutputBus(0))
    val graph1 = pan1.buildGraph(startTime, pulseDuration, pan1.graph(Seq()))
    client.send(client.newBundle(absoluteTimeToMillis(startTime), graph1))

    val delayAudioBus2 = staticAudioBus()
    val delay2 = monoDelay(delayAudioBus2, staticControl(1.0), delayTime = time1, decayTime = time1 * 13)
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)

    val pan2 = panning(delay2, sineControl(staticControl(0.07f), -0.5f, 0.5f))
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)

    pan2.getOutputBus.staticBus(client.getRealOutputBus(2))
    val graph2 = pan2.buildGraph(startTime, pulseDuration, pan2.graph(Seq()))
    client.send(client.newBundle(absoluteTimeToMillis(startTime), graph2))


    val chord1 = (majorSpectrum(3), majorSpectrum(4))
    val chord1Sidebands = makeFmSynthesis(chord1._1, chord1._2, 50)
    val chord1Spectrum = chord1Sidebands.map(_._1)

    val longRhythm = absolute(startTime,
      Seq(time1 * 13,
        (time1 * 13) + (time1 * 8), time1 * 13,
        (time1 * 13) + (time1 * 8), time1 * 8, time1 * 13, time1 * 13,
        (time1 * 13) + (time1 * 8), time1 * 13,
        (time1 * 13) + (time1 * 8), time1 * 8, (time1 * 13) + (time1 * 8),
        // second theme
        time2 * 8, time2 * 8, time2 * 8 + time2 * 8))

    val shortRhythm = absolute(startTime + (time1 * 13),
      Seq(time1 * 13, time1 * 13, time1 * 13, (time1 * 13) + (time1 * 8), time1 * 8, time1 * 13, time1 * 8, time1 * 8, time1 * 13, time1 * 8,
        time1 * 8, (time1 * 13) + (time1 * 8), time1 * 13))

    println(s"Development3 long rhythm $longRhythm")
    println(s"Development3 short rhythm $shortRhythm")

    play(longRhythm.head, time1 * 13, sineFm(0.7f, time1 * 5, chord1._2, sineModulator(chord1._1, (0, 130), time1 * 8)), (-0.7f, 0.7f), 4)
    play(longRhythm(1), time1 * 13, sineFm(0.5f, time1 * 8, chord1._1, pulseModulator(chord1._2, (150, 400), time1 * 5)), (0.7f, -0.7f), 4)

    println(s"Development3 short start ${shortRhythm.head}")
    play(shortRhythm.head, 0.1f,
      output(sineFm(0.7f, 0.01f, chord1._1, sineModulator(chord1._2, (1300, 1000), 0.01f), Instrument.EXPONENTIAL), delayAudioBus1))

    val chord2 = (majorSpectrum(5), majorSpectrum(7))

    play(longRhythm(2), time1 * 13, sineFm(0.4f, time1 * 5, chord2._2, triangleModulator(chord2._1, (500, 900), time1 * 8)), (-0.7f, 0.7f), 4)
    play(longRhythm(3), time1 * 13,
      sineFm(0.5f, time1 * 8, chord2._1, pulseModulator(chord2._2, (250, 400), time1 * 5)), (0.7f, -0.7f), 4)

    println(s"chord2 $chord2")  
    play(shortRhythm(1), 0.1f,
      output(
        ring(chord1Spectrum(2),
          sineFm(0.7f, 0.01f, chord2._1, sineModulator(chord2._2, (500, 1100), 0.01f), Instrument.EXPONENTIAL)), delayAudioBus2))


    play(shortRhythm(2), 0.1f,
      output(
        ring(chord1Spectrum(3), sineFm(0.7f, 0.01f, chord2._2, triangleModulator(chord2._1, (500, 1100), 0.01f), Instrument.EXPONENTIAL)), delayAudioBus1))


    play(shortRhythm(3), 0.1f,
      output(
        ring(chord1Spectrum(4), triangleFm(0.7f, 0.01f, chord2._1, pulseModulator(chord2._2, (800, 1000), 0.01f), Instrument.EXPONENTIAL)), delayAudioBus2))

    val chord3 = (majorSpectrum(6), majorSpectrum(9))

    play(longRhythm(4), time1 * 13, triangleFm(0.4f, time1 * 5, chord3._2, pulseModulator(chord3._1, (900, 300), time1 * 5)), (-0.7f, 0.7f), 4)
    play(longRhythm(5), time1 * 13, sineFm(0.4f, time1 * 8, chord3._1, sawModulator(chord3._2, (300, 1100), time1 * 8)), (0.7f, -0.7f), 4)

    play(longRhythm(6), time1 * 13,
      ring(chord1Spectrum(2), sineFm(0.4f, time1 * 8, chord3._2, sineModulator(chord3._1, (200, 800), time1 * 8))), (0.3f, -0.3f), 4)


    play(shortRhythm(4), 0.1f,
      output(
        ring(chord1Spectrum(2), triangleFm(0.7f, 0.01f, chord3._1, sineModulator(chord3._2, (900, 1300), 0.01f), Instrument.EXPONENTIAL)), delayAudioBus1))


    play(shortRhythm(5), 0.1f,
      output(
        ring(chord1Spectrum(5), sineFm(0.7f, 0.01f, chord3._2, sawModulator(chord3._1, (1200, 700), 0.01f), Instrument.EXPONENTIAL)), delayAudioBus2))


    val chord4 = (majorSpectrum(10), majorSpectrum(13))

    println(s"Development 3 long track2 start ${longRhythm(7)}")
    play(longRhythm(7), time1 * 13,
      triangleFm(0.4f, time1 * 8, chord4._2, sineModulator(chord4._1, (900, 1100), time1 * 8)), (0.7f, -0.7f), 4)
    play(longRhythm(7), time1 * 13,
      ring(chord1Spectrum(3), triangleFm(0.4f, time1 * 5, chord4._2, sineModulator(chord4._1, (900, 1100), time1 * 5))), (-0.7f, 0.7f), 6)

    play(longRhythm(8), time1 * 13,
      sineFm(0.4f, time1 * 5, chord4._1, pulseModulator(chord4._1, (800, 1200), time1 * 5)), (-0.7f, 0.7f), 4)
    play(longRhythm(8), time1 * 13,
      ring(chord1Spectrum(4), sineFm(0.4f, time1 * 8, chord4._1, sineModulator(chord4._2, (800, 400), time1 * 8))), (0.7f, -0.7f), 6)

    play(shortRhythm(6), 0.1f,
      output(
        ring(chord1Spectrum(6), triangleFm(0.7f, 0.01f, chord4._2, sineModulator(chord4._1, (1400, 700), 0.01f), Instrument.EXPONENTIAL)), delayAudioBus2))

    play(shortRhythm(7), 0.1f,
      output(
        ring(chord1Spectrum(4), sineFm(0.7f, 0.01f, chord4._1, sawModulator(chord4._2, (500, 1900), 0.01f), Instrument.EXPONENTIAL)), delayAudioBus1))

    play(shortRhythm(8), 0.1f,
      output(
        ring(chord1Spectrum(7), triangleFm(0.7f, 0.01f, chord4._2, sineModulator(chord4._1, (2500, 700), 0.01f), Instrument.EXPONENTIAL)), delayAudioBus1))

    play(shortRhythm(9), 0.1f,
      output(
        ring(chord1Spectrum(5), sineFm(0.7f, 0.01f, chord4._1, triangleModulator(chord4._2, (900, 1800), 0.01f), Instrument.EXPONENTIAL)), delayAudioBus2))


    val chord5 = (majorSpectrum(8), majorSpectrum(5))

    play(longRhythm(9), time1 * 13,
      triangleFm(0.4f, time1 * 8, chord5._1, sineModulator(chord5._2, (300, 900), time1 * 8)), (-0.7f, 0.7f), 4)
    play(longRhythm(9), time1 * 13,
      ring(chord1Spectrum(9), sineFm(0.4f, time1 * 8, chord5._1, triangleModulator(chord5._2, (900, 300), time1 * 8))), (0.7f, -0.7f), 6)

    play(longRhythm(10), time1 * 13,
      sineFm(0.4f, time1 * 5, chord4._2, pulseModulator(chord4._1, (600, 900), time1 * 5)), (0.7f, -0.7f), 4)
    play(longRhythm(10), time1 * 13,
      ring(chord1Spectrum(6), sineFm(0.4f, time1 * 5, chord4._2, pulseModulator(chord4._1, (900, 600), time1 * 5))), (-0.7f, 0.7f), 6)

    play(longRhythm(11), time1 * 13,
      ring(chord1Spectrum(8), sineFm(0.3f, time1 * 8, chord4._2, sineModulator(chord4._1, (700, 400), time1 * 8))), (0.5f, -0.5f), 4)


    play(shortRhythm(10), 0.1f,
      output(
        ring(chord1Spectrum(3), triangleFm(0.7f, 0.01f, chord5._2, sawModulator(chord5._1, (500, 900), 0.01f), Instrument.EXPONENTIAL)), delayAudioBus1))

    play(shortRhythm(11), 0.1f,
      output(
        ring(chord1Spectrum(4), sineFm(0.7f, 0.01f, chord5._1, pulseModulator(chord5._2, (1100, 400), 0.01f), Instrument.EXPONENTIAL)), delayAudioBus2))

    play(shortRhythm(12), 0.1f,
      output(
        ring(chord1Spectrum(2), sineFm(0.7f, 0.01f, chord5._2, triangleModulator(chord5._1, (300, 700), 0.01f), Instrument.EXPONENTIAL)), delayAudioBus1))

    // Second theme
    val subChord1 = (majorSpectrum(11), majorSpectrum(12))

    play(longRhythm(12), time2 * 8, simpleFm(0.9f, time2 * 4, subChord1._2, subChord1._1, (500, 1500), time2 * 4), (-0.4f, 0.4f), 4)
    play(longRhythm(12), time2 * 8, ring(chord1Spectrum(6), simpleFm(0.4f, time2 * 4, subChord1._2, subChord1._1, (500, 1500), time2 * 4)), (0.2f, -0.2f), 6)

    play(longRhythm(13), time2 * 8, simpleFm(0.7f, time2 * 4, subChord1._1, subChord1._2, (700, 1900), time2 * 4), (0.4f, -0.4f), 4)
    play(longRhythm(13), time2 * 8, ring(chord1Spectrum(4), simpleFm(0.7f, time2 * 4, subChord1._1, subChord1._2, (700, 1900), time2 * 4)), (-0.2f, 0.2f), 6)

    play(longRhythm(14), time2 * 8, simpleFm(0.8f, time2 * 4, subChord1._2, subChord1._1, (600, 2500), time2 * 4), (-0.4f, 0.4f), 4)
    play(longRhythm(14), time2 * 8, ring(chord1Spectrum(5), simpleFm(0.3f, time2 * 4, subChord1._2, subChord1._1, (600, 2500), time2 * 4)), (0.2f, -0.2f), 6)
  }

  def recapitulation(startTime: Double = 0f)(implicit client: SuperColliderClient): Unit = {
    val time = (majorSpectrum(3) / 1000f, majorSpectrum(4) / 1000f)
    val (time1, time2) = time

    val rhythm = absolute(startTime,
      // Main theme
      Seq(time1 * 13, time1 * 13 + time2 * 5))

    println(s"Recapitulation rhythm $rhythm")

    val pulseStartTime = startTime + (time1 * 13)
    val pulseDuration = time2 * 13 * 13
    val delayAudioBus = staticAudioBus()
    val delay = monoDelay(delayAudioBus, staticControl(1.0), delayTime = time2, decayTime = time2 * 13)
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)

    val pan = panning(delay, staticControl(0f))
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)

    pan.getOutputBus.staticBus(client.getRealOutputBus(0))
    val graph = pan.buildGraph(pulseStartTime, pulseDuration, pan.graph(Seq()))
    client.send(client.newBundle(absoluteTimeToMillis(pulseStartTime), graph))

    // Main theme
    val chord1 = (majorSpectrum(3), majorSpectrum(4))

    play(rhythm.head, time1 * 13, simpleFm(0.7f, time1 * 5, chord1._2, chord1._1, (0, 130), time1 * 8), (-0.7f, 0.7f), 2)
    play(rhythm(1), time1 * 13, simpleFm(0.5f, time1 * 8, chord1._1, chord1._2, (0, 130), time1 * 5), (0.7f, -0.7f), 2)
    println(s"Recapitulation short start $pulseStartTime")
    play(pulseStartTime, 0.1f, shortFm( 0.7f, 0.01f, chord1._2, chord1._1, delayAudioBus))
  }


  def sine(ampValue: Double, attackTime: Double, freq: Double): SineOsc = {
    val amp = percControl(0.001f, ampValue * STATIC_AMP, attackTime, Right(Instrument.LINEAR))
    sineOsc(amp, staticControl(freq)).addAction(TAIL_ACTION)
  }

  def pulse(ampValue: Double, attackTime: Double, freq: Double): PulseOsc = {
    val amp = percControl(0, ampValue * STATIC_AMP, attackTime, Right(Instrument.SINE))
    pulseOsc(amp, staticControl(freq)).addAction(TAIL_ACTION)
  }

  def ring(modFreq: Double, carrier: AudioInstrument): RingModulate = {
    ringModulate(carrier, staticControl(modFreq)).addAction(TAIL_ACTION)
  }

  def ringSine(ampValue: Double, attackTime: Double, modFreq: Double, carrierFreq: Double): RingModulate = {
    val amp = percControl(0.001f, ampValue * STATIC_AMP, attackTime, Right(Instrument.SINE))
    val carrierFreqBus = staticControl(carrierFreq)
    val sineCarrier = sineOsc(amp, carrierFreqBus)
    ringModulate(sineCarrier, staticControl(modFreq)).addAction(TAIL_ACTION)
  }

  def ringTriangle(ampValue: Double, attackTime: Double, modFreq: Double, carrierFreq: Double): RingModulate = {
    val amp = percControl(0.001f, ampValue * STATIC_AMP, attackTime, Right(Instrument.SINE))
    val carrierFreqBus = staticControl(carrierFreq)
    val sineCarrier = sineOsc(amp, carrierFreqBus)
    val triangleCarrier = triangleOsc(amp, carrierFreqBus)
    val xfadeCarrier = xfade(sineCarrier, triangleCarrier, lineControl(1, -1)).addAction(TAIL_ACTION)
    ringModulate(xfadeCarrier, staticControl(modFreq)).addAction(TAIL_ACTION)
  }

  def ringFm(ampValue: Double, carrierFreq: Double, fmModulatorFreq: Double, ringModulatorFreq: Double, attackTime: Double): RingModulate = {
    val amp = percControl(0, ampValue * STATIC_AMP, attackTime, Right(Instrument.SINE))
    val modulator = sineOsc(lineControl(300, 3000), staticControl(fmModulatorFreq))
    val fm = fmSineModulate(staticControl(carrierFreq), modulator, amp).addAction(TAIL_ACTION)
    ringModulate(fm, staticControl(ringModulatorFreq)).addAction(TAIL_ACTION)
  }

  def simpleFm(ampValue: Double, attackTime: Double, modFreq: Double, carrierFreq: Double, modAmount: (Double, Double), modAttackTime: Double): FmModulate = {
    val amp = percControl(0.001f, ampValue * STATIC_AMP, attackTime, Right(Instrument.SINE))
    val modAmountControl = percControl(modAmount._1, modAmount._2, modAttackTime, Right(Instrument.SINE))
    val modulator = sineOsc(modAmountControl, staticControl(modFreq))
    fmSineModulate(staticControl(carrierFreq), modulator, amp).addAction(TAIL_ACTION)
  }

  def pulseModulator(modFreq: Double, modAmount: (Double, Double), modAttackTime: Double): PulseOsc = {
    val modAmountControl = percControl(modAmount._1, modAmount._2, modAttackTime, Right(Instrument.SINE))
    pulseOsc(modAmountControl, staticControl(modFreq))
  }

  def sineModulator(modFreq: Double, modAmount: (Double, Double), modAttackTime: Double): SineOsc = {
    val modAmountControl = percControl(modAmount._1, modAmount._2, modAttackTime, Right(Instrument.SINE))
    sineOsc(modAmountControl, staticControl(modFreq))
  }

  def triangleModulator(modFreq: Double, modAmount: (Double, Double), modAttackTime: Double): TriangleOsc = {
    val modAmountControl = percControl(modAmount._1, modAmount._2, modAttackTime, Right(Instrument.SINE))
    triangleOsc(modAmountControl, staticControl(modFreq))
  }

  def sawModulator(modFreq: Double, modAmount: (Double, Double), modAttackTime: Double): SawOsc = {
    val modAmountControl = percControl(modAmount._1, modAmount._2, modAttackTime, Right(Instrument.SINE))
    sawOsc(modAmountControl, staticControl(modFreq))
  }

  def sineFm(ampValue: Double, attackTime: Double, carrierFreq: Double, modulator: AudioInstrument, attackCurve: EnvCurve = Instrument.SINE): FmSineModulate = {
    val amp = percControl(0.001f, ampValue * STATIC_AMP, attackTime, Right(attackCurve))
    fmSineModulate(staticControl(carrierFreq), modulator, amp).addAction(TAIL_ACTION)
  }

  def triangleFm(ampValue: Double, attackTime: Double, carrierFreq: Double, modulator: AudioInstrument, attackCurve: EnvCurve = Instrument.SINE): FmTriangleModulate = {
    val amp = percControl(0.001f, ampValue * STATIC_AMP, attackTime, Right(attackCurve))
    fmTriangleModulate(staticControl(carrierFreq), modulator, amp).addAction(TAIL_ACTION)
  }

  def pulseFm(ampValue: Double, attackTime: Double, carrierFreq: Double, modulator: AudioInstrument, attackCurve: EnvCurve = Instrument.SINE): FmPulseModulate = {
    val amp = percControl(0.001f, ampValue * STATIC_AMP, attackTime, Right(attackCurve))
    fmPulseModulate(staticControl(carrierFreq), modulator, amp).addAction(TAIL_ACTION)
  }

  def sawFm(ampValue: Double, attackTime: Double, carrierFreq: Double, modulator: AudioInstrument, attackCurve: EnvCurve = Instrument.SINE): FmSawModulate = {
    val amp = percControl(0.001f, ampValue * STATIC_AMP, attackTime, Right(attackCurve))
    fmSawModulate(staticControl(carrierFreq), modulator, amp).addAction(TAIL_ACTION)
  }

  def xfadeModulateFm(ampValue: Double, attackTime: Double, modFreq: Double, carrierFreq: Double, modAmount: (Double, Double), modAttackTime: Double): FmModulate = {
    val amp = percControl(0.001f, ampValue * STATIC_AMP, attackTime, Right(Instrument.SINE))
    val modAmountControl = percControl(modAmount._1, modAmount._2, modAttackTime, Right(Instrument.SINE))
    val modFreqControl = staticControl(modFreq)

    val modulator = xfade(
      sineOsc(modAmountControl, modFreqControl),
      pulseOsc(modAmountControl, modFreqControl),
      lineControl(-1, 1)).addAction(TAIL_ACTION)

    fmSineModulate(staticControl(carrierFreq), modulator, amp).addAction(TAIL_ACTION)
  }

  def shortFm(ampValue: Double, attackTime: Double, modFreq: Double, carrierFreq: Double, output: StaticAudioBusInstrument): FmSineModulate = {
    val amp = percControl(0.001f, ampValue * STATIC_AMP, attackTime, Right(Instrument.EXPONENTIAL))
    val modulator = sineOsc(lineControl(300, 1000), staticControl(modFreq))
    val fmSine = fmSineModulate(staticControl(carrierFreq), modulator, amp)
      .withOutput(output)
      .addAction(TAIL_ACTION)

    fmSine
  }

  def output(audioInstrument: AudioInstrument, output: StaticAudioBusInstrument): AudioInstrument = {
    audioInstrument
      .withOutput(output)
      .addAction(TAIL_ACTION)
    audioInstrument
  }

  def sineRing(ampValue: Double, attackTime: Double, modFreq: Double, carrierFreq: Double): RingModulate = {
    val amp = percControl(0.001f, ampValue * STATIC_AMP, attackTime, Right(Instrument.SINE))
    val carrier = sineOsc(amp, staticControl(carrierFreq)).addAction(TAIL_ACTION)
    ringModulate(carrier, staticControl(modFreq)).addAction(TAIL_ACTION)
  }

  def pulseRing(ampValue: Double, attackTime: Double, modFreq: Double, carrierFreq: Double): RingModulate = {
    val amp = percControl(0.001f, ampValue * STATIC_AMP, attackTime, Right(Instrument.SINE))
    val carrier = pulseOsc(amp, staticControl(carrierFreq)).addAction(TAIL_ACTION)
    ringModulate(carrier, staticControl(modFreq)).addAction(TAIL_ACTION)
  }

  def play(startTime: Double, duration: Double, audio: AudioInstrument, panValue: (Double, Double), staticOutputBus: Int = 0)(implicit client: SuperColliderClient): Unit = {
    val pan = panning(audio, lineControl(panValue._1, panValue._2))
      .addAction(TAIL_ACTION)
    pan.getOutputBus.staticBus(client.getRealOutputBus(staticOutputBus))
    val graph = pan.buildGraph(startTime, duration, pan.graph(Seq()))
    client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
  }

  def play(startTime: Double, duration: Double, audio: AudioInstrument)(implicit client: SuperColliderClient): Unit = {
    val graph = audio.buildGraph(startTime, duration, audio.graph(Seq()))
    client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
  }

  def playExposition(startTime: Double = 0, reset: Boolean = true): Unit = {
    if(reset) client.resetClock
    exposition(startTime)
  }

  def playDevelopment1(startTime: Double = 0, reset: Boolean = true): Unit = {
    if(reset) client.resetClock
    development1(startTime = startTime)
  }

  def playDevelopment2(startTime: Double = 0, reset: Boolean = true): Unit = {
    if(reset) client.resetClock
    development2(startTime = startTime)
  }

  def playDevelopment3(startTime: Double = 0, reset: Boolean = true): Unit = {
    if(reset) client.resetClock
    development3(startTime = startTime)
  }

  def playRecapitulation(startTime: Double = 0, reset: Boolean = true): Unit = {
    if(reset) client.resetClock
    recapitulation(startTime = startTime)
  }

  def playPiece(reset: Boolean = true): Unit = {
    if(reset) client.resetClock

    val time = (majorSpectrum(3) / 1000, majorSpectrum(4) / 1000)
    val (time1, time2) = time
    exposition(0f)
    development1(104.71566)
    development2(141.34325)
    development3(181.76443 + (13 * time1) + (8 * time1))

    recapitulation(255.73912 + (13 * time1) + (8 * time1))
  }

  /*
  def main(args: Array[String]): Unit = {
    val time = (majorSpectrum(3) / 1000f, majorSpectrum(4) / 1000f)
    val (time1, time2) = time

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
    development2(141.34325f)
    development3(181.76443f + (13 * time1) + (8 * time1))

    recapitulation(255.73912f + (13 * time1) + (8 * time1))
  }
*/




}
