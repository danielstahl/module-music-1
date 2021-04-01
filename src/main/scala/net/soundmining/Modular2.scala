package net.soundmining

import net.soundmining.synth.Instrument.{EnvCurve, TAIL_ACTION, setupNodes}
import net.soundmining.modular.ModularSynth._
import net.soundmining.modular.ModularInstrument.{AudioInstrument, ControlInstrument}
import net.soundmining.Note.noteToHertz
import net.soundmining.Spectrum.makeFmSynthesis
import net.soundmining.synth.Utils.absoluteTimeToMillis
import Melody._
import net.soundmining.synth._
import scala.util.Random
import Modular1._

object Modular2 {
  val majorSidebands = makeFmSynthesis(noteToHertz('c2), noteToHertz('c2) * Spectrum.phi, 50)
  val majorSpectrum = majorSidebands.map(_._1)
  val mirroredSpectrum = majorSidebands.map(_._2)
  val timeSpectrum = majorSpectrum.map(_ / 1000)
  implicit val r = new scala.util.Random()


  def test1(startTime: Double = 0)(implicit client: SuperColliderClient): Unit = {

    play(startTime, 21,
      ring(staticControl(majorSpectrum(4)),
        ring(staticControl(majorSpectrum(2)),
          xrossfade(
            noise(0.5, 13),
            pulse(0.5, 13, staticControl(majorSpectrum(3))),
            staticControl(-0.7)))),
      (-0.5, 0.5), 0)
  }

  def test2(startTime: Double = 0f)(implicit client: SuperColliderClient): Unit = {

    play(startTime, 21.0,
      ring(staticControl(majorSpectrum(7)),
        ring(staticControl(majorSpectrum(5)),
          moog(noise(0.5, 13), majorSpectrum(6)))),
      (0.5, -0.5), 0)
  }

  def test3(startTime: Double = 0f)(implicit client: SuperColliderClient): Unit = {

    play(startTime, 21,
        moog(dust(4.5, 13, 13), majorSpectrum(9)),
      (0.5, -0.5), 0)

  }

  def test4(startTime: Double = 0)(implicit client: SuperColliderClient): Unit = {

    play(startTime, 21,
      noise(
        threeBlockcontrol(0.001, 3, 0.5, 13, 0.5, 5, 0.001, Right(Instrument.SINE))
      ), (0.5, -0.5), 0)
  }

  def test5(startTime: Double = 0f)(implicit client: SuperColliderClient): Unit = {

    play(startTime, 21,
      ring(majorSpectrum(8),
        ring(majorSpectrum(7),
          resonant(dust(2.0, 13f, 0.85), majorSpectrum(9), 0.56f))),
      (0.5, -0.5), 0)
  }

  def test6(startTime: Double = 0f)(implicit client: SuperColliderClient): Unit = {

    play(startTime, 21,
      ring(staticControl(majorSpectrum(3)),
        ring(staticControl(majorSpectrum(2)),
            pulse(0.5, 13, staticControl(majorSpectrum(1))))),
      (-0.5, 0.5), 0)
  }

  def test7(startTime: Double = 0f)(implicit client: SuperColliderClient): Unit = {

    play(startTime, 21,
      ring(staticControl(majorSpectrum(15)),
        ring(staticControl(majorSpectrum(10)),
          pulseFm(0.5, 13, majorSpectrum(5),
            pulse(0.5, 13, staticControl(majorSpectrum(6)))))),
      (-0.5, 0.5), 0)
  }

  def test8(startTime: Double = 0f)(implicit client: SuperColliderClient): Unit = {

    val volumeControl =
      threeBlockcontrol(0.001, 1, 0.5, 19, 0.5, 1, 0.001, Right(Instrument.SINE))


    play(startTime, 21,
      ring(staticControl(majorSpectrum(4)),
        ring(staticControl(majorSpectrum(2)),
          xrossfade(
            noise(volumeControl),
            pulse(volumeControl, staticControl(majorSpectrum(3))),
            staticControl(-0.7)))),
      threeBlockcontrol(-0.7, 1, 0.2, 19, 0.2, 1, 0.7, Right(Instrument.SINE)), 0)


    val volumeControl2 =
      threeBlockcontrol(0.001, 1, 2.5, 19, 2.5, 1, 0.001, Right(Instrument.SINE))

    play(startTime + 8, 21,
      ring(staticControl(majorSpectrum(7)),
        ring(staticControl(majorSpectrum(5)),
          moog(noise(volumeControl2), majorSpectrum(6)))),
      threeBlockcontrol(0.7, 1, -0.2, 19, -0.2, 1, -0.7, Right(Instrument.SINE)), 0)

    val volumeControl3 =
      threeBlockcontrol(0.001, 1, 4.5, 19, 4.5, 1, 0.001, Right(Instrument.SINE))

    play(startTime + 5, 21,
      moog(dust(volumeControl3, 13), majorSpectrum(9)),
      threeBlockcontrol(0.2, 1, -0.7, 19, -0.7, 1, -0.2, Right(Instrument.SINE)), 0)

    val volumeControl4 =
      threeBlockcontrol(0.001, 1, 0.5, 19, 0.5, 1, 0.001, Right(Instrument.SINE))

    play(startTime + 13, 21,
      ring(staticControl(majorSpectrum(3)),
        ring(staticControl(majorSpectrum(2)),
          pulse(volumeControl4, staticControl(majorSpectrum(1))))),
      threeBlockcontrol(-0.2, 1, 0.7, 19, 0.7, 1, 0.2, Right(Instrument.SINE)), 0)

  }

  def test9(startTime: Double = 0)(implicit client: SuperColliderClient): Unit = {

    /*
    The idea is to spread something like the below gesture an make it "thicker". By making many copies of it and vary
    things like the panning, timing and frequency with small amounts.
    * */


    /*
    these are two different expressions that don't really go well together.
    * */
    val volumeControl =
      threeBlockcontrol(0.001, 1, 0.7, 19, 0.7, 1, 0.001, Right(Instrument.SINE))

    play(startTime, 21,
      ring(staticControl(majorSpectrum(4)),
        ring(staticControl(majorSpectrum(2)),
          xrossfade(
            moog(noise(volumeControl), majorSpectrum(33), 0.5),
            pulse(volumeControl, staticControl(majorSpectrum(3))),
            staticControl(-0.7f)))),
      threeBlockcontrol(-0.7, 1, 0.2, 19, 0.2, 1, 0.7, Right(Instrument.SINE)), 0)

    implicit val r = new scala.util.Random()

    (0 to 5).foreach {
      i =>
        val volumeControl2 =
          threeBlockcontrol(0.001, 1, 0.5, 19, 0.5, 1, 0.001, Right(Instrument.SINE))

        play(startTime + 21, 21,
          ring(staticControl(randomFreq(majorSpectrum(20))),
            ring(staticControl(randomFreq(majorSpectrum(25))),
              xrossfade(
                moog(noise(volumeControl2), randomFreq(majorSpectrum(15)), randomFreq(0.5f)),
                pulse(volumeControl2, staticControl(randomFreq(majorSpectrum(30)))),
                staticControl(randomPan(-0.7))))),
          threeBlockcontrol(randomPan(-0.7), 1, randomPan(0.2), 19, randomPan(0.2), 1, randomPan(0.7), Right(Instrument.SINE)), 0)
    }
  }

  def theme1single(startTime: Double = 0, volume: Double, blocks: (Double, Double, Double), pans: (Double, Double, Double), freqs: (Double, Double, Double, Double), outputBus: Int = 0)(implicit client: SuperColliderClient): Unit = {
    val (block1, block2, block3) = blocks
    val dur = block1 + block2 + block3
    val (pan1, pan2, pan3) = pans
    val (freq1, freq2, freq3, freq4) = freqs

    val volumeControl =
      threeBlockcontrol(0.001f, block1, volume, block2, volume, block3, 0.001f, Right(Instrument.SINE))

    play(startTime, dur,
      ring(staticControl(freq4),
        ring(staticControl(freq3),
          xrossfade(
            moog(noise(volumeControl), freq2, 0.5),
            pulse(volumeControl, staticControl(freq1)),
            staticControl(-0.7f)))),
      threeBlockcontrol(pan1, block1, pan2, block2, pan2, block3, pan3, Right(Instrument.SINE)), outputBus)
  }

  def theme1detune(startTime: Double, duration: Double, volume: Double, attackTime: Double, pans: (Double, Double), freqs: (Double, Double, Double, Double), outputBus: Int = 0)(implicit client: SuperColliderClient): Unit = {
    val (startPan, endPan) = pans
    val (freq1, freq2, freq3, freq4) = freqs

    (0 to 20).foreach {
      i =>
        val volumeControl2 =
          percControl(0.001f, volume, attackTime, Right(Instrument.SINE))

        play(startTime, duration,
          ring(staticControl(randomFreq(freq4)),
            ring(staticControl(randomFreq(freq3)),
              xrossfade(
                moog(noise(volumeControl2), randomFreq(freq2), randomFreq(0.5f)),
                pulse(volumeControl2, staticControl(randomFreq(freq1))),
                staticControl(randomPan(-0.7))))),
          lineControl(randomPan(startPan), randomPan(endPan)), outputBus)
    }
  }

  def theme2single(startTime: Double = 0f, volume: Double, blocks: (Double, Double, Double), pans: (Double, Double, Double), freqs: (Double, Double, Double), outputBus: Int = 0)(implicit client: SuperColliderClient): Unit = {
    val (block1, block2, block3) = blocks
    val dur = block1 + block2 + block3
    val (pan1, pan2, pan3) = pans
    val (freq1, freq2, freq3) = freqs

    val volumeControl =
      threeBlockcontrol(0.001f, block1, volume, block2, volume, block3, 0.001f, Right(Instrument.SINE))

    play(startTime, dur,
      ring(staticControl(freq3),
        ring(staticControl(freq2),
          moog(noise(volumeControl), freq1))),
      threeBlockcontrol(pan1, block1, pan2, block2, pan2, block3, pan3, Right(Instrument.SINE)), outputBus)
  }

  def theme2detune(startTime: Double, duration: Double, volume: Double, attackTime: Double, pans: (Double, Double), freqs: (Double, Double, Double), outputBus: Int = 0)(implicit client: SuperColliderClient): Unit = {
    val (startPan, endPan) = pans
    val (freq1, freq2, freq3) = freqs

    (0 to 20).foreach {
      i =>
        val volumeControl2 =
          percControl(0.001f, volume, attackTime, Right(Instrument.SINE))

        play(startTime, duration,
          ring(staticControl(randomFreq(freq3)),
            ring(staticControl(randomFreq(freq2)),
              moog(noise(volumeControl2), randomFreq(freq1)))),
          lineControl(randomPan(startPan), randomPan(endPan)), outputBus)
    }
  }

  def theme3single(startTime: Double = 0, volume: Double, blocks: (Double, Double, Double), pans: (Double, Double, Double), density: Double, freq: Double, outputBus: Int = 0)(implicit client: SuperColliderClient): Unit = {
    val (block1, block2, block3) = blocks
    val dur = block1 + block2 + block3
    val (pan1, pan2, pan3) = pans

    val volumeControl =
      threeBlockcontrol(0.001, block1, volume, block2, volume, block3, 0.001, Right(Instrument.SINE))

    play(startTime, dur,
      moog(dust(volumeControl, density), freq),
      threeBlockcontrol(pan1, block1, pan2, block2, pan2, block3, pan3, Right(Instrument.SINE)), outputBus)
  }

  def theme3detune(startTime: Double, duration: Double, volume: Double, attackTime: Double, pans: (Double, Double), density: Double, freq: Double, outputBus: Int = 0)(implicit client: SuperColliderClient): Unit = {
    val (startPan, endPan) = pans

    (0 to 20).foreach {
      i =>
        val volumeControl =
          percControl(0.001, volume, attackTime, Right(Instrument.SINE))

        play(startTime, duration,
          moog(dust(volumeControl, density), randomFreq(freq)),
          lineControl(randomPan(startPan), randomPan(endPan)), outputBus)
    }
  }

  def randomFreq(baseFreq: Double)(implicit r: Random): Double =
    (r.nextDouble() * (1.01 - 0.99) + 0.99) * baseFreq

  def randomPan(basePan: Double)(implicit r: Random): Double =
    (r.nextDouble() * (1.05 - 0.95) + 0.95) * basePan

  def randomBetween(random: Random, minInclusive: Double, maxExclusive: Double): Double =
    random.nextDouble() * (maxExclusive - minInclusive) + minInclusive

  def play(startTime: Double, duration: Double, audio: AudioInstrument, panValue: (Double, Double), outputBus: Int)(implicit client: SuperColliderClient): Unit = {
    val pan = panning(audio, lineControl(panValue._1, panValue._2))
      .addAction(TAIL_ACTION)
    pan.getOutputBus.staticBus(client.getRealOutputBus(outputBus))
    val graph = pan.buildGraph(startTime, duration, pan.graph(Seq()))
    client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
  }

  def play(startTime: Double, duration: Double, audio: AudioInstrument, panControl: ControlInstrument, outputBus: Int)(implicit client: SuperColliderClient): Unit = {
    val pan = panning(audio, panControl)
      .addAction(TAIL_ACTION)
    pan.getOutputBus.staticBus(client.getRealOutputBus(outputBus))
    val graph = pan.buildGraph(startTime, duration, pan.graph(Seq()))
    client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
  }

  def ring(modFreq: Double, carrier: AudioInstrument): RingModulate = {
    ringModulate(carrier, staticControl(modFreq)).addAction(TAIL_ACTION)
  }

  def ring(modFreq: ControlInstrument, carrier: AudioInstrument): RingModulate = {
    ringModulate(carrier, modFreq).addAction(TAIL_ACTION)
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
    val amp = percControl(0.001f, ampValue, attackTime, Right(attackCurve))
    fmSineModulate(staticControl(carrierFreq), modulator, amp).addAction(TAIL_ACTION)
  }

  def triangleFm(ampValue: Double, attackTime: Double, carrierFreq: Double, modulator: AudioInstrument, attackCurve: EnvCurve = Instrument.SINE): FmTriangleModulate = {
    val amp = percControl(0.001f, ampValue, attackTime, Right(attackCurve))
    fmTriangleModulate(staticControl(carrierFreq), modulator, amp).addAction(TAIL_ACTION)
  }

  def pulseFm(ampValue: Double, attackTime: Double, carrierFreq: Double, modulator: AudioInstrument, attackCurve: EnvCurve = Instrument.SINE): FmPulseModulate = {
    val amp = percControl(0.001f, ampValue, attackTime, Right(attackCurve))
    fmPulseModulate(staticControl(carrierFreq), modulator, amp).addAction(TAIL_ACTION)
  }

  def sawFm(ampValue: Double, attackTime: Double, carrierFreq: Double, modulator: AudioInstrument, attackCurve: EnvCurve = Instrument.SINE): FmSawModulate = {
    val amp = percControl(0.001f, ampValue, attackTime, Right(attackCurve))
    fmSawModulate(staticControl(carrierFreq), modulator, amp).addAction(TAIL_ACTION)
  }

  def sine(ampValue: Double, attackTime: Double, freq: Double): SineOsc = {
    val amp = percControl(0.001f, ampValue, attackTime, Right(Instrument.SINE))
    sineOsc(amp, staticControl(freq)).addAction(TAIL_ACTION)
  }

  def pulse(ampValue: Double, attackTime: Double, freq: Double): PulseOsc = {
    val amp = percControl(0.001f, ampValue, attackTime, Right(Instrument.SINE))
    pulseOsc(amp, staticControl(freq)).addAction(TAIL_ACTION)
  }

  def pulse(ampValue: Double, attackTime: Double, freq: ControlInstrument): PulseOsc = {
    val amp = percControl(0.001f, ampValue, attackTime, Right(Instrument.SINE))
    pulseOsc(amp, freq).addAction(TAIL_ACTION)
  }

  def pulse(ampControl: ControlInstrument, freq: ControlInstrument): PulseOsc = {
    pulseOsc(ampControl, freq).addAction(TAIL_ACTION)
  }

  def triangle(ampValue: Double, attackTime: Double, freq: Double): TriangleOsc = {
    val amp = percControl(0.001f, ampValue, attackTime, Right(Instrument.SINE))
    triangleOsc(amp, staticControl(freq)).addAction(TAIL_ACTION)
  }

  def triangle(ampValue: Double, attackTime: Double, freq: ControlInstrument): TriangleOsc = {
    val amp = percControl(0.001f, ampValue, attackTime, Right(Instrument.SINE))
    triangleOsc(amp, freq).addAction(TAIL_ACTION)
  }

  def saw(ampValue: Double, attackTime: Double, freq: Double): SawOsc =
    saw(ampValue, attackTime, staticControl(freq))

  def saw(ampValue: Double, attackTime: Double, freq: ControlInstrument): SawOsc = {
    val amp = percControl(0.001f, ampValue, attackTime, Right(Instrument.SINE))
    sawOsc(amp, freq).addAction(TAIL_ACTION)
  }

  def noise(ampValue: Double, attackTime: Double): WhiteNoiseOsc = {
    val amp = percControl(0.001f, ampValue, attackTime, Right(Instrument.SINE))
    whiteNoiseOsc(amp)
  }

  def noise(ampControl: ControlInstrument): WhiteNoiseOsc = {
    whiteNoiseOsc(ampControl).addAction(TAIL_ACTION)
  }

  def dust(ampValue: Double, attackTime: Double, freq: Double): DustOsc = {
    val amp = percControl(0.001f, ampValue, attackTime, Right(Instrument.SINE))
    dustOsc(amp, staticControl(freq)).addAction(TAIL_ACTION)
  }

  def dust(ampControl: ControlInstrument, freq: Double): DustOsc = {
    dustOsc(ampControl, staticControl(freq)).addAction(TAIL_ACTION)
  }

  def xrossfade(in1: AudioInstrument, in2: AudioInstrument): XFade = {
    xfade(in1, in2, lineControl(-1f, 1f)).addAction(TAIL_ACTION)
  }

  def xrossfade(in1: AudioInstrument, in2: AudioInstrument, control: ControlInstrument): XFade = {
    xfade(in1, in2, control).addAction(TAIL_ACTION)
  }

  def moog(in: AudioInstrument, freq: Double, gain: Double = 3.5): MoogFilter = {
    moogFilter(in, staticControl(freq), staticControl(gain)).addAction(TAIL_ACTION)
  }

  def resonant(in: AudioInstrument, freq: Double, gain: Double = 1.0): ResonantFilter = {
    resonantFilter(in, staticControl(freq), staticControl(gain)).addAction(TAIL_ACTION)
  }

  /**
    * Scale parts to a new total
    * Total: 13, Parts: 1,2,5 => 1.625, 3.25, 8.125
    */
  def scale(total: Float, parts: Seq[Double]): Seq[Double] = {
    val fact = total / parts.sum
    parts.map(part => part * fact)
  }

  def line(start: Double, end: Double, steps: Int): Seq[Double] = {
    val total = end - start
    (0 until steps).map(i => start + (i / total))
  }

  def pulse(value: Double, steps: Int): Seq[Double] =
    (0 until steps).map(_ => value)

  def exposition(startTime: Double = 0)(implicit client: SuperColliderClient): Unit = {
    /*
    Regarding rhythm. We could have a very slow ostinato type pulse rhythm that
    stays the same in all parts. Perhaps 3 against 2 or 3 against 4.
    E.g a simple rhythm but that is so slow that you don't recognize it
    immediately. Each part should be quite long, perhaps a couple of
    minutes. We could also use these patterns in the density of the dust.
    E.g a density of 2 4 and 3. We could also have low pulses with freq
    1/2, 1/3 and 1/4
    * */
    println(s"Major spectrum ${majorSpectrum.zipWithIndex}")
    println(s"Major mirrored spectrum ${mirroredSpectrum.zipWithIndex}")
    println(s"Time spectrum ${timeSpectrum.zipWithIndex}")

    val time = majorSpectrum.head
    val threeTime = time / 3
    val fourTime = time / 4
    val twoTime = time / 2

    println(s"time $time fourTime $fourTime threeTime $threeTime twoTime $twoTime")

    /*
    Major spectrum Vector((65.4064,0), (171.23618,1), (277.06598,2), (382.89575,3), (488.72552,4), (594.5553,5), (700.38513,6), (806.21484,7), (912.0447,8), (1017.8744,9), (1123.7042,10), (1229.5339,11), (1335.3638,12), (1441.1935,13), (1547.0233,14), (1652.853,15), (1758.6829,16), (1864.5127,17), (1970.3424,18), (2076.1724,19), (2182.0022,20), (2287.8318,21), (2393.6616,22), (2499.4915,23), (2605.3213,24), (2711.1511,25), (2816.9807,26), (2922.8105,27), (3028.6404,28), (3134.4702,29), (3240.2998,30), (3346.1296,31), (3451.9595,32), (3557.7893,33), (3663.6191,34), (3769.4487,35), (3875.2786,36), (3981.1084,37), (4086.9382,38), (4192.7676,39), (4298.5977,40), (4404.4272,41), (4510.257,42), (4616.087,43), (4721.9165,44), (4827.7466,45), (4933.576,46), (5039.406,47), (5145.236,48), (5251.0654,49))
    Major mirrored spectrum Vector((65.4064,0), (40.423378,1), (146.25316,2), (252.08295,3), (357.91272,4), (463.74252,5), (569.57227,6), (675.4021,7), (781.2318,8), (887.06165,9), (992.8915,10), (1098.7212,11), (1204.551,12), (1310.3807,13), (1416.2106,14), (1522.0403,15), (1627.8701,16), (1733.7,17), (1839.5297,18), (1945.3595,19), (2051.1892,20), (2157.0188,21), (2262.8486,22), (2368.6785,23), (2474.5083,24), (2580.3381,25), (2686.1677,26), (2791.9976,27), (2897.8274,28), (3003.6572,29), (3109.4868,30), (3215.3167,31), (3321.1465,32), (3426.9763,33), (3532.8062,34), (3638.6357,35), (3744.4656,36), (3850.2954,37), (3956.1252,38), (4061.9548,39), (4167.785,40), (4273.6147,41), (4379.4443,42), (4485.2744,43), (4591.104,44), (4696.934,45), (4802.7637,46), (4908.5933,47), (5014.4233,48), (5120.253,49))
    Time spectrum Vector((0.065406404,0), (0.17123617,1), (0.277066,2), (0.38289574,3), (0.4887255,4), (0.5945553,5), (0.70038515,6), (0.80621487,7), (0.9120447,8), (1.0178744,9), (1.1237042,10), (1.2295339,11), (1.3353637,12), (1.4411935,13), (1.5470233,14), (1.652853,15), (1.7586828,16), (1.8645127,17), (1.9703424,18), (2.0761724,19), (2.1820023,20), (2.2878318,21), (2.3936617,22), (2.4994915,23), (2.6053214,24), (2.7111511,25), (2.8169806,26), (2.9228106,27), (3.0286403,28), (3.1344702,29), (3.2402997,30), (3.3461297,31), (3.4519594,32), (3.5577893,33), (3.663619,34), (3.7694488,35), (3.8752785,36), (3.9811084,37), (4.0869384,38), (4.1927676,39), (4.298598,40), (4.404427,41), (4.510257,42), (4.616087,43), (4.7219167,44), (4.8277464,45), (4.933576,46), (5.039406,47), (5.145236,48), (5.2510653,49))
    time 65.4064 fourTime 16.3516 threeTime 21.802134 twoTime 32.7032
    * */

    //val volume = 32
    val volume = 5
    val start1 = startTime

    val theme1chord1 = (majorSpectrum(3), majorSpectrum(33), majorSpectrum(2), majorSpectrum(4))
    val theme1chord2 = (majorSpectrum(15), majorSpectrum(45), majorSpectrum(20), majorSpectrum(25))
    val theme1chord3 = (majorSpectrum(36), majorSpectrum(34), majorSpectrum(43), majorSpectrum(39))

    theme1single(start1, 0.7f * volume, (timeSpectrum(9), time, timeSpectrum(49)), (-0.3f, 0.7f, 0.8f), theme1chord1, 0)
    theme1single(start1, 0.5f * volume, (timeSpectrum(46), time, timeSpectrum(9)), (0.5f, -0.8f, -0.3f), theme1chord2,0)
    theme1single(start1, 0.7f * volume, (timeSpectrum(28), time, timeSpectrum(26)), (0.2f, -0.2f, 0.2f), theme1chord3, 0)

    theme1detune(start1 + threeTime - (timeSpectrum(28) / 2), timeSpectrum(28), 0.3f * volume, timeSpectrum(28) / 2, (0.7, -0.7), theme1chord1, 2)
    theme1detune(start1 + twoTime - (timeSpectrum(47) / 2), timeSpectrum(47), 0.3f * volume, timeSpectrum(47) / 2, (0.2, -0.4), theme1chord2, 2)
    theme1detune(start1 + (threeTime * 2) - (timeSpectrum(49) / 2), timeSpectrum(49), 0.3f * volume, timeSpectrum(49) / 2, (-0.3f, 0.8f), theme1chord3, 2)


    val theme2chord1 = (majorSpectrum(6), majorSpectrum(5), majorSpectrum(7))
    val theme2chord2 = (majorSpectrum(33), majorSpectrum(29), majorSpectrum(37))
    val theme2chord3 = (majorSpectrum(3), majorSpectrum(0), majorSpectrum(5))

    val start2 = start1 + time + timeSpectrum(47) + timeSpectrum(28)

    theme2single(start2, 2.3f * volume, (timeSpectrum(9), time, timeSpectrum(49)), (-0.9, 0.2, 0.9), theme2chord1, 4)
    theme2single(start2, 0.5f * volume, (timeSpectrum(49), time, timeSpectrum(28)), (0.2, -0.5, -0.5), theme2chord2, 4)
    theme2single(start2, 3.3f * volume, (timeSpectrum(28), time, timeSpectrum(46)), (-0.2, 0.7, 0.9), theme2chord3, 4)

    theme2detune(start2 + threeTime - (timeSpectrum(39) / 2), timeSpectrum(39), 0.7 * volume, timeSpectrum(39) / 2, (0.5, -0.2), theme2chord1, 6)
    theme2detune(start2 + twoTime - (timeSpectrum(49) / 2), timeSpectrum(49), 0.2 * volume, timeSpectrum(49) / 2, (0.7, -0.3), theme2chord2, 6)
    theme2detune(start2 + (threeTime * 2) - (timeSpectrum(43) / 2), timeSpectrum(43), 1.7 * volume, timeSpectrum(43) / 2, (0.8, 0.4), theme2chord3, 6)

    val start3 = start2 + time + timeSpectrum(46) + timeSpectrum(28)

    val theme3note1 = majorSpectrum(9)
    val theme3note2 = majorSpectrum(15)
    val theme3note3 = majorSpectrum(3)

    theme3single(start3, 8.9f * volume, (timeSpectrum(28), time, timeSpectrum(47)), (-0.5, 0.6, 0.9), 3, theme3note1, 8)
    theme3single(start3, 8.9f * volume, (timeSpectrum(47), time, timeSpectrum(28)), (0.5, -0.2, -0.5), 5, theme3note2, 8)
    theme3single(start3, 8.9f * volume, (timeSpectrum(49), time, timeSpectrum(49)), (0.2, -0.5, 0.7), 8, theme3note3, 8)

    theme3detune(start3 + threeTime - (timeSpectrum(49) / 2), timeSpectrum(49), 8.9 * volume,  timeSpectrum(49) / 2, (0.8, 0.2), 3, theme3note1, 10)
    theme3detune(start3  + twoTime - (timeSpectrum(47) / 2), timeSpectrum(47), 8.8 * volume, timeSpectrum(47) / 2, (-0.1, -0.7), 5, theme3note2, 10)
    theme3detune(start3 + + (threeTime * 2) - (timeSpectrum(28) / 2), timeSpectrum(28), 8.8 * volume, timeSpectrum(28) / 2, (-0.4, 0.2), 8, theme3note3, 10)
  }

  def development1(startTime: Double = 0)(implicit client: SuperColliderClient): Unit = {
    val time = majorSpectrum.head

    val start1 = startTime
    val longTime = time / 4 //16
    val longHalf = longTime / 2
    val longThree = time / 3
    val longThreeHalf = longThree / 2
    val fiveTime = timeSpectrum(47)
    val threeTime = timeSpectrum(28)

    //val volume = 32
    val volume = 5

    theme1single(start1, 0.8f * volume, (threeTime, longTime, fiveTime), (-0.7, -0.9, -0.6), (majorSpectrum(3), majorSpectrum(33), majorSpectrum(2), majorSpectrum(4)), 0)

    theme1single(start1 + longTime, 0.8 * volume, (fiveTime, longTime, threeTime), (-0.9, -0.8, -0.5), (majorSpectrum(5), majorSpectrum(24), majorSpectrum(1), majorSpectrum(7)), 0)

    theme1detune(start1 + (longTime * 2), longThree, 0.4f * volume, longThreeHalf, (-0.7, -0.3), (majorSpectrum(7), majorSpectrum(39), majorSpectrum(15), majorSpectrum(19)), 2)

    theme1single(start1 + (longTime * 3), 0.8f * volume, (threeTime, longTime, threeTime), (-0.2f, -0.5f, -0.1f), (majorSpectrum(23), majorSpectrum(41), majorSpectrum(25), majorSpectrum(31)), 0)

    theme1single(start1 + (longTime * 4), 0.8f * volume, (fiveTime, longTime, fiveTime), (-0.4f, -0.3f, 0.2f), (majorSpectrum(24), majorSpectrum(43), majorSpectrum(27), majorSpectrum(35)), 0)

    theme1detune(start1 + (longTime * 5), longThree, 0.4f * volume, longThreeHalf, (-0.3f, 0.3f), (majorSpectrum(31), majorSpectrum(46), majorSpectrum(29), majorSpectrum(39)), 2)

    theme1single(start1 + (longTime * 5) + longThreeHalf, 0.8f * volume, (threeTime, longTime, fiveTime), (0.2f, 0.4f, 0.5f), (majorSpectrum(31), majorSpectrum(46), majorSpectrum(29), majorSpectrum(39)), 0)

    theme3detune(start1 + (longTime * 6), longThree, 4.9f * volume, longThreeHalf, (0.9f, 0.7f), 3f, majorSpectrum(39), 6)

    theme3single(start1 + (longTime * 6) + longThreeHalf, 6.9f * volume, (threeTime, longTime, fiveTime), (0.5f, 0.4f, 0.2f), 8f, majorSpectrum(39), 4)

    theme3single(start1 + (longTime * 7), 7.9f * volume, (fiveTime, longTime, fiveTime), (0.2f, -0.3f, -0.4f), 8f, majorSpectrum(27), 4)

    theme3single(start1 + (longTime * 8), 7.9f * volume, (threeTime, longTime, threeTime), (-0.1f, -0.5f, -0.2f), 8f, majorSpectrum(23), 4)

    theme3detune(start1 + (longTime * 9), longThree, 9.9f * volume, longThreeHalf, (-0.3f, -0.7f), 3f, majorSpectrum(15), 6)

    theme3single(start1 + (longTime * 10), 11.9f * volume, (fiveTime, longTime, threeTime), (-0.5f, -0.8f, -0.9f), 8f, majorSpectrum(7), 4)

    theme3single(start1 + (longTime * 11), 12.9f * volume, (threeTime, longTime, fiveTime), (-0.6f, -0.9f, -0.7f), 8f, majorSpectrum(3), 4)
  }

  def development2(startTime: Double = 0f)(implicit client: SuperColliderClient): Unit = {

    val time = majorSpectrum.head
    val longTime = time / 3
    val longHalf = longTime / 2
    val longThree = longTime / 3

    val times = Seq(startTime, startTime + longThree, startTime + longHalf, startTime + (longThree * 2),
      startTime + longTime, startTime + longTime +longThree, startTime + longTime + longHalf, startTime + longTime + (longThree * 2),
      startTime + (longTime * 2), startTime + (longTime * 2) + longThree, startTime + (longTime * 2) + longHalf, startTime + (longTime * 2) +(longThree * 2),
    )

    val longFour = (longTime * 3) / 4
    val fourTimes = Seq(startTime, startTime + longFour, startTime + (longFour * 2), startTime + (longFour * 3))
    println(s"fourTimes $fourTimes")
    println(s"times $times")

    /*
    fourTimes List(0.0, 16.3516, 32.7032, 49.054802)
times List(0.0, 7.267378, 10.901067, 14.534756, 21.802134, 29.069511, 32.7032, 36.33689, 43.604267, 50.871643, 54.505333, 58.139023)
    * */

    val volume = 4

    theme2detune(times.head, timeSpectrum(39), 0.8f * volume, timeSpectrum(39) / 2, (0.5f, -0.2f), (majorSpectrum(6), majorSpectrum(5), majorSpectrum(7)), 0)
    theme2detune(times(1), timeSpectrum(39), 0.8f * volume, timeSpectrum(39) / 2, (-0.5f, 0.2f), (majorSpectrum(33), majorSpectrum(29), majorSpectrum(37)), 0)
    theme2detune(times(2), timeSpectrum(39), 0.8f * volume, timeSpectrum(39) / 2, (0.7f, -0.4f), (majorSpectrum(3), majorSpectrum(0), majorSpectrum(5)), 0)
    theme2detune(times(3), timeSpectrum(39), 0.8f * volume, timeSpectrum(39) / 2, (-0.7f, 0.3f), (majorSpectrum(12), majorSpectrum(10), majorSpectrum(15)), 0)

    theme3detune(fourTimes(1), timeSpectrum(39), 8.9f * volume, timeSpectrum(39) / 2, (0.5f, -0.5f), 3f, majorSpectrum(15), 2)

    theme2detune(times(4), timeSpectrum(39), 0.8f * volume, timeSpectrum(39) / 2, (-0.5f, 0.2f), (majorSpectrum(9), majorSpectrum(7), majorSpectrum(11)), 0)
    theme2detune(times(5), timeSpectrum(39), 0.8f * volume, timeSpectrum(39) / 2, (0.5f, -0.2f), (majorSpectrum(40), majorSpectrum(31), majorSpectrum(49)), 0)
    theme2detune(times(6), timeSpectrum(39), 0.8f * volume, timeSpectrum(39) / 2, (-0.7f, 0.4f), (majorSpectrum(24), majorSpectrum(19), majorSpectrum(27)), 0)
    theme2detune(times(7), timeSpectrum(39), 0.8f * volume, timeSpectrum(39) / 2, (0.7f, -0.3f), (majorSpectrum(10), majorSpectrum(5), majorSpectrum(16)), 0)

    theme2detune(times(8), timeSpectrum(39), 0.8f * volume, timeSpectrum(39) / 2, (0.2f, -0.5f), (majorSpectrum(2), majorSpectrum(5), majorSpectrum(7)), 0)

    theme3detune(fourTimes(3), timeSpectrum(39), 8.9f * volume, timeSpectrum(39) / 2, (-0.5f, 0.5f), 3f, majorSpectrum(39), 2)

    theme2detune(times(9), timeSpectrum(39), 0.8f * volume, timeSpectrum(39) / 2, (-0.2f, 0.5f), (majorSpectrum(35), majorSpectrum(30), majorSpectrum(43)), 0)
    theme2detune(times(10), timeSpectrum(39), 0.8f * volume, timeSpectrum(39) / 2, (0.4f, -0.7f), (majorSpectrum(18), majorSpectrum(31), majorSpectrum(39)), 0)
    theme2detune(times(11), timeSpectrum(39), 0.8f * volume, timeSpectrum(39) / 2, (-0.3f, 0.7f), (majorSpectrum(14), majorSpectrum(21), majorSpectrum(36)), 0)
  }

  def recapitulation(startTime: Double = 0f)(implicit client: SuperColliderClient): Unit = {
    val start1 = startTime
    val time = majorSpectrum.head

    val volume = 5

    val theme1chord1 = (majorSpectrum(3), majorSpectrum(33), majorSpectrum(2), majorSpectrum(4))
    val theme1chord2 = (majorSpectrum(15), majorSpectrum(45), majorSpectrum(20), majorSpectrum(25))
    val theme1chord3 = (majorSpectrum(36), majorSpectrum(34), majorSpectrum(43), majorSpectrum(39))

    theme1single(start1, 0.7f * volume, (timeSpectrum(9), time, timeSpectrum(49)), (-0.3f, 0.7f, 0.8f), theme1chord1, 0)
    theme1single(start1, 0.5f * volume, (timeSpectrum(46), time, timeSpectrum(9)), (0.5f, -0.8f, -0.3f), theme1chord2, 0)
    theme1single(start1, 0.7f * volume, (timeSpectrum(28), time, timeSpectrum(26)), (0.2f, -0.2f, 0.2f), theme1chord3, 0)


    val theme2chord1 = (majorSpectrum(6), majorSpectrum(5), majorSpectrum(7))
    val theme2chord2 = (majorSpectrum(33), majorSpectrum(29), majorSpectrum(37))
    val theme2chord3 = (majorSpectrum(3), majorSpectrum(0), majorSpectrum(5))

    val start2 = start1

    theme2single(start2, 2.5f * volume, (timeSpectrum(9), time, timeSpectrum(49)), (-0.9f, 0.2f, 0.9f), theme2chord1, 2)
    theme2single(start2, 0.7f * volume, (timeSpectrum(49), time, timeSpectrum(28)), (0.2f, -0.5f, -0.5f), theme2chord2, 2)
    theme2single(start2, 3.5f * volume, (timeSpectrum(28), time, timeSpectrum(46)), (-0.2f, 0.7f, 0.9f), theme2chord3, 2)

    val start3 = start2

    val theme3note1 = majorSpectrum(9)
    val theme3note2 = majorSpectrum(15)
    val theme3note3 = majorSpectrum(3)

    theme3single(start3, 8.9f * volume, (timeSpectrum(28), time, timeSpectrum(47)), (-0.5f, 0.6f, 0.9f), 3f, theme3note1, 4)
    theme3single(start3, 8.9f * volume, (timeSpectrum(47), time, timeSpectrum(28)), (0.5f, -0.2f, -0.5f), 5f, theme3note2, 4)
    theme3single(start3, 8.9f * volume, (timeSpectrum(49), time, timeSpectrum(49)), (0.2f, -0.5f, 0.7f), 8f, theme3note3, 4)
  }

  def playExposition(startTime: Double = 0, reset: Boolean = true): Unit = {
    if(reset) client.resetClock
    exposition(startTime)
  }

  def playDevelopment1(startTime: Double = 0, reset: Boolean = true): Unit = {
    if(reset) client.resetClock
    development1(startTime)
  }

  def playDevelopment2(startTime: Double = 0, reset: Boolean = true): Unit = {
    if(reset) client.resetClock
    development2(startTime)
  }

  def playRecapitulation(startTime: Double = 0, reset: Boolean = true): Unit = {
    if(reset) client.resetClock
    recapitulation(startTime)
  }

  /*
  def main(args: Array[String]): Unit = {
    implicit val player: MusicPlayer = MusicPlayer()
    player.startPlay()
    setupNodes(player)


    exposition(0f)

    val time = majorSpectrum.head
    val development1Start = (time + timeSpectrum(47) + timeSpectrum(28)) + (time + timeSpectrum(46) + timeSpectrum(28)) + (time + timeSpectrum(49) + timeSpectrum(49) + timeSpectrum(49))
    development1(development1Start)


    println(s"dev1 start $development1Start")
    val development2Start = development1Start + ((majorSpectrum.head / 4) * 13)

    println(s"dev2 start $development2Start")
    development2(development2Start)

    val recapitulationStart = development2Start + ((majorSpectrum.head / 3) * 3)
    recapitulation(recapitulationStart)
  
    val totalTime = recapitulationStart + majorSpectrum.head + timeSpectrum(9)
    println(s"Total time $totalTime")

  }
  */
}
