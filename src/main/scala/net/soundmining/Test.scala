package net.soundmining

//import net.soundmining.Instrument.{TAIL_ACTION, setupNodes}
//import Instruments._
//import net.soundmining.Utils.absoluteTimeToMillis
//import Note._

object Test {
/*
  def test1(): Unit = {
    implicit val player: MusicPlayer = MusicPlayer()
    player.startPlay()
    setupNodes(player)

    val amp = percControl(0.01f, 1.0f, 1f, Right(Instrument.LINEAR))
    val freq = staticControl(880f)
    val triangle = triangleOsc(amp, freq).addAction(TAIL_ACTION)
    val panValue = lineControl(-1f, 1f)
    val pan = panning(triangle, panValue)
      .addAction(TAIL_ACTION)

    pan.getOutputBus.staticBus(0)

    val graph = pan.buildGraph(0f, 5f, pan.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(0f), graph)
  }

  def test2(): Unit = {
    implicit val player: MusicPlayer = MusicPlayer()
    player.startPlay()
    setupNodes(player)

    val amp = percControl(0.01f, 1.0f, 13f, Right(Instrument.LINEAR))
    val freq = staticControl(110f)
    val triangle = triangleOsc(amp, freq).addAction(TAIL_ACTION)
    val pulse = pulseOsc(amp, freq).addAction(TAIL_ACTION)
    val xfader = xfade(triangle, pulse, lineControl(-1, 1)).addAction(TAIL_ACTION)
    val pan = panning(xfader, lineControl(-1f, 1f))
      .addAction(TAIL_ACTION)

    pan.getOutputBus.staticBus(0)

    val graph = pan.buildGraph(0f, 21f, pan.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(0f), graph)
  }

  def test3(): Unit = {
    implicit val player: MusicPlayer = MusicPlayer()
    player.startPlay()
    setupNodes(player)
    val amp = percControl(0.01f, 1.0f, 3f, Right(Instrument.LINEAR))
    val freq = staticControl(40f)
    val filterFreqBus = lineControl(40f, 5000f)
    val filterGainGus = staticControl(3.5f)
    val pulse = pulseOsc(amp, freq).addAction(TAIL_ACTION)
    val moog = moogFilter(pulse, filterFreqBus, filterGainGus).addAction(TAIL_ACTION)
    val pan = panning(moog, lineControl(-1f, 1f)).addAction(TAIL_ACTION)

    pan.getOutputBus.staticBus(0)

    val graph = pan.buildGraph(0f, 8f, pan.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(0f), graph)
  }

  def test4(): Unit = {
    implicit val player: MusicPlayer = MusicPlayer()
    player.startPlay()
    setupNodes(player)
    val amp = percControl(0.001f, 0.1f, 3f, Right(Instrument.LINEAR))
    val filterFreqBus = lineControl(4000f, 500f)
    val noise = whiteNoiseOsc(amp).addAction(TAIL_ACTION)
    val resonentFilter = resonantFilter(noise, filterFreqBus, staticControl(0.1f)).addAction(TAIL_ACTION)
    val pan = panning(resonentFilter, lineControl(-1f, 1f)).addAction(TAIL_ACTION)
    pan.getOutputBus.staticBus(0)

    val graph = pan.buildGraph(0f, 8f, pan.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(0f), graph)
  }

  def test5(): Unit = {
    implicit val player: MusicPlayer = MusicPlayer()
    player.startPlay()
    setupNodes(player)
    val amp = percControl(0.001f, 1f, 3f, Right(Instrument.LINEAR))
    val freq = staticControl(noteToHertz('d3))
    val modFreq = staticControl(noteToHertz('d4))
    val pulse = pulseOsc(amp, freq).addAction(TAIL_ACTION)
    val ringModulator = ringModulate(pulse, modFreq).addAction(TAIL_ACTION)
    val pan = panning(ringModulator, lineControl(-1f, 1f)).addAction(TAIL_ACTION)
    pan.getOutputBus.staticBus(0)

    val graph = pan.buildGraph(0f, 13f, pan.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(0f), graph)
  }

  def test6(): Unit = {
    implicit val player: MusicPlayer = MusicPlayer()
    player.startPlay()
    setupNodes(player)
    val amp = percControl(0.001f, 1f, 3f, Right(Instrument.LINEAR))
    val freq = staticControl(noteToHertz('g3))
    val modFreq = staticControl(noteToHertz('d4))
    val pulse = pulseOsc(amp, freq).addAction(TAIL_ACTION)
    val amModulator = amModulate(pulse, modFreq).addAction(TAIL_ACTION)
    val pan = panning(amModulator, lineControl(-1f, 1f)).addAction(TAIL_ACTION)
    pan.getOutputBus.staticBus(0)

    val graph = pan.buildGraph(0f, 13f, pan.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(0f), graph)
  }

  def test7(): Unit = {
    implicit val player: MusicPlayer = MusicPlayer()
    player.startPlay()
    setupNodes(player)
    val amp = percControl(0.001f, 1f, 5f, Right(Instrument.LINEAR))
    val carrierFreqBus = staticControl(noteToHertz('a2))
    val modFreq = staticControl(noteToHertz('a2) * 1.5f)
    val modIndex = percControl(300f, 5000f, 5f, Right(Instrument.LINEAR))
    val modulator = sineOsc(modIndex, modFreq).addAction(TAIL_ACTION)
    val fmSine = fmSineModulate(carrierFreqBus, modulator, amp).addAction(TAIL_ACTION)
    val pan = panning(fmSine, lineControl(-0.5f, 0.5f)).addAction(TAIL_ACTION)
    pan.getOutputBus.staticBus(0)

    val graph = pan.buildGraph(0f, 13f, pan.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(0f), graph)
  }


  def test8(): Unit = {
    implicit val player: MusicPlayer = MusicPlayer()
    player.startPlay()
    setupNodes(player)
    val amp = percControl(0.001f, 1f, 6.5f, Right(Instrument.LINEAR))

    val modIndex1 = percControl(300f, 2000f, 8f, Right(Instrument.LINEAR))
    val modulator1 = sineOsc(modIndex1, staticControl(noteToHertz('g6))).addAction(TAIL_ACTION)

    val modIndex2 = percControl(300f, 3000f, 8f, Right(Instrument.LINEAR))
    val modulator2 = sineOsc(modIndex2, staticControl(noteToHertz('aiss5))).addAction(TAIL_ACTION)

    val combinedModulator = mix(modulator1, modulator2).addAction(TAIL_ACTION)

    val carrierFreqBus1 = staticControl(noteToHertz('c5))
    val fmSaw = fmSineModulate(carrierFreqBus1, combinedModulator, amp).addAction(TAIL_ACTION)

    val pan = panning(fmSaw, lineControl(-0.5f, 0.5f)).addAction(TAIL_ACTION)

    pan.getOutputBus.staticBus(0)

    val graph = pan.buildGraph(0f, 13f, pan.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(0f), graph)
  }

  def test9(): Unit = {
    implicit val player: MusicPlayer = MusicPlayer()
    player.startPlay()
    setupNodes(player)
    val amp = percControl(0.001f, 1f, 6.5f, Right(Instrument.LINEAR))

    val modIndex1 = percControl(300f, 2000f, 8f, Right(Instrument.LINEAR))
    val modulator1 = sineOsc(modIndex1, staticControl(noteToHertz('g2))).addAction(TAIL_ACTION)

    val carrierFreqBus1 = staticControl(noteToHertz('c2))
    val fmSine1 = fmSineModulate(carrierFreqBus1, modulator1, amp).addAction(TAIL_ACTION)

    val carrierFreqBus2 = staticControl(noteToHertz('c1))
    val fmSine2 = fmSineModulate(carrierFreqBus2, modulator1, amp).addAction(TAIL_ACTION)

    val combinedFm = mix(fmSine1, fmSine2).addAction(TAIL_ACTION)

    val pan = panning(combinedFm, lineControl(-0.5f, 0.5f)).addAction(TAIL_ACTION)

    pan.getOutputBus.staticBus(0)

    val graph = pan.buildGraph(0f, 13f, pan.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(0f), graph)
  }

  def test10(): Unit = {
    implicit val player: MusicPlayer = MusicPlayer()
    player.startPlay()
    setupNodes(player)
    val amp = percControl(0.001f, 1f, 3f, Right(Instrument.LINEAR))
    val freq = staticControl(noteToHertz('c3))
    val modFreq = staticControl(noteToHertz('d4))
    val pulse = pulseOsc(amp, freq).addAction(TAIL_ACTION)
    val sine = sineOsc(amp, freq).addAction(TAIL_ACTION)
    val xfader = xfade(pulse, sine, lineControl(1, -1)).addAction(TAIL_ACTION)
    val ringModulator = ringModulate(xfader, modFreq).addAction(TAIL_ACTION)

    val xfader2 = xfade(ringModulator, xfader, lineControl(1, -1)).addAction(TAIL_ACTION)
    val pan = panning(xfader2, lineControl(-0.5f, 0.5f)).addAction(TAIL_ACTION)
    pan.getOutputBus.staticBus(0)

    val graph = pan.buildGraph(0f, 13f, pan.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(0f), graph)
  }

  def test11(): Unit = {
    implicit val player: MusicPlayer = MusicPlayer()
    player.startPlay()
    setupNodes(player)

    val modIndex = sineControl(staticControl(0.2f), 300, 3000)

    val graph1 = modIndex.buildGraph(0, 10, modIndex.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(0), graph1)

    def play(startTime: Float, note: Symbol, duration: Float, ampValue: Float): Unit = {
      val amp = relativePercControl(0.001f, ampValue * 0.5f, 0.05f, Right(Instrument.EXPONENTIAL))
      val carrierFreqBus = staticControl(noteToHertz(note))
      val modFreq = staticControl(noteToHertz(note) * 7.5f)
      val modulator = sineOsc(modIndex, modFreq).addAction(TAIL_ACTION)
      val fmSine = fmSineModulate(carrierFreqBus, modulator, amp).addAction(TAIL_ACTION)
      val pan = panning(fmSine, staticControl(0f)).addAction(TAIL_ACTION)
      pan.getOutputBus.staticBus(0)
      val graph = pan.buildGraph(startTime, duration * 0.5f, pan.graph(Seq()))
      player.sendNew(absoluteTimeToMillis(startTime), graph)
    }

    val pattern = 0f until 8f by 0.125f

    val s = 0f

    val dur = 1.5f

    (0f until 10f by 1.0f).foreach(i => {
      play(i, 'a1, dur, 0.75f)
      play(i + pattern(1), 'c2, dur, 0.5f)
      play(i + pattern(2), 'e2, dur, 0.5f)
      play(i + pattern(3), 'c2, dur, 0.5f)

      play(i + pattern(4), 'e2, dur, 0.75f)
      play(i + pattern(5), 'a1, dur, 0.5f)
      play(i + pattern(6), 'c2, dur, 0.5f)
      play(i + pattern(7), 'e2, dur, 0.5f)
    })
  }

  def test12(): Unit = {
    implicit val player: MusicPlayer = MusicPlayer()
    player.startPlay()
    setupNodes(player)

    val amp = percControl(0.01f, 1.0f, 1f, Right(Instrument.LINEAR))
    val freq = sineControl(lineControl(0.2f, 0.1f), 500, 400)
    val triangle = sineOsc(amp, freq).addAction(TAIL_ACTION)
    val panValue = lineControl(-0.5f, 0.5f)
    val pan = panning(triangle, panValue)
      .addAction(TAIL_ACTION)

    pan.getOutputBus.staticBus(0)

    val graph = pan.buildGraph(0f, 13f, pan.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(0f), graph)
  }

  def test13(): Unit = {
    implicit val player: MusicPlayer = MusicPlayer()
    player.startPlay()
    setupNodes(player)

    val amp = percControl(0.01f, 1.0f, 1f, Right(Instrument.LINEAR))
    val freq = controlMix(
      lineControl(200, 500),
      sineControl(lineControl(5f, 6f), 10, 20))

    val triangle = sineOsc(amp, freq).addAction(TAIL_ACTION)
    val panValue = lineControl(-0.5f, 0.5f)
    val pan = panning(triangle, panValue)
      .addAction(TAIL_ACTION)

    pan.getOutputBus.staticBus(0)

    val graph = pan.buildGraph(0f, 13f, pan.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(0f), graph)
  }

  def main(args: Array[String]): Unit = {
    test13()
  }

*/
}
