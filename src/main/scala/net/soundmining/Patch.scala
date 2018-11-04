package net.soundmining

/**
  * A patch is a number of templates for instruments and how they are connected.
  */

class Dur(val value: Float) extends AnyVal

class Out(val value: Int) extends AnyVal

abstract class Patch {

  /*
  (
var amp = Bus.control(s, 1), panBus = Bus.control(s, 1), freqBus = Bus.control(s, 1), sig = Bus.audio(s, 1), dur = 5;
Synth(\percControl, [\out, amp, \dur, dur, \attack, 1, \startValue, 0.001, \peakValue, 1, \curve, \lin]);
Synth(\lineControl, [\out, panBus, \dur, dur, \startValue, -1, \endValue, 1]);
Synth(\staticControl, [\out, freqBus, \dur, dur, \value, 880]);
Synth(\triangleOsc, [\out, sig, \dur, dur, \ampBus, amp, \freqBus, freqBus], addAction:\addToTail);
Synth(\pan, [\in, sig, \panBus, panBus, \out, 0, \dur, dur], addAction:\addToTail);
)
   */



  def build(args: Map[String, String])(implicit dur: Dur, out: Out): Unit = {

    // attackpoint, start and peak.
    // absolute is in absolute time and relative is in relative time
    // Default (apply) is relative
    //val percControl = PercControl.absolute(1, 0.001, 1).curve('lin)
    //val percControl = PercControl.relative(0.1, 0.001, 1).curve('lin)
    //val triangle = Triangle(880, percControl)
    //val pan = Pan(LineControl(-1, 1)).in(triangle)

  }

  def play(args: Map[String, String])
}
