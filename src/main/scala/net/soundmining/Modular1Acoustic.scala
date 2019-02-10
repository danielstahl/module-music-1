package net.soundmining

import net.soundmining.Melody.absolute
import net.soundmining.Note._
import net.soundmining.Spectrum.makeFmSynthesis

/**
  * A variant of M1 for acoustic instruments where
  * we try to translate the electronic music to
  * notated music for string quartet. Don't use
  * quarter note.
  */
object Modular1Acoustic {
  val majorSidebands = makeFmSynthesis(noteToHertz('c3), noteToHertz('c2), 50)
  val majorSpectrum = majorSidebands.map(_._1)
  val mirroredSpectrum = majorSidebands.map(_._2)

  def exposition(startTime: Float = 0f): Unit = {
    val time = (majorSpectrum(3) / 1000f, majorSpectrum(4) / 1000f)
    val (time1, time2) = time
    println(s"Time $time")
    val rhythm = Seq(
      // Main theme
      "1 13", "1 13 + 5",
      "1 13", "1 13 + 5",
      "1 13", "1 13 + 5",
      "1 13", "1 13 + 5",
      // Second theme
      time2 * 8, time2 * 8, time2 * 8 + time2 * 8,
      time2 * 8, time2 * 8, time2 * 8 + time2 * 8,
      time2 * 8, time2 * 8, time2 * 8 + time2 * 8,
      // Main theme
      time1 * 13, time1 * 13 + time2 * 5,
      time1 * 13, time1 * 13 + time2 * 5,
      time1 * 13, time1 * 13 + time2 * 5
    )

    val duration = Seq(
      // Main theme
      "13", "13",
      "13", "13",
      "13", "13",
      "13", "13",
      // Second theme
      time2 * 8, time2 * 8, time2 * 8,
      time2 * 8, time2 * 8, time2 * 8,
      time2 * 8, time2 * 8, time2 * 8,
      // Main theme
      time1 * 13, time1 * 13,
      time1 * 13, time1 * 13,
      time1 * 13, time1 * 13
    )

    // 5/8 1/1
    // 1/1 5/8 5/8 p
    val attack = Seq(
      // Main theme
      "5", "8",
      "5", "8",
      "5", "8",
      "5", "8",
      // Second theme
      time2 * 4, time2 * 4, time2 * 4,
      time2 * 4, time2 * 4, time2 * 4,
      time2 * 4, time2 * 4, time2 * 4,
      // Main theme
      time1 * 5, time1 * 8,
      time1 * 5, time1 * 8,
      time1 * 5, time1 * 8
    )

    val chord1 = (majorSpectrum(3), majorSpectrum(4))
    val fm11 = makeFmSynthesis(chord1._2, chord1._1, 10)
        .map {
          case (s, m) => (hertzToNote(s), hertzToNote(m))
        }
    val fm12 = makeFmSynthesis(chord1._1, chord1._2, 10)
      .map {
        case (s, m) => (hertzToNote(s), hertzToNote(m))
      }

    println(s"time ${rhythm.head} duration ${duration.head} attack ${attack.head} chord $fm11")
    println(s"time ${rhythm(1)} duration ${duration(1)} attack ${attack(1)} chord $fm12")
    println(s"time ${rhythm(1)} duration 0.1, chord chord $fm11")

  }

  def main(args: Array[String]): Unit = {
    exposition()
  }
}
