package akerblad

import io._
import collection._
import System._

class Args(args: List[String]) {
  def optValue(name: String): String = args.indexOf(name) match {
    case -1 => Akerblad.usage("ERROR: Missing argument: %s".format(name)); throw new Error()
    case idx @ _ => args(idx+1)
  }
  def opt(name: String) = args.contains(name)
  def pos(i: Int) = i match {
    case i if(i < 0) => args(args.length+i)
    case _ => args(i)
  }
}

object StopLoader {
  def load(file: String) = Source.fromFile(file).getLines.map(_.trim).toSet
}

object DictionaryLoader {
  // TODO: SetMultiMap?
  def load(file: String, xstop: Set[String]): Map[String, Iterable[String]] = {
    val dict = new mutable.HashMap[String, mutable.ArrayBuffer[String]]
    for(line <- Source.fromFile(file).getLines) {
      // TODO: non-regex split
      val Array(x, y) = line.split(" <> ")
      if(!xstop(x)) {
        dict.getOrElseUpdate(x.trim.toLowerCase, new mutable.ArrayBuffer) += y.trim.toLowerCase
      }
    }
    dict
  }
}

class Config(args: Args) {

  val fast = args.opt("-f")
  val emulate = args.opt("-e")
  val disallow3 = args.opt("-n")
  val alignall = args.opt("-a")
  val outputScore = args.opt("-S")
  val x2yCharRatio = args.optValue("-c").toDouble
  val dictFile = args.optValue("-d")
  val xStopFile = args.optValue("-s")

  val MINS = -10
  
  val WIN_PER_100 = 8
  val MIN_WIN_SIZE = 10
  val MAX_WIN_SIZE = 100
  val DEBUG = true

  val xStop = StopLoader.load(xStopFile)
  // TODO: Replace with a MultiMap?
  val dict: Map[String,Iterable[String]] = DictionaryLoader.load(dictFile, xStop)

  if(DEBUG) {
    err.println("fast = " + fast);
    err.println("emulate = " + emulate);
    err.println("disallow3 = " + disallow3);
    err.println("alignall = " + alignall);
    err.println("xtoyc = " + x2yCharRatio);
  }

  // replaces penalty01, etc.
  // TODO: Read from config?
  val penalty = Array.fill(5,5)(0.0) // 2d array of 5x5
  penalty(0)(0) = 0.0
  penalty(0)(1) = 0.8
  penalty(2)(1) = 0.95
  penalty(1)(2) = 0.95
  penalty(2)(2) = 0.85
  penalty(3)(1) = 0.94
  penalty(1)(3) = 0.94
  penalty(4)(1) = 0.92 // 0.85
  penalty(1)(4) = 0.92 // 0.85
  // assumed (not in perl)
  penalty(2)(3) = 0.0
  penalty(3)(2) = 0.0
  penalty(3)(3) = 0.0

  val scorer = new Scorer()
}
