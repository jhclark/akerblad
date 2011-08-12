package akerblad

import akerblad.Types._

import System._
import io._

/** Akerblad Sentence Aligner
 * A Scala port of Champollion
 * Ported by Jonathan Clark */
object Akerblad {
  def main(argArr: Array[String]) = {
    val args = new Args(argArr.toList)
    if(args.opt("-h")) {
      usage("Help:")
    }

    val conf = new Config(args) 

    // TODO: Parse these last 3 filenames a bit better
    // TODO: Write a perl-style getOpts command for Java
    if(argArr.length < 3) {
      usage("ERROR: Not enough arguments")
    }
    val xfnP = args.pos(-3)
    val yfnP = args.pos(-2)
    val alignedFileP = args.pos(-1)

    // TODO: Load here instead of in config?
    // TODO: Make sure there's the same number of files in each list!
    val files: Traversable[(String,String,String)] = {
      if(args.opt("-l")) {
        (Source.fromFile(xfnP).getLines.toList,
         Source.fromFile(yfnP).getLines.toList, 
         Source.fromFile(alignedFileP).getLines.toList
        ).zipped.toList
      } else {
        List( (xfnP, yfnP, alignedFileP) )
      }
    }

    for( (xfn, yfn, alignedFile) <- files) {
      val x = new Document(conf)
      x.load(xfn, conf.xStop)
      val y = new Document(conf)
      x.load(yfn, conf.xStop) // <-- note xStop for target side as well
      val dp = new DocPair(conf, x, y)

      err.println("Aligning sentences in documents %s %s...".format(xfn, yfn))

      val aligner = new Aligner(conf, dp)
      val rAlign: Alignments = aligner.align()
      err.println("done.")

      // If all sentences are translated
      if(conf.alignall) {
	throw new Error("Unsupported.")
      }

      print(conf, rAlign, alignedFile)
    }
    exit(0)
  }

  def print(conf: Config, rAlign: Alignments, aFile: String) = {

    def munge(sents: Seq[Int]) = sents.size match {
      case 0 => "omitted"
      case _ => sents.mkString(",")
    }

    import java.io._
    val out = new PrintWriter(aFile)

    for( (xSents, ySents, score) <- rAlign) {
      val xLine = munge(xSents)
      val yLine = munge(ySents)
      val line = conf.outputScore match {
        case false => "%s <=> %s".format(xLine, yLine)
        case true => "%s <=> %s".format(xLine, yLine, score)
      }
      out.println(line)
    }

    out.close
  }

  def usage(msg: String) = {
    err.println(msg)
    err.println("""
usage: 0 [-hdscn] <X token file> <Y token file> <alignment file>

		
-h       : this (help) message
-d dictf : use dictf as the translation dictionary
-s xstop : use words in file xstop as X stop words
-c n     : number of Y chars for each X char
-n       : disallow 1-3, 3-1, 1-4, 4-1 alignments
-e       : emulate champollion as closely as possible (not recommended)
-S       : output scores next to alignments
-l       : use last 3 arguments as newline separated lists of input files
           (saves time of reloading dict & stopwords)""")
    exit(1)
  }
}
