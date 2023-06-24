val home = System.getProperty("user.home")

val src = s"${home}/projects/notes/journals"

import scala.io.Source
import scala.language.reflectiveCalls
import java.io.PrintWriter

object Control {
  def using[A <: {def close(): Unit}, B](resource: A)(f: A => B): B =
    try {
      f(resource)
    } finally {
      resource.close()
    }
}

"a::".split(':')

case class DailyReport(
                        Workplace: String,
                        Hours: String,
                        Day: String,
                        KeyActivities: String,
                        Notes: String,
                        KeyResults: String,
                      )


val files = java.io.File(src).listFiles()

def extractData(lines: List[String], lbl: String): String = {
  val l = lines.find(l => l.contains(lbl))
  if (l.nonEmpty) {
    val str = l.get
    val idx = str.indexOf(lbl)
    str.substring(idx + lbl.length).trim
  }
  else ""
}


val data = files.map(f => {
  Control.using(Source.fromFile(f.getPath))(
    source => {
      val lines = source.getLines().filter(_.contains("::")).toList
      if (lines.nonEmpty && lines.exists(l => l.contains("Tags:: DailyReport"))) {
        DailyReport(
          extractData(lines, "Workplace::"),
          extractData(lines, "Hours::"),
          extractData(lines, "Day::").substring(2, 12),
          extractData(lines, "Key-Activities::"),
          extractData(lines, "Notes::"),
          extractData(lines, "Key-Results::")
        )
      }
      else {
        DailyReport("", "", f.getName.dropRight(3).replace('_', '-'), "", "", "")
      }
    }
  )
}).toList.sortBy(d => d.Day)

// write a copy of data
Control.using(new PrintWriter(s"${home}/projects/DailyReport.csv")) {
  pw =>
    pw.write("Day,Workplace,Hours,Activities,Results,Notes\n")
    data.foreach(d => pw.write(s"${d.Day},${d.Workplace},${d.Hours},${d.KeyActivities},${d.KeyResults},${d.Notes}\n"))
}


///Query home hours and office hours
val summary = data.filter(r => r.Workplace.length > 1)
  .flatMap(r => {
    val wp = r.Workplace.split("[&+-]")
      .map(_.trim.toLowerCase.filter(c => c != '.'))
    val hr = r.Hours.split("[&+]")
    wp.zip(hr)
  })
  .groupBy(d => d._1)
  .map(g => (g._1, g._2.map(d => d._2.toDouble).sum))

data.filter(r => r.Workplace.length > 1).minBy(d => d.Day).Day
data.filter(r => r.Workplace.length > 1).maxBy(d => d.Day).Day

summary.foreach(println)