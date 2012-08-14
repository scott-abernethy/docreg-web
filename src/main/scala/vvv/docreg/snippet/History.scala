package vvv.docreg.snippet

import scala.xml.NodeSeq
import net.liftweb.widgets.flot._
import vvv.docreg.model._
import net.liftweb.mapper._
import java.util.{Calendar}
import net.liftweb.common.{Empty, Full}
import org.squeryl.PrimitiveTypeMode._
import java.sql.Timestamp
import net.liftweb.common.Full
import scala.Right
import scala.Some

case class Sample(index: Int, count: Int, label: String)
{
  def incr: Sample = Sample(index, count + 1, label)
}

object Sample
{
  def order(a: Sample, b: Sample): Boolean =
  {
    a.index < b.index
  }
}

object HistoryHelpers {
  val drwClient: (Document, Revision, Project) => Boolean = (_, r, p) => Option(r.clientVersion).map(_.startsWith("dr+w")).getOrElse(false)
}

class History
{
  val defaultOptions: FlotOptions = new FlotOptions {
    override def yaxis = Full(new FlotAxisOptions {
      override def min = Full(0)
    })

    override def legend = Full(new FlotLegendOptions {
      override def position = Full("nw")
    })
  }

  def mini(in: NodeSeq) =
  {
    val history = new MonthHistory();

    val all = new FlotSerie() {
      override val data = history.data( (d,r,p) => UserSession.inStreamFilter(StreamMode.all).apply(d,r,p) )
      override def color = Full(Right(1))
      override def lines = Full(new FlotLinesOptions {
        override def show = Full(true)
        override def fill = Full(true)
      })
    }
    val favourites = new FlotSerie() {
      override val data = history.data( (d,r,p) => UserSession.inStreamFilter(StreamMode.selected).apply(d,r,p) )
      override def color = Full(Right(2))
      override def lines = Full(new FlotLinesOptions {
        override def show = Full(true)
        override def fill = Full(true)
      })
    }
    val watching = new FlotSerie() {
      override val data = history.data( (d,r,p) => UserSession.inStreamFilter(StreamMode.watching).apply(d,r,p) )
      override def color = Full(Right(3))
      override def lines = Full(new FlotLinesOptions {
        override def show = Full(true)
        override def fill = Full(true)
      })
    }
    val me = new FlotSerie() {
      override val data = history.data( (d,r,p) => UserSession.inStreamFilter(StreamMode.me).apply(d,r,p) )
      override def color = Full(Right(4))
      override def lines = Full(new FlotLinesOptions {
        override def show = Full(true)
        override def fill = Full(true)
      })
    }

    val options = new FlotOptions() {
      override def legend = Empty
      override def xaxis = Full(new FlotAxisOptions {
        override protected def buildOptions = c("show", Full(false)) :: super.buildOptions
      })
      override def yaxis = Full(new FlotAxisOptions {
        override protected def buildOptions = c("show", Full(false)) :: super.buildOptions
      })

      override def grid = Full(new FlotGridOptions {
        override def borderWidth = Full(0)

        override def color = Empty
      })
    }

    graph(in, List(all,favourites,watching,me), options)
  }

  def month(in: NodeSeq) =
  {
    val period: MonthHistory = new MonthHistory()
    val combined: FlotSerie = historySeries(period.data(), 1, "All")
    graph(in, List(combined), defaultOptions)
  }

  def year(in: NodeSeq) =
  {
    val period: YearHistory = new YearHistory()
    val combined: FlotSerie = historySeries(period.data(), 1, "All")
    val drwOnly: FlotSerie = historySeries(period.data(HistoryHelpers.drwClient), 5, "Web")
    graph(in, List(combined,drwOnly), defaultOptions)
  }

  def longTerm(in: NodeSeq) =
  {
    val period: LongTermHistory = new LongTermHistory()
    val combined: FlotSerie = historySeries(period.data(), 1, "All")
    val drwOnly: FlotSerie = historySeries(period.data(HistoryHelpers.drwClient).filter(_._2 > 0), 5, "Web")
    graph(in, List(combined,drwOnly), defaultOptions)
  }

  private def historySeries(d: List[(Double, Double)], c: Int, labelText: String): FlotSerie =
  {
    new FlotSerie {
      override val data = d
      override def label = Full(labelText)
      override def color = Full(Right(c))
      override def lines = Full(new FlotLinesOptions {
        override def show = Full(true)
        override def fill = Full(true)
      })
      override def points = Full(new FlotPointsOptions {
        override def show = Full(true)
      })
    }
  }

  private def graph(in: NodeSeq, data_to_plot: List[FlotSerie], flotOptions: FlotOptions): NodeSeq =
  {
    val graphDiv = in \\ "div" filter (_.attribute("class").exists(_.text contains "graph")) headOption
    val graphDivId = graphDiv.flatMap(_.attribute("id").map(_.text))

    in ++ Flot.render(graphDivId.getOrElse("foo"), data_to_plot, flotOptions, Flot.script(in))
  }
}

class MonthHistory
{
  val raw = load()

  def load() = {
    val cal: Calendar = Calendar.getInstance()
    cal.set(Calendar.SECOND, 0)
    cal.set(Calendar.MINUTE, 0)
    cal.set(Calendar.HOUR_OF_DAY, 1)
    cal.getTime
    cal.add(Calendar.DAY_OF_YEAR, -30)
    val startDate = new Timestamp(cal.getTimeInMillis)

    inTransaction{
      join(Document.dbTable, Revision.dbTable, Project.dbTable)( (d,r,p) =>
        where(r.date >= startDate)
          select( (d,r,p) )
          orderBy(r.date asc)
          on(r.documentId === d.id, d.projectId === p.id)
      ).toList
    }
  }

  def data(): List[(Double, Double)] = data((_,_,_) => true)

  def data(accepted: (Document,Revision,Project) => Boolean): List[(Double, Double)] =
  {
    val rs = raw.filter(x => accepted(x._1, x._2, x._3)).map(_._2)
    analyse(Calendar.getInstance, rs).map(s => (s.index.toDouble, s.count.toDouble))
  }

  def analyse(now: Calendar, revisions: Seq[Revision]): List[Sample] =
  {
    var range = Map.empty[(Int,Int), Sample]
    for (i <- List.range(0, 30))
    {
      val d = now.get(Calendar.DAY_OF_YEAR)
      val label = now.get(Calendar.DAY_OF_MONTH)
      val y = now.get(Calendar.YEAR)
      val index: Int = 0 - i
      range = range + ((d,y) -> Sample(index, 0, label.toString))
      now.add(Calendar.DAY_OF_YEAR, -1)
    }

    val cal: Calendar = Calendar.getInstance()
    for (r <- revisions)
    {
      cal.setTime(r.date)
      val d = cal.get(Calendar.DAY_OF_YEAR)
      val y = cal.get(Calendar.YEAR)
      range.get((d,y)) match {
        case Some(sample) =>
          range = range + ((d,y) -> sample.incr)
        case _ =>
      }
    }

    range.values.toList.sortWith(Sample.order)
  }
}

class YearHistory
{
  val raw = load()

  def load() = {
    val cal: Calendar = Calendar.getInstance()
    cal.set(Calendar.SECOND, 0)
    cal.set(Calendar.MINUTE, 0)
    cal.set(Calendar.HOUR_OF_DAY, 0)
    cal.set(Calendar.DAY_OF_MONTH, 1)
    cal.getTime
    cal.add(Calendar.MONTH,-11)
    val startDate = new Timestamp(cal.getTimeInMillis)

    inTransaction{
      join(Document.dbTable, Revision.dbTable, Project.dbTable)( (d,r,p) =>
        where(r.date >= startDate)
          select( (d,r,p) )
          orderBy(r.date asc)
          on(r.documentId === d.id, d.projectId === p.id)
      ).toList
    }
  }

  def data(): List[(Double, Double)] = data((_,_,_) => true)

  def data(accepted: (Document,Revision,Project) => Boolean): List[(Double, Double)] =
  {
    val rs = raw.filter(x => accepted(x._1, x._2, x._3)).map(_._2)
    analyse(Calendar.getInstance, rs).map(s => (s.index.toDouble, s.count.toDouble))
  }

  def analyse(now: Calendar, revisions: Seq[Revision]): List[Sample] =
  {
    var range = Map.empty[Int, Sample]
    for (i <- List.range(0, 12))
    {
      val m = now.get(Calendar.MONTH)
      range = range + (m -> Sample(0 - i, 0, m.toString))
      now.add(Calendar.MONTH, -1)
    }

    val cal: Calendar = Calendar.getInstance()
    for (r <- revisions)
    {
      cal.setTime(r.date)
      val m = cal.get(Calendar.MONTH)
      range.get(m) match {
        case Some(sample) =>
          range = range + (m -> sample.incr)
        case _ =>
      }
    }

    range.values.toList.sortWith(Sample.order)
  }
}

class LongTermHistory
{
  val raw = load()

  def load() = {
    val cal: Calendar = Calendar.getInstance()
    cal.set(Calendar.SECOND, 0)
    cal.set(Calendar.MINUTE, 0)
    cal.set(Calendar.HOUR_OF_DAY, 0)
    cal.set(Calendar.DAY_OF_YEAR, 1)
    cal.getTime
    cal.add(Calendar.YEAR,-15)
    val startDate = new Timestamp(cal.getTimeInMillis)

    inTransaction{
      join(Document.dbTable, Revision.dbTable, Project.dbTable)( (d,r,p) =>
        where(r.date >= startDate)
          select( (d,r,p) )
          orderBy(r.date asc)
          on(r.documentId === d.id, d.projectId === p.id)
      ).toList
    }
  }

  def data(): List[(Double, Double)] = data((_,_,_) => true)

  def data(accepted: (Document,Revision,Project) => Boolean): List[(Double, Double)] =
  {
    val rs = raw.filter(x => accepted(x._1, x._2, x._3)).map(_._2)
    analyse(Calendar.getInstance, rs).map(s => (s.index.toDouble, s.count.toDouble))
  }

  def analyse(now: Calendar, revisions: Seq[Revision]): List[Sample] =
  {
    var range = Map.empty[Int, Sample]
    for (i <- List.range(0, 15))
    {
      val y = now.get(Calendar.YEAR)
      range = range + (y -> Sample(y, 0, y.toString))
      now.add(Calendar.YEAR, -1)
    }

    val cal: Calendar = Calendar.getInstance()
    for (r <- revisions)
    {
      cal.setTime(r.date)
      val m = cal.get(Calendar.YEAR)
      range.get(m) match {
        case Some(sample) =>
          range = range + (m -> sample.incr)
        case _ =>
      }
    }

    range.values.toList.sortWith(Sample.order)
  }
}

trait HistorySerie extends FlotSerie
{
  override def bars = Full(new FlotBarsOptions {
    override def show = Full(true)
  })
}
