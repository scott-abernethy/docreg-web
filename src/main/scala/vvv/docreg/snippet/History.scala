package vvv.docreg.snippet

import scala.xml.NodeSeq
import net.liftweb.widgets.flot._
import vvv.docreg.model.Revision
import net.liftweb.mapper._
import java.util.{Calendar}
import net.liftweb.common.{Empty, Full}

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

class History
{
  def mini(in: NodeSeq) =
  {
    val series = new FlotSerie() {
      override val data = MonthHistory.data()
      override def color = Full(Right(1))
      override def lines = Full(new FlotLinesOptions {
        override def show = Full(true)
        override def fill = Full(true)
      })
    }
    graph(in, series)
  }

  def month(in: NodeSeq) =
  {
    graph(in, historySeries(MonthHistory.data(), 1))
  }

  def year(in: NodeSeq) =
  {
    graph(in, historySeries(YearHistory.data(), 2))
  }

  def longTerm(in: NodeSeq) =
  {
    graph(in, historySeries(LongTermHistory.data(), 3))
  }

  private def historySeries(d: List[(Double, Double)], c: Int): FlotSerie =
  {
    new FlotSerie {
      override val data = d
      override def label = Full("Revisions")
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

  private def graph(in: NodeSeq, data_to_plot: FlotSerie): NodeSeq =
  {
    val graphDiv = in \\ "div" filter (_.attribute("class").exists(_.text contains "graph")) headOption
    val graphDivId = graphDiv.flatMap(_.attribute("id").map(_.text))

    val flotOptions: FlotOptions = new FlotOptions {
      override def yaxis = Full(new FlotAxisOptions {
        override def min = Full(0)
      })

      override def legend = Full(new FlotLegendOptions {
        override def position = Full("nw")
      })
    }
    in ++ Flot.render(graphDivId.getOrElse("foo"), List(data_to_plot), flotOptions, Flot.script(in))
  }
}

object MonthHistory
{
  def data(): List[(Double, Double)] =
  {
    val cal: Calendar = Calendar.getInstance()
    cal.set(Calendar.SECOND, 0)
    cal.set(Calendar.MINUTE, 0)
    cal.set(Calendar.HOUR_OF_DAY, 1)
    cal.getTime
    cal.add(Calendar.DAY_OF_YEAR, -30)
    val startDate = cal.getTime

    val rs = Revision.findAll(
      BySql("DATE_C >= ?", IHaveValidatedThisSQL("me", "now"), startDate),
      OrderBy(Revision.date, Ascending)
    )

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

object YearHistory
{
  def data(): List[(Double, Double)] =
  {
    val cal: Calendar = Calendar.getInstance()
    cal.set(Calendar.SECOND, 0)
    cal.set(Calendar.MINUTE, 0)
    cal.set(Calendar.HOUR_OF_DAY, 0)
    cal.set(Calendar.DAY_OF_MONTH, 1)
    cal.getTime
    cal.add(Calendar.MONTH,-11)
    val startDate = cal.getTime

    val rs = Revision.findAll(
      BySql("DATE_C >= ?", IHaveValidatedThisSQL("me", "now"), startDate),
      OrderBy(Revision.date, Ascending)
    )

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

object LongTermHistory
{
  def data(): List[(Double, Double)] =
  {
    val cal: Calendar = Calendar.getInstance()
    cal.set(Calendar.SECOND, 0)
    cal.set(Calendar.MINUTE, 0)
    cal.set(Calendar.HOUR_OF_DAY, 0)
    cal.set(Calendar.DAY_OF_YEAR, 1)
    cal.getTime
    cal.add(Calendar.YEAR,-15)
    val startDate = cal.getTime

    val rs = Revision.findAll(
      BySql("DATE_C >= ?", IHaveValidatedThisSQL("me", "now"), startDate),
      OrderBy(Revision.date, Ascending)
    )

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
