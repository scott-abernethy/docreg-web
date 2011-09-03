package vvv.docreg.snippet

import scala.xml.NodeSeq
import net.liftweb.widgets.flot._
import vvv.docreg.model.Revision
import net.liftweb.mapper._
import java.util.{Calendar}
import collection.mutable.HashMap
import net.liftweb.common.Full

class History
{
  def month(in: NodeSeq) =
  {
    val data_to_plot = new LineAndPointSerie() {
      override val data = MonthHistory.data()
      override def label = Full("Revisions")
      override def color = Full(Right(1))
    }
    graph(in, data_to_plot)
  }

  def year(in: NodeSeq) =
  {
    val data_to_plot = new LineAndPointSerie() {
      override val data = YearHistory.data()
      override def label = Full("Revisions")
      override def color = Full(Right(2))
    }
    graph(in, data_to_plot)
  }

  def tenYears(in: NodeSeq) =
  {
    val data_to_plot = new LineAndPointSerie() {
      override val data = TenYearHistory.data()
      override def label = Full("Revisions")
      override def color = Full(Right(3))
    }
    graph(in, data_to_plot)
  }

  private def graph(in: NodeSeq, data_to_plot: FlotSerie): NodeSeq =
  {
    val graphDiv = in \\ "div" filter (_.attribute("class").exists(_.text contains "graph")) headOption
    val graphDivId = graphDiv.flatMap(_.attribute("id").map(_.text))

    in ++ Flot.render(graphDivId.getOrElse("foo"), List(data_to_plot), new FlotOptions {}, Flot.script(in))
  }
}

object MonthHistory
{
  def data(): scala.List[(Double, Double)] =
  {
    val cal: Calendar = Calendar.getInstance()
    cal.set(Calendar.DAY_OF_MONTH, 0)
    val endDate = cal.getTime
    cal.add(Calendar.MONTH, -1)
    val startDate = cal.getTime

    val rs = Revision.findAll(
      BySql("DATE_C >= ?", IHaveValidatedThisSQL("me", "now"), startDate),
      BySql("DATE_C < ?", IHaveValidatedThisSQL("me", "now"), endDate),
      OrderBy(Revision.date, Ascending)
    )

    val graphlist = new HashMap[Int, Int]()
    for (r <- rs) {
      cal.setTime(r.date)
      val d = cal.get(Calendar.DAY_OF_MONTH)
      graphlist.put(d, graphlist.getOrElse(d, 0) + 1)
    }

    for (i <- List.range(1, 32))
    yield (i.toDouble, graphlist.getOrElse(i, 0).toDouble)
  }
}

object YearHistory
{
  def data(): scala.List[(Double, Double)] =
  {
    val cal: Calendar = Calendar.getInstance()
    cal.add(Calendar.MONTH,0)
    val endDate = cal.getTime
    cal.add(Calendar.YEAR,-1)
    val startDate = cal.getTime

    val rs = Revision.findAll(
      BySql("DATE_C >= ?", IHaveValidatedThisSQL("me", "now"), startDate),
      BySql("DATE_C < ?", IHaveValidatedThisSQL("me", "now"), endDate),
      OrderBy(Revision.date, Ascending)
    )

    val graphlist = new HashMap[Int, Int]()
     for (r <- rs) {
      cal.setTime(r.date)
      val d = cal.get(Calendar.MONTH) + 1
      graphlist.put(d, graphlist.getOrElse(d, 0) + 1)
    }

    for (i <- List.range(1, 13))
    yield (i.toDouble, graphlist.getOrElse(i, 0).toDouble)
  }
}

object TenYearHistory
{
  def data(): scala.List[(Double, Double)] =
  {
    val cal: Calendar = Calendar.getInstance()
    cal.add(Calendar.YEAR,0)
    val endDate = cal.getTime
    cal.add(Calendar.YEAR,-12)
    val startDate = cal.getTime

    val rs = Revision.findAll(
      BySql("DATE_C >= ?", IHaveValidatedThisSQL("me", "now"), startDate),
      BySql("DATE_C < ?", IHaveValidatedThisSQL("me", "now"), endDate),
      OrderBy(Revision.date, Ascending)
    )

    val graphlist = new HashMap[Int, Int]()
     for (r <- rs) {
      cal.setTime(r.date)
      val d = cal.get(Calendar.YEAR)
      graphlist.put(d, graphlist.getOrElse(d, 0) + 1)
    }

    // todo hardcoded years
    for (i <- List.range(1999, 2012))
    yield (i.toDouble, graphlist.getOrElse(i, 0).toDouble)
  }
}

trait LineAndPointSerie extends FlotSerie
{
  override def lines = Full(new FlotLinesOptions
  {
    override def show = Full(true)
  })
  override def points = Full(new FlotPointsOptions
  {
    override def show = Full(true)
  })
}
