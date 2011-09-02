package vvv.docreg.snippet

/**
 * Created by IntelliJ IDEA.
 * User: vkunta
 * Date: 22/08/11
 * Time: 8:54 PM
 * To change this template use File | Settings | File Templates.
 */
import scala.xml.NodeSeq
import net.liftweb.util.Helpers._
import net.liftweb.http.js.JsCmds._
import net.liftweb.widgets.flot._
import vvv.docreg.model.Revision
import net.liftweb.mapper._
import java.util.{ArrayList, Calendar, Date}
import bootstrap.liftweb.Boot
import vvv.docreg.db.DbVendor
import collection.mutable.HashMap
import net.liftweb.common.Full

class GraphY {
  def sine(xhtml: NodeSeq) = {
    val data_values: List[(Double, Double)] = GraphY.data()

    val data_to_plot = new FlotSerie() {
      override val data = data_values

      override def bars = Full(new FlotBarsOptions {
      override def show = Full(true)
      })

    override def label = Full("Yearly Revisions")
    }


    Flot.render("graph_area2", List(data_to_plot), new FlotOptions {} , Flot.script(xhtml))
  }
}

object GraphY {
  def data(): scala.List[(Double, Double)] =
  {
    val cal: Calendar = Calendar.getInstance()
    cal.add(Calendar.MONTH,0)
    val endDate = cal.getTime
    cal.add(Calendar.YEAR,-1)
    val startDate = cal.getTime

    println(endDate + " to " + startDate)

    val rs = Revision.findAll(
      BySql("DATE_C >= ?", IHaveValidatedThisSQL("me", "now"), startDate),
      BySql("DATE_C < ?", IHaveValidatedThisSQL("me", "now"), endDate),
      OrderBy(Revision.date, Ascending)
    )

    val graphlist = new HashMap[Int, Int]()
     for (r <- rs) {
      cal.setTime(r.date)
      val d = cal.get(Calendar.MONTH)
      graphlist.put(d, graphlist.getOrElse(d, 0) + 1)
    }




    val data_values: List[(Double, Double)] = for (i <- List.range(1, 12))
    yield (i.toDouble, graphlist.getOrElse(i, 0).toDouble)
    data_values
  }

  def main(args: Array[String])
  {
    DbVendor.init()

    val data_values: scala.List[(Double, Double)] = data()

    println(data_values)
  }
}
