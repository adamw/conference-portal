package pl.softwaremill.services

import org.specs.Specification

import bootstrap.liftweb.Boot

import xml._

import pl.softwaremill.lib.D
import pl.softwaremill.model.Conference

import pl.softwaremill.model.TestModelTools._

/**
 * @author Adam Warski (adam at warski dot org)
 */
object SlotServiceTest extends Specification {
  doBeforeSpec { (new Boot()).boot }

  "XHTML table generation" should {
    val slotService = D.inject[SlotService].open_!

    def generateTable(conf: Conference) = {
      slotService.generateTable(conf, room => Text(room.name.is),
        span => Text(span.startTime + " - " + span.endTime), () => EntityRef("nbsp"), slot => Text("full"))
    }

    "return an empty table for an empty conference" >> {
      val result = generateTable(new Conference)
      result must equalIgnoreSpace(<table><tr><th>&nbsp;</th></tr></table>).ordered
    }

    "return a one-row table for a conference with one slot span" >> {
      val conf = new Conference
      conf.addRoom.name("r1")
      conf.addRoom.name("r2")
      conf.addRoom.name("r3")
      conf.addRoom.name("r4")

      addSlot(conf, "08:00", "10:00", "r1")
      addSlot(conf, "08:00", "10:00", "r3")
      addSlot(conf, "08:00", "10:00", "r4")

      val result = generateTable(conf)
      result must equalIgnoreSpace(<table>
        <tr>
          <th>&nbsp;</th><th>r1</th><th>r2</th><th>r3</th><th>r4</th>
        </tr>
        <tr>
          <td>08:00 - 10:00</td><td>full</td><td>&nbsp;</td><td>full</td><td>full</td>
        </tr>
      </table>).ordered
    }

    "return a multi-row table for a conference with multiple slot spans" >> {
      val conf = new Conference
      conf.addRoom.name("r1")
      conf.addRoom.name("r2")
      conf.addRoom.name("r3")
      conf.addRoom.name("r4")

      addSlot(conf, "08:00", "10:00", "r1")
      addSlot(conf, "08:00", "10:00", "r3")
      addSlot(conf, "08:00", "10:00", "r4")

      addSlot(conf, "11:00", "12:00", "r1")
      addSlot(conf, "11:00", "12:00", "r2")

      addSlot(conf, "13:00", "14:00", "r4")
      addSlot(conf, "13:00", "15:00", "r1")

      addSlot(conf, "16:00", "17:00", "r1")
      addSlot(conf, "16:00", "17:00", "r2")

      addSlot(conf, "16:00", "18:00", "r3")

      val result = generateTable(conf)
      result must equalIgnoreSpace(<table>
        <tr>
          <th>&nbsp;</th><th>r1</th><th>r2</th><th>r3</th><th>r4</th>
        </tr>
        <tr><td>08:00 - 10:00</td><td>full</td><td>&nbsp;</td><td>full</td><td>full</td></tr>
        <tr><td>11:00 - 12:00</td><td>full</td><td>full</td><td>&nbsp;</td><td>&nbsp;</td></tr>
        <tr><td>13:00 - 14:00</td><td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td><td>full</td></tr>
        <tr><td>13:00 - 15:00</td><td>full</td><td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td></tr>
        <tr><td>16:00 - 17:00</td><td>full</td><td>full</td><td>&nbsp;</td><td>&nbsp;</td></tr>
        <tr><td>16:00 - 18:00</td><td>&nbsp;</td><td>&nbsp;</td><td>full</td><td>&nbsp;</td></tr>
      </table>).ordered
    }
  }
}