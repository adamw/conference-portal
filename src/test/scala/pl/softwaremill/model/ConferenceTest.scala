package pl.softwaremill.model

import org.specs.Specification
import bootstrap.liftweb.Boot

import TestModelTools._

/**
 * @author Adam Warski (adam at warski dot org)
 */
object ConferenceTest extends Specification {
  doBeforeSpec { (new Boot()).boot }

  "A Conference" should {
    "have no rooms when created" >> {
      (new Conference).rooms.size must_== 0
    }
  }

  "The add operation" should {
    val conf = new Conference

    "correctly assign indexes for the first room" >> {
      conf.addRoom
      conf.rooms.size must_== 1
      checkRoomPositions(conf)
    }

    "correctly assign indexes for three rooms" >> {
      conf.addRoom
      conf.addRoom
      conf.addRoom
      conf.rooms.size must_== 3
      checkRoomPositions(conf)
    }
  }

  "The delete operation" should {
    val conf = new Conference

    "delete the specified room" >> {
      conf.addRoom.name("r1")
      conf.addRoom.name("r2")
      conf.addRoom.name("r3")
      conf.addRoom.name("r4")

      conf.deleteRoom(conf.rooms(1))

      conf.rooms.size must_== 3
      conf.rooms(0).name must_== "r1"
      conf.rooms(1).name must_== "r3"
      conf.rooms(2).name must_== "r4"
    }

    "correctly re-assign indexes for the first room" >> {
      conf.addRoom
      conf.addRoom
      conf.addRoom

      conf.deleteRoom(conf.rooms.first)

      conf.rooms.size must_== 2
      checkRoomPositions(conf)
    }

    "correctly re-assign indexes for the last room" >> {
      conf.addRoom
      conf.addRoom
      conf.addRoom

      conf.deleteRoom(conf.rooms.last)

      conf.rooms.size must_== 2
      checkRoomPositions(conf)
    }

    "correctly re-assign indexes for a middle room" >> {
      conf.addRoom
      conf.addRoom
      conf.addRoom
      conf.addRoom

      conf.deleteRoom(conf.rooms(1))

      conf.rooms.size must_== 3
      checkRoomPositions(conf)
    }

    "correctly re-assign indexes after a move up" >> {
      conf.addRoom
      conf.addRoom
      conf.addRoom
      conf.addRoom

      conf.moveUp(conf.rooms(2))
      conf.deleteRoom(conf.rooms(1))

      conf.rooms.size must_== 3
      checkRoomPositions(conf)
    }
  }

  "The move up operation" should {
    val conf = new Conference
    conf.addRoom.name("r1")
    conf.addRoom.name("r2")
    conf.addRoom.name("r3")
    conf.addRoom.name("r4")

    def moveUpExample(index: Int, expectedNames: Array[String])() = {
      "correctly move up room %d".format(index) in {
        conf.moveUp(conf.rooms(index))
        checkRoomNames(conf, expectedNames: _*)
        checkRoomPositions(conf)
      }
    }

    moveUpExample(0, Array("r1", "r2", "r3", "r4"))
    moveUpExample(1, Array("r2", "r1", "r3", "r4"))
    moveUpExample(2, Array("r1", "r3", "r2", "r4"))
    moveUpExample(3, Array("r1", "r2", "r4", "r3"))
  }

  "The move down operation" should {
    val conf = new Conference
    conf.addRoom.name("r1")
    conf.addRoom.name("r2")
    conf.addRoom.name("r3")
    conf.addRoom.name("r4")

    def moveDownExample(index: Int, expectedNames: Array[String])() = {
      "correctly move down room %d".format(index) in {
        conf.moveDown(conf.rooms(index))
        checkRoomNames(conf, expectedNames: _*)
        checkRoomPositions(conf)
      }
    }

    moveDownExample(0, Array("r2", "r1", "r3", "r4"))
    moveDownExample(1, Array("r1", "r3", "r2", "r4"))
    moveDownExample(2, Array("r1", "r2", "r4", "r3"))
    moveDownExample(3, Array("r1", "r2", "r3", "r4"))
  }

  "The move up and down operations" should {
    val conf = new Conference
    val r1 = conf.addRoom.name("r1")
    val r2 = conf.addRoom.name("r2")
    val r3 = conf.addRoom.name("r3")
    val r4 = conf.addRoom.name("r4")

    "correctly move up and down rooms multiple times" >> {
      // Moving r4 to the top
      conf.moveUp(r4)
      conf.moveUp(r4)
      conf.moveUp(r4)

      // Moving r1 to the bottom
      conf.moveDown(r1)
      conf.moveDown(r1)

      // Swapping r2 and r3
      conf.moveUp(r3)

      checkRoomPositions(conf)
      checkRoomNames(conf, "r4", "r3", "r2", "r1")
    }
  }

  def checkRoomPositions(conf: Conference) {
    for (i <- 0 until conf.rooms.size) conf.rooms(i).position must_== i
  }

  def checkRoomNames(conf: Conference, names: String*) {
    conf.rooms.size must_== names.size
    conf.rooms.toList.zip(names.toList).foreach({ case (r: Room, n: String) => r.name.is must_== n })
  }

  "Slot validation" should {
    def slotsOverlap(e1: SlotSpan, e2: SlotSpan, roomDesc: RoomDesc) =
      "%s %s %s %s %s".format(e1.startTime, e1.endTime, e2.startTime, e2.endTime, roomDesc.name)

    "reject duplicate slots" >> {
      val conf = new Conference
      conf.addRoom.name("r1")
      conf.addRoom.name("r2")

      addSlot(conf, "08:00", "10:00", "r1")
      addSlot(conf, "11:00", "12:00", "r1")
      addSlot(conf, "08:00", "10:00", "r2")
      addSlot(conf, "08:00", "10:00", "r2")
      addSlot(conf, "13:00", "14:00", "r2")

      val result = conf.validateSlots(slotsOverlap _)

      result must haveSize(1)
      result must contain("08:00 10:00 08:00 10:00 r2")
    }

    "reject single overlapping slots" >> {
      val conf = new Conference
      conf.addRoom.name("r1")
      conf.addRoom.name("r2")

      addSlot(conf, "08:00", "10:00", "r1")
      addSlot(conf, "09:00", "12:00", "r1")

      val result = conf.validateSlots(slotsOverlap _)

      result must haveSize(1)
      result must contain("08:00 10:00 09:00 12:00 r1")
    }

    "accept slots with matching start end times" >> {
      val conf = new Conference
      conf.addRoom.name("r1")
      conf.addRoom.name("r2")

      addSlot(conf, "08:00", "10:00", "r1")
      addSlot(conf, "10:00", "12:00", "r1")

      val result = conf.validateSlots(slotsOverlap _)

      result must haveSize(0)
    }

    "reject all overlapping slots" >> {
      val conf = new Conference
      conf.addRoom.name("r1")
      conf.addRoom.name("r2")
      conf.addRoom.name("r3")

      addSlot(conf, "15:00", "17:00", "r3")
      addSlot(conf, "08:00", "10:00", "r1")
      addSlot(conf, "11:00", "12:00", "r1")
      addSlot(conf, "08:00", "10:00", "r2")
      addSlot(conf, "09:59", "12:00", "r2")
      addSlot(conf, "13:00", "14:00", "r2")
      addSlot(conf, "16:00", "18:00", "r3")

      val result = conf.validateSlots(slotsOverlap _)

      result must haveSize(2)
      result must contain("08:00 10:00 09:59 12:00 r2")
      result must contain("15:00 17:00 16:00 18:00 r3")
    }
  }
}
