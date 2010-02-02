package pl.softwaremill.lib

import net.liftweb._
import http._
import pl.softwaremill.services._
import reflect.Manifest

/**
 * A factory for generating new instances of Date.  You can create
 * factories for each kind of thing you want to vend in your application.
 * An example is a payment gateway.  You can change the default implementation,
 * or override the default implementation on a session, request or current call
 * stack basis.
 */
object D extends Factory {
  implicit object conferenceService extends FactoryMaker[ConferenceService](() => new ConferenceServiceImpl)
  implicit object slotService extends FactoryMaker[SlotService](() => new SlotServiceImpl)
  implicit object paperService extends FactoryMaker[PaperService](() => new PaperServiceImpl)
  implicit object userService extends FactoryMaker[UserService](() => new UserServiceImpl)

  /**
   * objects in Scala are lazily created.  The init()
   * method creates a List of all the objects.  This
   * results in all the objects getting initialized and
   * registering their types with the dependency injector
   */
  private def init() {
    List(conferenceService, slotService, paperService, userService)
  }
  init()

  def inject_![T](implicit man: Manifest[T]) = inject[T](man).open_!
}

/*
/**
 * Examples of changing the implementation
 */
sealed abstract class Changer {
  def changeDefaultImplementation() {
    D.time.default.set(() => new Date())
  }

  def changeSessionImplementation() {
    D.time.session.set(() => new Date())
  }

  def changeRequestImplementation() {
    D.time.request.set(() => new Date())
  }

  def changeJustForCall(d: Date) {
    D.time.doWith(d) {
      // perform some calculations here
    }
  }
}
*/
