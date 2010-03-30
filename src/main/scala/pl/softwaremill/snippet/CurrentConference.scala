package pl.softwaremill.snippet

import xml.{Unparsed, Text, NodeSeq}

import net.liftweb.util.Helpers._
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.sitemap.{Loc, Menu}
import S._

import pl.softwaremill.model._
import pl.softwaremill.lib.D
import pl.softwaremill.services.{UserService, PaperService}
import pl.softwaremill.loc.{Menus, Locs, ViewPaperLoc}

import SnippetTools._

/**
 * @author Adam Warski (adam at warski dot org)
 */
object CurrentConference extends RequestVar(Conference.create)

class CurrentConference {
  def name(ignore: NodeSeq): NodeSeq = Text(CurrentConference.is.name)

  def acceptedPapers(paperTemplate: NodeSeq): NodeSeq = {
    val papers = D.inject_![PaperService].acceptedConferencePapers(CurrentConference.is)

    papers.flatMap(paper => bind("paper", paperTemplate,
      "title" -> paper.title,
      "author" -> paper.author,
      "view" -> anchor(ViewPaperLoc.link.createPath(paper), ?("common.view"))
      ))
  }

  def acceptedAuthors(authorTemplate: NodeSeq): NodeSeq = {
    val authors = D.inject_![UserService].acceptedAuthors(CurrentConference.is)

    authors.flatMap(author => bind("author", authorTemplate,
      "name" -> author.shortName,
      "view" -> anchor(Locs.AuthorLoc.link.createPath(author), ?("common.view"))
      ))
  }
}

class ActiveConference {
  def mainMenu(x: NodeSeq): NodeSeq = {
    def body(menuItem: MenuItem) = {
      menuItem.menuItemType match {
        case MenuItemType.Link => anchor(menuItem.linkContent.is, menuItem.title)
        case MenuItemType.Page => anchor(Locs.CmsLoc.link.createPath(menuItem), menuItem.title)
        case MenuItemType.Manage => anchor(Locs.ManageLoc.link.createPath(()), menuItem.title)
        case _ => anchor("", menuItem.title)
      }
    }

    def testAccessFor(loc: Loc[_]) = loc.testAccess match {
      case Left(true) => true case _ => false
    }

    def additionalChildren(menuItem: MenuItem): Box[NodeSeq] = {
      def testVisible(loc: Loc[_]) = !loc.hidden && testAccessFor(loc)

      def fromMenu(menu: Menu): NodeSeq = {
        (for (loc <- Full(menu.loc) if testVisible(loc);
              link <- loc.createDefaultLink;
              linkText <- loc.linkText) yield <li>{ anchor(link.text, linkText.text) }</li>) openOr NodeSeq.Empty
      }

      def fromMenus(menus: Seq[Menu]): NodeSeq = menus.flatMap(fromMenu(_))

      menuItem.menuItemType match {
        case MenuItemType.Conference => Full(fromMenus(Menus.ConferenceMenus))
        case MenuItemType.User => Full(fromMenus(User.sitemap))
        case _ => Empty
      }
    }

    def visible(menuItem: MenuItem): Boolean = {
      menuItem.menuItemType match {
        case MenuItemType.Manage => testAccessFor(Locs.ManageLoc)
        case _ => true
      }
    }

    (for (conf <- Configuration.is.activeConference;
          mainMenu <- conf.mainMenuItem.obj)
    yield mainMenu.children.flatMap(_.htmlTree(body _, additionalChildren _, visible _,
        List("navigation-2", "navigation-3")))) openOr NodeSeq.Empty
  }
}

object CurrentPaper extends RequestVar(Paper.create)

class CurrentPaper {
  def title(default: NodeSeq): NodeSeq = {
    val paper = CurrentPaper.is
    if (paper.saved_?) Text(CurrentPaper.is.title) else default
  }

  def view(viewTemplate: NodeSeq): NodeSeq = {
    val paper = CurrentPaper.is
    bind("paper", viewTemplate,
      "title" -> paper.title,
      "shortDescription" -> paper.shortDescription.toHtml,
      "author" -> paper.author
      )
  }
}

object CurrentAuthor extends RequestVar(User.create)

class CurrentAuthor {
  def name(ignore: NodeSeq): NodeSeq = Text(CurrentAuthor.is.shortName)

  def bio(ignore: NodeSeq): NodeSeq = CurrentAuthor.is.bio.toHtml

  def acceptedPapers(paperTemplate: NodeSeq): NodeSeq = {
    val papers = D.inject_![PaperService].acceptedConferencePapers(CurrentConference.is, CurrentAuthor.is)

    papers.flatMap(paper => bind("paper", paperTemplate,
      "title" -> paper.title,
      "view" -> anchor(ViewPaperLoc.link.createPath(paper), ?("common.view"))
      ))
  }

  def img(ignore: NodeSeq): NodeSeq = CurrentAuthor.is.faceImageHtml
}

object CurrentMenuItemPage extends RequestVar(MenuItem.create)

class CurrentMenuItemPage {
  def title(ignore: NodeSeq): NodeSeq = Text(CurrentMenuItemPage.is.title)

  def content(ignore: NodeSeq): NodeSeq = Unparsed(CurrentMenuItemPage.is.pageContent)
}