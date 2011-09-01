package me.winslow.d.mn2gt
import driver._

import scala.swing._
import javax.swing

object GUI extends SwingApplication {

  import GridBagPanel._

  sealed trait OperationMode
  object Local extends OperationMode
  object Remote extends OperationMode

  object State {
    var mapnikFile: Option[java.io.File] = None
    var outputDir: Option[java.io.File] = None
    var mode: OperationMode = Local
    var geoserverConnection: Option[GeoServerConnection] = None

    def isValid =
      mode match {
        case Local =>
          mapnikFile.isDefined && outputDir.isDefined
        case Remote =>
          mapnikFile.isDefined && geoserverConnection.isDefined
      }

    def job: Option[Operation] =
      mode match {
        case Local =>
          for {
            mf <- mapnikFile
            od <- outputDir
          } yield LocalConversion(mf, od)
        case Remote =>
          for {
            mf <- mapnikFile
            conn <- geoserverConnection
          } yield PublishToGeoServer(mf, conn)
      }
  }

  case class ConnectionSpecified(conn: GeoServerConnection)
    extends scala.swing.event.Event

  class InputBox extends GridBagPanel {
    val mapnikFileLabel = new Label("Mapnik XML")
    val mapnikFileField = new TextField(30)
    val fileChooserLauncher = new Button("Open...")

    border = new swing.border.TitledBorder("Input")
    enabled = false
    layout += (
      mapnikFileLabel -> ((0, 0): Constraints),
      mapnikFileField -> ((1, 0): Constraints),
      fileChooserLauncher -> ((2, 0): Constraints)
    )
  }

  class OperationBox extends GridBagPanel {
    val local = new RadioButton("Just save SLD files to local disk")
    val publish = new RadioButton("Really publish layers to GeoServer")
    val group = new ButtonGroup(local, publish)

    border = new swing.border.TitledBorder("Operation")
    layout += (
      local -> new Constraints {
        grid = (0, 0)
        anchor = Anchor.West
      },
      publish -> new Constraints {
        grid = (0, 1)
        anchor = Anchor.West
      }
    )
  }

  class OutputBox extends GridBagPanel {
    val outputDirLabel = new Label("SLD directory")
    val outputDirField = new TextField(30)
    val fileChooserLauncher = new Button("Open...")

    border = new swing.border.TitledBorder("Local Output")

    override def enabled_=(b: Boolean) = {
      super.enabled = b
      outputDirLabel.enabled = b
      outputDirField.enabled = b
      fileChooserLauncher.enabled = b
    }

    layout += (
      outputDirLabel -> ((0, 0): Constraints),
      outputDirField -> ((1, 0): Constraints),
      fileChooserLauncher -> ((2, 0): Constraints)
    )
  }

  class GeoServerBox extends GridBagPanel {
    val urlLabel = new Label("Server URL")
    val urlField =
      new TextField("http://localhost:8080/geoserver/", 30)
    val adminLabel = new Label("Admin user")
    val adminField = new TextField("admin", 30)
    val passwordLabel = new Label("Password")
    val passwordField = new PasswordField(30)
    val datadirLabel = new Label("Data directory")
    val datadirField = new TextField("/var/lib/geoserver-data", 30)
    val nsPrefixLabel = new Label("Namespace Prefix")
    val nsPrefixField = new TextField("mn2gt", 30)
    val nsUriLabel = new Label("Namespace URI")
    val nsUriField = new TextField("http://example.com/mn2gt/", 30)

    private val fields = 
      Seq(
        urlField,
        adminField,
        passwordField,
        datadirField,
        nsPrefixField,
        nsUriField,
        urlLabel,
        adminLabel,
        passwordLabel,
        datadirLabel,
        nsPrefixLabel,
        nsUriLabel
      )

    listenTo(
      urlField,
      adminField,
      passwordField,
      datadirField,
      nsPrefixField,
      nsUriField
    )

    reactions += {
      case event.EditDone(_) =>
        for {
          url <- Some(urlField.text) filter(_ nonEmpty)
          user <- Some(adminField text) filter(_ nonEmpty)
          password <- Some(passwordField password) filter(_ nonEmpty)
          datadir <- Some(datadirField text) filter(_ nonEmpty)
          nsPrefix <- Some(nsPrefixField text) filter(_ nonEmpty)
          nsUri <- Some(nsUriField text) filter(_ nonEmpty)
        } publish(ConnectionSpecified(GeoServerConnection(
            url, user, password.mkString, datadir, nsPrefix, nsUri
          )))
      case _ => ()
    }

    override def enabled_=(b: Boolean) {
      super.enabled = b
      fields.foreach(_.enabled = b)
    }

    border = new swing.border.TitledBorder("GeoServer")
    layout += (
      urlLabel -> ((0, 0): Constraints),
      urlField -> ((1, 0): Constraints),
      adminLabel -> ((0, 1): Constraints),
      adminField -> ((1, 1): Constraints),
      passwordLabel -> ((0, 2): Constraints),
      passwordField -> ((1, 2): Constraints),
      datadirLabel -> ((0, 3): Constraints),
      datadirField -> ((1, 3): Constraints),
      nsPrefixLabel -> ((0, 4): Constraints),
      nsPrefixField -> ((1, 4): Constraints),
      nsUriLabel -> ((0, 5): Constraints),
      nsUriField -> ((1, 5): Constraints)
    )
  }

  class CommitBox extends GridBagPanel {
    val go = Action("Convert!") {
      Dialog.showMessage(
        parent = this,
        message = "Hello",
        title = "Greeting"
      )
    }
    val button = new Button(go)

    override def enabled_=(b: Boolean) = {
      super.enabled = b
      button.enabled = b
    }

    layout += (
      new Component {}  -> new Constraints {
        grid = (0, 0)
        weightx = 1
        fill = Fill.Both
      },
      button -> new Constraints {
        grid = (1, 0)
        anchor = Anchor.LastLineEnd
      }
    )
  }
  
  def startup(args: Array[String]) {
    locally {
      import javax.swing.UIManager.{ getInstalledLookAndFeels, setLookAndFeel }
      import collection.JavaConversions._
      for (nimbus <- getInstalledLookAndFeels.find(_.getName == "Nimbus"))
        setLookAndFeel(nimbus.getClassName)
    }

    val input = new InputBox
    val operation = new OperationBox
    val output = new OutputBox
    val geoserver = new GeoServerBox
    val commit = new CommitBox 

    def enableAppropriateControls = {
      commit.enabled = State.isValid
      output.enabled = (State.mode == Local)
      geoserver.enabled = (State.mode == Remote)
    }

    locally {
      import scala.swing.event._

      input.mapnikFileField.reactions += {
        case ValueChanged(c: TextField) =>
          State.mapnikFile = Some(new java.io.File(c.text))
          enableAppropriateControls
        case x => println(x)
      }

      operation.local.reactions += {
        case ButtonClicked(_) => 
          State.mode = Local
          enableAppropriateControls
        case _ => ()
      }
      
      output.outputDirField.reactions += {
        case ValueChanged(c: TextField) =>
          State.outputDir = Some(new java.io.File(c.text))
          enableAppropriateControls
        case _ => ()
      }

      operation.publish.reactions += {
        case ButtonClicked(_) =>
          State.mode = Remote
          enableAppropriateControls
        case _ => ()
      }

      geoserver.reactions += {
        case ConnectionSpecified(conn) =>
          State.geoserverConnection = Some(conn)
          enableAppropriateControls
        case _ => ()
      }

      commit.button.reactions += {
        case ButtonClicked(_) =>
          State.job foreach { job =>
            actors.Futures.future {
              job.run()
            }
          }
      }
    }

    operation.local.selected = true
    enableAppropriateControls

    val frame = new MainFrame
    locally { import frame._
      title = "Mapnik → GeoServer Importer"
      visible = true
      contents = {
        val grid = new BoxPanel(Orientation.Vertical)
        grid.contents += (
          input,
          operation,
          output,
          geoserver,
          commit
        )

        grid
      }
    }
  }
}
