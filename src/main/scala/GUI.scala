package me.winslow.d.mn2gt

object GUI extends swing.SwingApplication {
  class FileField extends {
    val textField = new swing.TextField() {
      preferredSize = new swing.Dimension(120, preferredSize.height)
    }
    val fileChooser = new swing.FileChooser()
    val chooserButton = new swing.Button("Choose...") 
  } with swing.BoxPanel(swing.Orientation.Horizontal) {
    contents += (textField, chooserButton)
    chooserButton.action = swing.Action("Choose...") {
      import swing.FileChooser.Result._
      fileChooser.showOpenDialog(chooserButton) match {
        case Approve =>
          textField.text = fileChooser.selectedFile.getAbsolutePath
      }
    }
  }

  locally {
    import javax.swing.UIManager.{ getInstalledLookAndFeels, setLookAndFeel }
    import collection.JavaConversions._
    for (nimbus <- getInstalledLookAndFeels.find(_.getName == "Nimbus"))
      setLookAndFeel(nimbus.getClassName)
  }

  val sourceLabel = new swing.Label("Mapnik XML")
  val outputLabel = new swing.Label("Output (SLD files)")
  val source = new FileField
  val output = new FileField
  output.fileChooser.fileSelectionMode =
    swing.FileChooser.SelectionMode.DirectoriesOnly
  val runButton = new swing.Button("Convert!")
  runButton.enabled = false

  locally {
    import swing.Reactions.Reaction
    import swing.event.ValueChanged
    val handler: Reaction = {
      case ValueChanged(f) =>
        runButton.enabled =
          source.textField.text.nonEmpty &&
          output.textField.text.nonEmpty
      case _ => ()
    }
    source.textField.subscribe(handler)
    output.textField.subscribe(handler)
  }

  val sldConversionForm = new swing.GridBagPanel {
    import swing.GridBagPanel.{ Anchor, Fill }
    layout(sourceLabel) = new Constraints {
      grid = (0, 0)
      anchor = Anchor.West
    }
    layout(outputLabel) = new Constraints {
      grid = (0, 2)
      anchor = Anchor.West
    }
    layout(source) = new Constraints { 
      grid = (0, 1)
      fill = Fill.Horizontal
      weightx = 1d
    }
    layout(output) = new Constraints { 
      grid = (0, 3)
      fill = Fill.Horizontal
      weightx = 1d
    }
    layout(runButton) = new Constraints {
      grid = (0, 4)
      anchor = Anchor.LastLineEnd
    }
  }

  val geoserverConfigurationForm = {
    import swing.event._
    import util.control.Exception.allCatch

    val geoserverUrl = new swing.Label("URL")
    val dataDir = new swing.Label("Data Directory (on Server)")
    val userName = new swing.Label("Admin User")
    val passwd = new swing.Label("Password")
    val nsPrefix = new swing.Label("Namespace Prefix")
    val nsURI = new swing.Label("Namespace URI")

    val geoserverUrlField = new swing.TextField(30) {
      inputVerifier = _ => allCatch.opt(new java.net.URL(text)).isDefined
    }
    
    val dataDirField = new swing.TextField(30)
    val userNameField = new swing.TextField(30)
    val passwdField = new swing.PasswordField(30)
    val nsPrefixField = new swing.TextField(30)
    val nsURIField = new swing.TextField(30) {
      inputVerifier = { _ =>
        allCatch.opt(new java.net.URL(text))
          .forall(_.getHost() != "localhost")
      }
    }
    
    val runButton = new swing.Button("Configure!!") {
      enabled = false 
      
      listenTo(geoserverUrlField, dataDirField, userNameField, passwdField,
        nsPrefixField, nsURIField)

      reactions += {
        case EditDone(_) =>
          @inline def isUrl(x: swing.TextField): Boolean = 
            try {
              new java.net.URL(x.text); true
            } catch {
              case (_: java.net.MalformedURLException) => false
            }
            
          val freeText =
            Seq(dataDirField, userNameField, nsPrefixField)
          enabled = (
            freeText.forall(_.text.nonEmpty) & 
            passwdField.password.nonEmpty &
            isUrl(geoserverUrlField) &
            isUrl(nsURIField)
          )
      }

      action = swing.Action("Configure!!") {
        // TODO: get some code in here to configure geoserver,
        // but not on the swing dispatch thread
      }
    }

    new swing.GridBagPanel {
      import swing.GridBagPanel.{ Anchor, Fill }
      border = new javax.swing.border.EmptyBorder(4, 4, 4, 4)
      layout += (
        geoserverUrl -> new Constraints { grid = (0, 0); anchor = Anchor.East },
        dataDir -> new Constraints { grid = (0, 1); anchor = Anchor.East },
        userName -> new Constraints { grid = (0, 2); anchor = Anchor.East },
        passwd -> new Constraints { grid = (0, 3); anchor = Anchor.East },
        nsPrefix -> new Constraints { grid = (0, 4); anchor = Anchor.East },
        nsURI -> new Constraints { grid = (0, 5); anchor = Anchor.East }
      )
      layout += (
        geoserverUrlField -> (1, 0),
        dataDirField -> (1, 1),
        userNameField -> (1, 2),
        passwdField -> (1, 3),
        nsPrefixField -> (1, 4),
        nsURIField -> (1, 5)
      )
      layout(runButton) =
        new Constraints {
          grid = (1, 6)
          anchor = Anchor.LastLineEnd
        }
    }
  }

  val tabs = new swing.TabbedPane {
    pages += new swing.TabbedPane.Page(
      "Generate SLD Files",
      sldConversionForm,
      "Write SLD files to local hard drive")
    pages += new swing.TabbedPane.Page(
      "Configure GeoServer", 
      geoserverConfigurationForm,
      "Configure a remote GeoServer via REST API")
  }

  trait Job {
    def progressRange: (Int, Int)
    def run(updateProgress: Int => Unit)
  }

  trait Converter {
    import Mapnik2GeoTools._

    val convert = new xml.transform.RuleTransformer(
      FilterTransformer,
      PointSymTransformer,
      MarkersSymTransformer,
      LineSymTransformer,
      RasterSymTransformer
    ) andThen (new xml.transform.RuleTransformer(RuleCleanup))
  }

  import java.io.File
  class WriteSLDs(input: File, output: File) extends Job with Converter {
    val original = xml.XML.load(input.getAbsolutePath)

    val styleCount = (original \\ "Style").size
    val progressRange = (0, styleCount)

    val doc = convert(original)
    val printer = new xml.PrettyPrinter(80, 2)

    def writeStyle(style: xml.Node) {
      val name = style.attributes.asAttrMap("name")
      val wrapper = 
        <StyledLayerDescriptor
          version="1.0.0"
          xmlns="http://www.opengis.net/sld"
          xmlns:ogc="http://www.opengis.net/ogc"
          xmlns:xlink="http://www.w3.org/1999/xlink"
        >
          <NamedLayer>
            <Name>{name}</Name>
            <UserStyle>
              <Name>{name}</Name>
              <FeatureTypeStyle>{style.child}</FeatureTypeStyle>
            </UserStyle>
          </NamedLayer>
        </StyledLayerDescriptor>

      val writer = new java.io.FileWriter(new File(output, name + ".sld"))
      writer.write(printer.format(wrapper))
      writer.close()
    }

    def run(updateProgress: Int => Unit) {
      for ((style, idx) <- (doc \\ "Style").zipWithIndex) {
        writeStyle(style)
        updateProgress(idx + 1)
      }
    }
  }

  class WriteSQLLoader(input: File, output: File) extends Job {
    val selectPattern = """(?si:\(SELECT\s+(.*)\)\s+AS)""".r

    val original = xml.XML.load(input.getAbsolutePath)

    def params(datastore: xml.NodeSeq): Map[String, String] =
      (datastore \ "Parameter").map(
        p => (p.attributes.asAttrMap("name"), p.text)
      )(collection.breakOut) 
      
    val layers = 
      (original \\ "Layer") filter { lyr =>
        params(lyr \ "Datasource") contains "table"
      }

    val progressRange = (0, layers.size)

    def run(updateProgress: Int => Unit) {
      val dataLayers =
        for (layer <- layers; settings = params(layer)) yield {
          val db = (settings("user"), settings("host"), settings("port"), settings("dbname"))
          val name = layer.attributes.asAttrMap("name")
          val table = settings("table")
          (name, db, table)
        }

      val databases = dataLayers.groupBy(_._2)

      val createStatementsByDB = 
        databases.mapValues { tables =>
          val createStatements =
            for {
              (name, db, table) <- tables
              where <- selectPattern.findFirstMatchIn(table) map(_.group(1).trim)
            } yield {
              val cleanName = name.replaceAll("[\\s-]", "_")
              Seq(
                "CREATE TABLE " + cleanName + " AS SELECT " + where + ";",
                "ALTER TABLE " + cleanName + " ADD COLUMN id SERIAL PRIMARY KEY;",
                "INSERT INTO geometry_columns VALUES ( '', 'public', '" + cleanName + "', 'way', 2, 900913, 'GEOMETRY');",
                "CREATE INDEX " + cleanName + "_idx ON " + cleanName + " USING GIST(way);"
              ).mkString("%n".format())
            }
        }
    }
  }

  override def main(args: Array[String]) = super.main(args)

  def startup(args: Array[String]) {
    val frame = new swing.MainFrame
    locally {
      import frame._

      runButton.subscribe {
        case swing.event.ButtonClicked(`runButton`) =>
          val preSize = size
          val job = new WriteSLDs(
            new java.io.File(source.textField.text),
            new java.io.File(output.textField.text)
          )

          val progress = new swing.ProgressBar
          val (min, max) = job.progressRange
          progress.min = min
          progress.max = max

          actors.Futures.future { 
            job.run(prog => progress.value = prog)
            // val sink = new FileSystem(new java.io.File(output.textField.text))
            // val original = xml.XML.load(source.textField.text)
            // val convert =
            //   new xml.transform.RuleTransformer(
            //     Mapnik2GeoTools.FilterTransformer
            //   )
            // val doc = convert(original)
            // progress.max = (doc \\ "Style").size + (doc \\ "Layer").size
            // for ((style, idx) <- (doc \\ "Style").zipWithIndex) {
            //   sink.writeStyle(style)
            //   progress.value = idx + 1
            // }
            // sink.writeLayers(doc \\ "Layer")
            progress.value = progress.max
          }

          contents = new swing.FlowPanel(progress)
          size = preSize
        case _ => ()
      }

      title = "Mapnik To GeoServer Translator"
      contents = tabs // geoserverConfigurationForm
      visible = true
    }
  }
}
