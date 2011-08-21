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
    }
    source.textField.subscribe(handler)
    output.textField.subscribe(handler)
  }


  val sldConversionForm = new swing.GridBagPanel {
    import swing.GridBagPanel.{ Anchor, Fill }
    layout(sourceLabel) = (0, 0)
    layout(outputLabel) = (0, 1)
    layout(source) = new Constraints { 
      grid = (1, 0)
      fill = Fill.Horizontal
      weightx = 1d
    }
    layout(output) = new Constraints { 
      grid = (1, 1)
      fill = Fill.Horizontal
      weightx = 1d
    }
    layout(runButton) = new Constraints {
      grid = (1, 2)
      anchor = Anchor.LastLineEnd
    }
  }

  def startup(args: Array[String]) {
    val frame = new swing.MainFrame
    locally {
      import frame._

      runButton.subscribe {
        case swing.event.ButtonClicked(`runButton`) =>
          val progress = new swing.ProgressBar
          progress.min = 0
          progress.max = 100

          val preSize = size
          actors.Futures.future { 
            val sink = new FileSystem(new java.io.File(output.textField.text))
            val original = xml.XML.load(source.textField.text)
            val convert =
              new xml.transform.RuleTransformer(
                Mapnik2GeoTools.FilterTransformer
              )
            val doc = convert(original)
            progress.max = (doc \\ "Style").size + (doc \\ "Layer").size
            for ((style, idx) <- (doc \\ "Style").zipWithIndex) {
              sink.writeStyle(style)
              progress.value = idx + 1
            }
            sink.writeLayers(doc \\ "Layer")
            progress.value = progress.max
          }

          contents = new swing.FlowPanel(progress)
          size = preSize
      }

      title = "Mapnik To GeoServer Translator"
      contents = sldConversionForm
      visible = true
    }
  }
}
