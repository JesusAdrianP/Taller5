package scalashop

import org.scalameter.measure

import java.awt.{BorderLayout, GridLayout}
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.{BorderFactory, JButton, JComboBox, JFileChooser, JFrame, JLabel, JMenu, JMenuBar, JMenuItem, JOptionPane, JPanel, JScrollPane, JSpinner, JTextArea, SpinnerNumberModel, UIManager, WindowConstants, border}

object ScalaShop {

  class ScalaShopFrame extends JFrame("ScalaShop\u2122") {
    setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    setSize(1024, 600)
    setLayout(new BorderLayout)

    val rightpanel = new JPanel
    rightpanel.setBorder(BorderFactory.createEtchedBorder(border.EtchedBorder.LOWERED))
    rightpanel.setLayout(new BorderLayout)
    add(rightpanel, BorderLayout.EAST)

    val controls = new JPanel
    controls.setLayout(new GridLayout(0, 2))
    rightpanel.add(controls, BorderLayout.NORTH)

    val filterLabel = new JLabel("Filtro")
    controls.add(filterLabel)

    val filterCombo = new JComboBox(Array(
      "desenfoque-horizontal",
      "desenfoque-vertical"
    ))
    controls.add(filterCombo)

    val radiusLabel = new JLabel("Radio")
    controls.add(radiusLabel)

    val radiusSpinner = new JSpinner(new SpinnerNumberModel(3, 1, 16, 1))
    controls.add(radiusSpinner)

    val tasksLabel = new JLabel("Tareas")
    controls.add(tasksLabel)

    val tasksSpinner = new JSpinner(new SpinnerNumberModel(32, 1, 128, 1))
    controls.add(tasksSpinner)

    val stepbutton = new JButton("Aplicar filtro")
    stepbutton.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent) {
        val time = measure {
          canvas.applyFilter(getFilterName, getNumTasks, getRadius)
        }
        updateInformationBox(time.value)
      }
    })
    controls.add(stepbutton)

    val clearButton = new JButton("Recargar")
    clearButton.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent) {
        canvas.reload()
      }
    })
    controls.add(clearButton)

    val info = new JTextArea("   ")
    info.setBorder(BorderFactory.createLoweredBevelBorder)
    rightpanel.add(info, BorderLayout.SOUTH)

    val mainMenuBar = new JMenuBar()

    val fileMenu = new JMenu("File")
    val openMenuItem = new JMenuItem("Abrir...")
    openMenuItem.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent) {
        val fc = new JFileChooser()
        if (fc.showOpenDialog(ScalaShopFrame.this) == JFileChooser.APPROVE_OPTION) {
          canvas.loadFile(fc.getSelectedFile.getPath)
        }
      }
    })
    fileMenu.add(openMenuItem)
    val exitMenuItem = new JMenuItem("Salir")
    exitMenuItem.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent) {
        sys.exit(0)
      }
    })
    fileMenu.add(exitMenuItem)

    mainMenuBar.add(fileMenu)

    val helpMenu = new JMenu("Ayuda")
    val aboutMenuItem = new JMenuItem("Sobre")
    aboutMenuItem.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent) {
        JOptionPane.showMessageDialog(null, "ScalaShop, the ultimate image manipulation tool\nBrought to you by EPFL, 2015")
      }
    })
    helpMenu.add(aboutMenuItem)

    mainMenuBar.add(helpMenu)

    setJMenuBar(mainMenuBar)

    val canvas = new PhotoCanvas

    val scrollPane = new JScrollPane(canvas)

    add(scrollPane, BorderLayout.CENTER)
    setVisible(true)

    def updateInformationBox(time: Double) {
      info.setText(s"Tiempo: $time")
    }

    def getNumTasks: Int = tasksSpinner.getValue.asInstanceOf[Int]

    def getRadius: Int = radiusSpinner.getValue.asInstanceOf[Int]

    def getFilterName: String = {
      filterCombo.getSelectedItem.asInstanceOf[String]
    }

  }

  try {
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
  } catch {
    case _: Exception => println("Cannot set look and feel, using the default one.")
  }

  val frame = new ScalaShopFrame

  def main(args: Array[String]) {
    frame.repaint()
  }

}
