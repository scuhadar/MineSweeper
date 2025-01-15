import hevs.graphics.FunGraphics
import hevs.graphics.utils.GraphicsBitmap
import java.awt.event.{KeyAdapter, KeyEvent, MouseAdapter, MouseEvent}
import java.awt.{Color, Font}
import scala.util.Random

object MineSweeper extends App {
  val cellSize = 30
  val selectedLevel = "easy"

  val gridSize:Int= selectedLevel match{
    case "easy"   => 9
    case "medium" => 16
    case "hard"   => 24
    case _        => 9
  }

  val numMines:Int= selectedLevel match {
    case "easy" => 10
    case "medium" => 40
    case "hard" => 90
  }

  val window:Int=(cellSize*gridSize)+100

  val display = new FunGraphics(window, window, "MINESWEEPER")
  val cellTotalSize = gridSize * cellSize
  val startX = (window - cellTotalSize) / 2
  val startY = (window - cellTotalSize) / 2

  var gameOver = false // Oyunun bitiş durumunu kontrol etmek için
  var mineImage = new GraphicsBitmap("/res/mine.jfif")
  val scaleRate = (cellSize.toDouble / mineImage.getWidth) * 0.6
  var remainingMines:Int =numMines

  val grid: Array[Array[Cell]] = Array.ofDim[Cell](gridSize, gridSize)
  for (x <- 0 until gridSize) {
    for (y <- 0 until gridSize) {
      grid(x)(y) = new Cell(x, y)
    }
  }

  def drawGrid(): Unit = {
    display.clear()
    for (col <- 0 until gridSize; row <- 0 until gridSize) {
      val x = col * cellSize
      val y = row * cellSize

      val cell = grid(row)(col)

      val cellColor =
        if (cell.isRevealed) {
          if (cell.isMine) Color.WHITE
          else Color.LIGHT_GRAY
        } else {
          Color.CYAN
        }

      display.setColor(cellColor)
      display.drawFillRect(x + startX, y + startY, cellSize, cellSize)

      display.setColor(Color.BLACK)
      display.drawRect(x + startX, y + startY, cellSize, cellSize)

      if (cell.isRevealed && cell.isMine) {
        val centerX = x + startX + cellSize / 2
        val centerY = y + startY + cellSize / 2
        display.drawTransformedPicture(centerX, centerY, angle = 0, scale = scaleRate, mineImage)
      }

      if (cell.isFlagged && !cell.isRevealed) {
        val fontSize = (cellSize * 0.6).toInt
        val centerX = x + startX + cellSize / 2
        val centerY = y + startY + cellSize / 2

        val font = new Font("SansSerif", Font.BOLD, fontSize)
        val flagText = "⚑"
        val textBounds = display.getStringSize(flagText, font)

        val textWidth = textBounds.getWidth.toInt
        val textHeight = textBounds.getHeight.toInt
        val textX = centerX - textWidth / 2
        val textY = centerY + textHeight / 2

        display.setColor(Color.RED)
        display.drawString(textX, textY, flagText, font, Color.RED)
      }

      if (cell.isRevealed && !cell.isMine && cell.adjacentMines > 0) {
        val fontSize = (cellSize * 0.5).toInt
        val centerX = x + startX + cellSize / 2
        val centerY = y + startY + cellSize / 2

        val font = new Font("SansSerif", Font.BOLD, fontSize)
        val text = cell.adjacentMines.toString
        val textBounds = display.getStringSize(text, font)

        val textWidth = textBounds.getWidth.toInt
        val textHeight = textBounds.getHeight.toInt
        val textX = centerX - textWidth / 2
        val textY = centerY + textHeight / 2

        val color = cell.adjacentMines match {
          case 1 => Color.BLUE
          case 2 => Color.GREEN
          case 3 => Color.RED
          case 4 => Color.MAGENTA
          case 5 => Color.ORANGE
          case 6 => Color.CYAN
          case 7 => Color.PINK
          case 8 => Color.GRAY
          case _ => Color.BLACK
        }

        display.setColor(color)
        display.drawString(textX, textY, text, font, color)
      }
    }

    if (gameOver) {
      display.setColor(Color.RED)
      val font = new Font("SansSerif", Font.BOLD, 20)
      val message = s"Game Over! You passed $secondsElapsed second"
      val bounds = display.getStringSize(message, font)
      val x = (window - bounds.getWidth.toInt) / 2
      val y = (window + bounds.getHeight.toInt) / 2
      display.drawString(x, y, message, font, Color.RED)
    } else if (checkWin()) {
      display.setColor(Color.GREEN)
      val font = new Font("SansSerif", Font.BOLD, 20)
      val message = s"You Won! You passed $secondsElapsed second"
      val bounds = display.getStringSize(message, font)
      val x = (window - bounds.getWidth.toInt) / 2
      val y = (window + bounds.getHeight.toInt) / 2
      display.drawString(x, y, message, font, Color.blue)
    }
    drawMineCounterBox()
  }

  def revealAllMines(): Unit = {
    for (row <- 0 until gridSize; col <- 0 until gridSize) {
      val cell = grid(row)(col)
      if (cell.isMine) {
        cell.reveal()
      }
    }
    gameOver = true
    drawGrid()
  }

  def checkWin(): Boolean = {
    for (row <- 0 until gridSize; col <- 0 until gridSize) {
      val cell = grid(row)(col)
      if (!cell.isMine && !cell.isRevealed) {
        return false
      }
    }
    true
  }

  def recurseReveal(row: Int, col: Int): Unit = {
    if (row < 0 || row >= gridSize || col < 0 || col >= gridSize) return

    val cell = grid(row)(col)

    if (cell.isRevealed || cell.isMine) return

    cell.reveal()

    if (cell.adjacentMines == 0) {
      for (dx <- -1 to 1; dy <- -1 to 1 if !(dx == 0 && dy == 0)) {
        recurseReveal(row + dx, col + dy)
      }
    }
  }

  display.addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent): Unit = {
      if (gameOver || checkWin()) return

      val posx = e.getX
      val posy = e.getY

      val col = (posx - startX) / cellSize
      val row = (posy - startY) / cellSize

      if (col >= 0 && col < gridSize && row >= 0 && row < gridSize) {
        val cell = grid(row)(col)

        if (e.getButton == MouseEvent.BUTTON1) {
          if (!cell.isRevealed && !cell.isFlagged) {
            if (cell.isMine) {
              revealAllMines()
            } else if (cell.adjacentMines == 0) {
              recurseReveal(row, col)
            } else {
              cell.reveal()
            }
            drawGrid()
          }
        } else if (e.getButton == MouseEvent.BUTTON3) {
          if (!cell.isRevealed) {
            cell.isFlagged = !cell.isFlagged
            if(cell.isFlagged) remainingMines-=1 else remainingMines +=1
            drawGrid()
            drawMineCounterBox()
          }
        }
      }
    }
  })

  def placeMines(numMines: Int): Unit = {
    val random = new Random()
    var minesPlaced = 0

    while (minesPlaced < numMines) {
      val row = random.nextInt(gridSize)
      val col = random.nextInt(gridSize)

      if (!grid(row)(col).isMine) {
        grid(row)(col).placeMine()
        minesPlaced += 1
      }
    }
  }

  def countSurroundingMines(row: Int, col: Int): Int = {
    var count = 0

    for (dx <- -1 to 1; dy <- -1 to 1 if !(dx == 0 && dy == 0)) {
      val newRow = row + dx
      val newCol = col + dy
      if (newRow >= 0 && newRow < gridSize && newCol >= 0 && newCol < gridSize && grid(newRow)(newCol).isMine) {
        count += 1
      }
    }
    count
  }

  def calculateMineCounts(): Unit = {
    for (row <- 0 until gridSize; col <- 0 until gridSize) {
      if (!grid(row)(col).isMine) {
        grid(row)(col).adjacentMines = countSurroundingMines(row, col)
      }
    }
  }

  def resetGame():Unit={
    secondsElapsed = 0
    remainingMines = numMines
    startTimer()
    gameOver = false
    display.clear()
    for(col<- 0 until gridSize){
      for(row<- 0 until gridSize){
        val cell = grid(col)(row)
        cell.isRevealed = false
        cell.isFlagged = false
        cell.isMine = false
        cell.adjacentMines = 0
      }
    }
    placeMines(numMines)
    calculateMineCounts()
    display.clear(Color.cyan)
    drawGrid()
  }

  // Ekranda zamanlayıcı kutusunu çizmek için bir fonksiyon
  def drawTimerBox(): Unit = {
    val boxWidth = 110  // Kutu genişliği
    val boxHeight = 30  // Kutu yüksekliği
    val boxX = (window-cellTotalSize)/2 // Kutunun X pozisyonu (ekranın sağında)
    val boxY = 10  // Kutunun Y pozisyonu (ekranın üst kısmında)

    // Kutu çiz
    display.setColor(Color.cyan)
    display.drawFillRect(boxX, boxY, boxWidth, boxHeight)  // Çerçeve çiz

    // Zamanlayıcıyı kutunun içine yaz
    display.setColor(Color.white)  // Yazıyı beyaz yap
    val timerText = s"Time: $secondsElapsed"
    display.drawString(boxX+5 ,boxY+20,timerText)  // Yazıyı kutuya yerleştir
  }

  def drawMineCounterBox():Unit={
    val boxWidth =100
    val boxHeight = 30
    val boxX =((window+cellTotalSize)/2)-boxWidth
    val boxY = 10

    display.setColor(Color.cyan)
    display.drawFillRect(boxX, boxY, boxWidth, boxHeight)

    val mineCounterText = s"Mines : $remainingMines"

    display.setColor(Color.white)
    display.drawString(boxX+5,boxY+20, mineCounterText)
  }

  display.setKeyManager(new KeyAdapter() {
    override def keyPressed(e: KeyEvent):Unit={
      if (e.getKeyChar == 'r' || e.getKeyChar == 'R'){
        val font = new Font("SansSerif", Font.BOLD, 20)
        val message = "Restarting the game..."
        val bounds = display.getStringSize(message, font)
        val x = (window - bounds.getWidth.toInt) / 2
        val y = (window + bounds.getHeight.toInt) / 2
        display.clear()
        display.drawString(x, y, message, font, Color.RED)
        Thread.sleep(1000)
        resetGame()
      }
    }
  })

  // Variables for tracking time and game state
  var secondsElapsed: Int = 0  // Tracks the number of seconds passed since the start
  // Function to start the timer when the game begins
  def startTimer(): Unit = {
    new Thread(new Runnable {
      def run(): Unit = {
        while (!gameOver && ! checkWin()  )  {    // Loop runs until the game is over
          Thread.sleep(1000)      // Wait for 1 second
          secondsElapsed += 1     // Increment the time by 1 second
          drawTimerBox()
        }
      }
    }).start()  // Start the thread in the background
  }
  def initializeGame(): Unit = {
    startTimer()
    placeMines(numMines)
    calculateMineCounts()
    drawGrid()
    drawMineCounterBox()
  }
  initializeGame()
}
