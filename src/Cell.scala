class Cell (val x:Int, val y:Int, var isMine:Boolean=false, var isRevealed: Boolean = false,var isFlagged: Boolean = false) {

  var adjacentMines: Int = 0

  def placeMine(): Unit = {
    isMine = true
  }

  def reveal(): Unit = {
    if (!isRevealed) {
      isRevealed = true
    }
  }
}
