Minesweeper Game

<img width="379" alt="easy - you win" src="https://github.com/user-attachments/assets/e5ab7801-c749-4643-9e4c-979d23a487d1" />

This is a Scala implementation of the classic Minesweeper game with a graphical user interface (GUI) using the FunGraphics library. The game allows players to choose difficulty levels, explore the grid, and flag potential mines.

This project was created for the HES-SO Valais Programming course by Soner Çuhadar and Yılmaz Carkanat.

(https://www.linkedin.com/in/cuhadarson)
(https://www.linkedin.com/in/yilmaz-carkanat-77ba122a5/)

Features
	•	Difficulty Levels:
	•	Easy (9x9 grid with 10 mines)
	•	Medium (16x16 grid with 40 mines)
	•	Hard (24x24 grid with 90 mines)
 
Graphical Interface:
	•	Intuitive and colorful grid representation.
	•	Mines and flags visually represented with icons and colors.
 
 <img width="829" alt="hard" src="https://github.com/user-attachments/assets/82cc42f0-6138-4ddd-af15-c49b3a638f4f" />

Game Interaction:
	•	Left-click to reveal cells.
	•	Right-click to flag/unflag cells.
	•	Press R to restart the game.
 
Timers and Counters:
	•	Timer to track the game duration.
	•	Mine counter showing the remaining mines.
	•	Win/Lose Conditions:
	•	Lose: Reveal a mine.
	•	Win: Successfully reveal all non-mine cells.
 
<img width="387" alt="minesweeper - easy" src="https://github.com/user-attachments/assets/45388571-2159-4c13-b231-998c3e72960a" />

How to Play
	1.	Launch the application.
	2.	Select your difficulty level by modifying the selectedLevel variable in the code (easy, medium, or hard).
	3.	Explore the grid using:
	•	Left-click to reveal a cell.
	•	Right-click to flag/unflag a suspected mine.
	4.	The game ends when:
	•	All non-mine cells are revealed (Win).
	•	A mine is revealed (Lose).

<img width="585" alt="minesweeper -medium -gameover" src="https://github.com/user-attachments/assets/fb26650b-79ce-40a2-a076-619306ae2fe1" />

Key Bindings
	•	R: Restart the game.

Setup and Run Instructions

Prerequisites
	•	Scala environment installed.
	•	The FunGraphics library and its dependencies.
	•	Ensure the mine icon file (mine.jfif) is in the res folder of your project directory.

Running the Game
	1.	Clone or download the project.
	2.	Open the project in your Scala IDE or editor.
	3.	Run the MineSweeper object.

Directory Structure

project/
├── src/
│   └── main/
│       └── scala/
│           └── MineSweeper.scala
├── res/
│   └── mine.jfif
└── README.md


License

This project is for educational purposes and is open for contributions and modifications.
