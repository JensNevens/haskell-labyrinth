# Haskell Labyrinth

_Programming assignment for the course [Functional Programming](https://www.vub.ac.be/en/study/fiches/54625/functional-programming) taught at [VUB SOFT Department](http://soft.vub.ac.be/soft)_

## Labyrinth Rules

The goal of this assignment is to implement the [Labyrinth board game](https://www.ravensburger.com/uk/games/family-games/labyrinth/index.html
). This game is designed for up to 4 players and its aim is to collect treasures in a dynamically changing maze. At the beginning of the game, treasure cards are distributed across players. The winner is the first player who collect all the treasures associated to its cards and return to its starting position. A turn consists of:

  1. Shift a movable row of the maze using an extra tile, producing an novel extra tile.
  2. Gather your treasures reachable from your pawn position.
  3. Move your pawn to a reachable tile of your choice.

## Implementation Details

You should develop a terminal-based application that enables 1 to 4 players to play labyrinth and support basic “artificial intelligence”. At the beginning of the game the users specify how many real players will take part in the race and which will be controlled by the AI. Then the free tiles (16 corners, 6 t-shaped, 12 lines) are randomly placed between the fixed tiles with an extra free tile as shown in Figure 1. The kind and orientation of each tile should be clear from its ASCII representation. Finally, the 24 treasures are randomly placed on the tiles with maximum one treasure per tile and no treasure on the starting positions. Each one of these treasure is associated with a treasure card. These 24 treasure cards are randomly distributed across the players with a equal number of treasure cards for every player. Once the preparation phase is completed, players play each one in turn. A the beginning of its turn, a player has access to the following information:

  1. The state of the board (the extra tile, placed tiles, treasures and pawns).
  2. The player’s own treasure cards
  3. The number of treasure cards left for every other player.

If the player is not controlled by the AI, the user is invited to insert the free tile on the board to shift a movable row of the maze without making a pawn fall off the board. Then, all the player’s treasure cards whose associated treasure is reachable from the player pawn are automatically revealed from the player’s hand. The player can now move its pawn to a reachable tile of its choice (a tile may receive multiple pawns). If the player moves to its starting position without having any treasure left to collect he wins the game. Make sure that the next player does not see the treasure cards of the current player by using `System.Process.callCommand "clear"`. The turns of the players controlled by the AI should not be shown and the next human controlled player should be prompted to play (unless the AI won the game). The minimal requirement for the AI is that it should win the game alone in a finite number of turn for every possible starting configuration. Smart strategies will be considered as bonuses.

## Serialization

A game of labyrinth may take a long time therefore you should enable saving and loading functionalities. Human players should be able to save and exit the game at the beginning of each turn. Your executable should accept an optional command-line argument to load a game from a file. The serialization format is specified in Table 1 and is mostly self-explaining. The order of the player’s list indicates the turn order to resume the game. Tiles are filled from top to bottom and then left to right as shown in Table 2. `nil` denotes the empty string and `natural` denotes natural numbers: `/[0-9]+/`. All tokens can be separated by multiple blank characters: `/[\n\t ]+/`.
