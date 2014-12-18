# PokemonArena

A multiplayer Pokemon battle arena game written in Haskell.

There will be support for multiple front-ends, but first support will be text-based due to time constraints.

# Design

**This design document is a work in progress, ideas are welcome in form of issues.**

*  **PokemonArena** will consist of a client and a server.
*  The server will be connected to a database and will hold a persistent world.
*  Clients will be able to connect to the server.
*  There will be a lobby system to handle the multiplayer features.
*  The single-player features will also be hosted on the server to prevent cheating.
*  The single-player features will consist procedurally generated campaigns in the Wilderness to gain fame and fortune.
*  The single-player features will consist of battling against AI's of matching difficulty.
*  The town will consist of different shops; to buy items with effects, different pokeballs, different pokemons, etc.

## Server
### World
The server will handle the simulation of the world.

The world will consist of three stages so far.

*  The Arena
*  The Wilderness
*  The Town

The server will be connected to a lobby system with match-making support.

### The Town

### The Wilderness

### The Single-player Arena

## Client
The client will only send its inputs to the server which will validate them before execution.

The server will send the minimum representation of the consequence from the client's input to keep performance at bay.

### Front-ends
*  Text-based at first to get features up and running,
*  HTML5 on canvas/webgl,
*  2D with SDL,
*  3D with OpenGL.

### Input
*  Keyboard in text,
*  Point & click when in 2D/3D.

## General protocol
*  TODO: Need to look at different options of serialization in Haskell, will probably use protobuffs unless anything better turns up.

## Database for persistence
*  TODO: Research database usage in Haskell
