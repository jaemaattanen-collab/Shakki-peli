# ScalaFX Chess Application

A comprehensive chess application built with Scala 3 and ScalaFX, featuring Stockfish integration for game analysis.

## Features

- **Chess Board** - Full chess game with all standard rules (castling, en passant, promotion)
- **PGN Support** - Import/export PGN files with variation support
- **Game Analysis** - Stockfish-powered analysis with move classifications (Brilliant, Best, Excellent, Good, Inaccuracy, Mistake, Blunder)
- **Game Review** - Review analyzed games with visual feedback
- **Error Training Mode** - Practice from your mistakes against Stockfish

## Requirements

- **Scala 3.6.2**
- **SBT 1.10+**
- **Java 17+** (for JavaFX/ScalaFX)
- **Stockfish** - Download from [stockfishchess.org](https://stockfishchess.org/download/)

## Stockfish Setup

The application looks for Stockfish in these locations:
1. `/usr/local/bin/stockfish`
2. `/opt/homebrew/bin/stockfish`  
3. `./stockfish` (project directory)
4. System PATH

For macOS with Homebrew:
```bash
brew install stockfish
```

## Running

```bash
sbt run
```

## Building

```bash
sbt compile
```

## Testing

```bash
sbt test
```

## Project Structure

```
src/main/scala/chess/
├── analysis/       # Stockfish integration, game analysis
├── board/          # Board and square models
├── controllers/    # Game controller, move history
├── pgn/            # PGN parser, exporter, replayer
├── pieces/         # Chess piece implementations
├── training/       # Error training mode
├── types/          # Type definitions
├── ui/             # ScalaFX UI components
└── utils/          # Utilities

Peliarkisto/        # Sample PGN files for testing
```

## License

Private project
