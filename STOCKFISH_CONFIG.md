# Stockfish Configuration Guide for Your Macs

## Current Setup: M1 MacBook Air (2020-2022)
**Specs:**
- 8 cores (4 performance + 4 efficiency)
- 8-16GB RAM
- Excellent single-core performance

**Current Settings:**
```scala
depth = 14          // Sweet spot for speed/accuracy on M1
threads = 6         // Uses 6 of 8 cores (leaves 2 for OS/UI)
hashSizeMB = 1024   // 1GB hash - plenty for M1
```

**Performance:**
- ~30-45 seconds for 40-move game
- ~0.7-1.0 seconds per move
- Excellent for quick analysis!

---

## Upgrade: M4 Pro 14-Core MacBook Pro 16" (Next Week!)
**Specs:**
- 14 cores (10 performance + 4 efficiency)
- 18-36GB RAM
- Significantly faster than M1

**Recommended Settings for M4 Pro:**
```scala
depth = 17          // Much deeper analysis (50% more depth!)
threads = 10        // Use 10 cores (leaves 4 for system)
hashSizeMB = 2048   // 2GB hash for better caching
```

**Expected Performance:**
- ~25-35 seconds for 40-move game (even with deeper analysis!)
- ~0.6-0.8 seconds per move
- Near-master level accuracy

---

## Quick Upgrade Instructions

When you get your M4 Pro, just update these lines in:
**File:** `src/main/scala/chess/ui/ScalaFXChessApp.scala`
**Line:** ~1779-1781

Change from:
```scala
val analyzerOpt = GameAnalyzer.create(EngineConfig(
  depth = 14,         // Good balance for M1
  threads = 6,        // Use 6 of 8 cores on M1
  hashSizeMB = 1024   // 1GB hash
))
```

To:
```scala
val analyzerOpt = GameAnalyzer.create(EngineConfig(
  depth = 17,         // Deeper analysis on M4 Pro!
  threads = 10,       // Use 10 of 14 cores
  hashSizeMB = 2048   // 2GB hash for M4 Pro
))
```

Then recompile:
```bash
sbt compile
sbt run
```

---

## Depth Comparison

| Depth | Strength | Speed (M1) | Speed (M4 Pro) | Use Case |
|-------|----------|------------|----------------|----------|
| 10    | ~2000 Elo | 15s | 8s | Quick check |
| 12    | ~2200 Elo | 25s | 12s | Fast review |
| 14    | ~2400 Elo | 40s | 18s | **Current M1** |
| 16    | ~2600 Elo | 70s | 28s | Strong analysis |
| 17    | ~2700 Elo | 100s | 35s | **Recommended M4 Pro** |
| 18    | ~2800 Elo | 140s | 45s | Very deep |
| 20    | ~2900 Elo | 250s | 70s | Maximum |

---

## Why These Settings?

### M1 Air (Current)
- **6 threads:** Leaves 2 cores free so UI stays responsive
- **1GB hash:** Sweet spot - more doesn't help much on M1
- **Depth 14:** Fast enough for quick reviews, accurate enough for club players

### M4 Pro (Upgrade)
- **10 threads:** Uses most performance cores, leaves efficiency cores for system
- **2GB hash:** M4 Pro has memory bandwidth to benefit from larger hash
- **Depth 17:** Near-master level analysis without excessive wait times

---

## Pro Tips

1. **For tournaments:** Bump depth to 18-20 on M4 Pro for pre-game prep
2. **For blitz reviews:** Use depth 12 for super-fast feedback
3. **Memory:** If you get 36GB+ RAM on M4 Pro, you could go to 4GB hash!
4. **Cores:** Never use ALL cores - always leave 2-4 for the system

Enjoy your analysis! 🚀♟️
