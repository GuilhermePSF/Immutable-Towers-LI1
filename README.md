<h1 align="center">
  <img src="https://raw.githubusercontent.com/GuilhermePSF/Immutable-Towers-LI1/refs/heads/main/images/inimigo/sulD.bmp" alt="South Tower" height="60" />
  &nbsp;&nbsp;IMMUTABLE TOWERS&nbsp;&nbsp;
  <img src="https://raw.githubusercontent.com/GuilhermePSF/Immutable-Towers-LI1/refs/heads/main/images/inimigo/esteD.bmp" alt="East Tower" height="60" />
</h1>

> A functional interpretation of the classic **Tower Defense** genre, implemented in **Haskell** with a graphical interface powered by **gloss**. Players must strategically position towers to prevent waves of enemies from reaching their base, utilizing different projectile types with unique synergies and effects. Built for *Laboratórios de Informática I* (1st year, Software Engineering — Universidade do Minho).

![Language](https://img.shields.io/badge/language-Haskell-purple)
![Build](https://img.shields.io/badge/build-Cabal-blue)
![Grade](https://img.shields.io/badge/Grade-18%2F20-brightgreen)

---

## Hall of Fame

This project was featured in the **LI1 Hall of Fame** at Universidade do Minho.

**Play the game online:** [Immutable Towers - Web Version](https://haslab.github.io/Teaching/LI1/HallOfFame/2425_web/2024li1g009.jsexe/run.html)

**Browse other featured projects:** [LI1 Hall of Fame](https://haslab.github.io/Teaching/LI1/HallOfFame/)

---

## Requirements

| Dependency | Notes |
|---|---|
| GHC | Glasgow Haskell Compiler |
| Cabal | Haskell build tool |
| gloss | Graphics library for rendering |
| HUnit | Unit testing framework |

---

## Build

From the project root:
```bash
cabal build
```

This produces the game executable from the Haskell sources.

---

## Usage

### Play the game

Launch the interactive graphical interface:
```bash
cabal run
```

The game features:
- Mouse-driven interaction for tower placement and upgrades
- Strategic resource management through the in-game shop
- Progressive difficulty through wave-based enemy spawning
- Level progression system with save/load functionality

### Run tests

Execute the automated test suite:
```bash
cabal test
```

---

## Gameplay Mechanics

### Terrain Types
- **Grass** — standard buildable terrain
- **Dirt** — alternative buildable surface
- **Water** — non-buildable obstacle

### Tower Types & Projectiles

| Projectile | Effect |
|---|---|
| **Fire** | Continuous damage over time |
| **Ice** | Freezes enemies, halting movement |
| **Resin** | Reduces enemy movement speed |
| **Poison** | Applies weaker Fire and Resin effects simultaneously |
| **Laser** | High-damage precision targeting |

### Enemy System
- Dynamic enemy states with directional movement
- Health and speed attributes
- Portal-based wave spawning system
- Visual status indicators and animations

---

## Repository Layout

```
immutable-towers/
├── src/
│   ├── Main.hs              # Gloss-based graphical interface
│   ├── Jogo.hs              # Global game state management
│   ├── Mapa.hs              # Map representation and validation
│   ├── Base.hs              # Player base state and mechanics
│   ├── Torre.hs             # Tower definitions and projectile logic
│   ├── Inimigo.hs           # Enemy movement and effect system
│   ├── Portal.hs            # Wave generation and portal management
│   └── Testes.hs            # HUnit test suite
├── images/                  # Game sprites and visual assets
│   ├── inimigo/             # Enemy directional sprites
│   ├── torre/               # Tower graphics
│   └── terreno/             # Terrain tiles
├── Enunciado.pdf            # Original project specification (Portuguese)
└── *.cabal                  # Cabal project configuration
```

---

## Extended Features

This implementation includes several enhancements beyond the base requirements:

- **Isometric rendering** — Rich visual representation with depth perception
- **Advanced tower mechanics** — Including laser-based towers with unique targeting
- **Mouse interaction** — Click-based tower purchasing and positioning
- **Visual effects** — Animations for combat, projectiles, and game events
- **Level progression** — Multi-stage difficulty scaling
- **Persistent state** — Save and load game functionality

---

## Testing

The project includes comprehensive unit tests implemented with **HUnit**.

Run the full test suite:
```bash
cabal test
```

Test coverage includes:
- Map validation logic
- Enemy pathfinding and state transitions
- Tower targeting and damage calculations
- Projectile synergy effects
- Portal wave generation

---

## Documentation

For detailed project specification and requirements, refer to:
- [`Enunciado.pdf`](Enunciado.pdf) — Original project statement (Portuguese)

---

## Authors
- Guilherme Ferreira - A111042
- Lucas Pinto - A111042
