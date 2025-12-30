# Aeth
[![prabinpanta0](https://img.shields.io/badge/prabinpanta0-red)](https://github.com/prabinpanta0)
![NISH](https://img.shields.io/badge/Aeth-Nesh-green)

An elegant polymorphic shell that lacks the concept of elegance.

* **Version:** Nish (0.0.0.0)
* **License:** [MIT](LICENSE)

<div align="center">
  <img src="docs/assets/aeth.png" alt="Aeth Prompt" width="600"/>
</div>

A hybrid Linux shell written in Haskell, designed to bridge the legacy world of raw text with the modern world of structured data. Aeth is enjoyable to use, and safe by construction.

Aeth is a **polymorphic shell** that doesn't force you into one paradigm. Instead of choosing between traditional Unix shells (text streams) or modern shells like Nushell (structured data), Aeth provides both:

-   **Raw Mode:** `ls` → Returns text (traditional shell behavior)
-   **Structured Mode:** `@ls` → Returns structured data (Haskell types)

This allows you to leverage decades of Unix tools while also enjoying the benefits of type-safe structured data manipulation.

---

## Features

### Core Shell Capabilities

-   **Process Management:** Fork/exec to launch external programs
-   **Path Resolution:** Finds executables through `$PATH`
-   **I/O Redirection:** Supports `<`, `>`, and `2>` (coming soon)
-   **Piping:** Connect stdout → stdin for raw commands
-   **Built-in Commands:** `cd`, `exit`
-   **Environment Management:** Handles `$HOME`, `$USER`, `$PATH`, etc.

### Structured Commands

-   **`@ls [path]`** - List directory contents as structured table (name, kind, size)
-   **`@pwd`** - Print working directory as structured text
-   **`filter`** - Filter structured tables with expressions

### Dynamic Prompt

The prompt displays contextual information:

-    Current time
-    User@hostname
-    Current directory (with home shortening)
-    Git branch (when in a git repo)
-    Language/framework icons (Haskell, Python, Node, Rust, Go, C)
-    Primary IP address
-    Battery percentage (laptops)
-    Last command duration
-    Exit status indicator

---

## Installation

### Prerequisites

-   GHC 9.6.7+ and Cabal
-   Nerd Font (for icon rendering in prompt)

### Build

```bash
# Clone the repository
git clone https://github.com/prabinpanta0/Aeth
cd Aeth

# Build and run
cabal build
cabal run Aeth
```

---

## Usage

### Interactive Mode

```bash
./aeth
```

You'll see the dynamic prompt. Try these commands:

```bash
# Raw mode (traditional shell)
ls -la
echo "hello world"
cat README.md | grep Aeth

# Structured mode
@ls
@pwd
@ls /usr/bin

# Filtered structured output
@ls | filter { .size > 1MB }
@ls | filter { .kind == dir }
@ls | filter { .name contains test }
```

### Non-Interactive Mode

Execute commands directly:

```bash
cabal run Aeth -- -c "ls -la"
cabal run Aeth -- -c "@ls"
cabal run Aeth -- -c "@pwd"
```

### Filtering Structured Data

The `filter` command allows rich queries on structured tables:

```bash
@ls | filter { .size > 1MB }        # Files larger than 1MB
@ls | filter { .size >= 1024 }      # Files >= 1024 bytes
@ls | filter { .kind == dir }       # Only directories
@ls | filter { .kind != file }      # Everything except files
@ls | filter { .name contains kde } # Names containing "kde"
```

**Supported operators:** `==`, `!=`, `>`, `>=`, `<`, `<=`, `contains`  
**Supported fields:** `.name`, `.kind`, `.size`

---

## Configuration

Aeth uses a Haskell-based configuration file for maximum flexibility and type safety.

### Default Location

```
~/.config/Aeth/config.hs
```

The repository includes a template at `config/config.hs` that you can customize and copy to the location above.

### Quick Setup

```bash
# Create config directory
mkdir -p ~/.config/Aeth

# Copy template
cp config/config.hs ~/.config/Aeth/config.hs

# Edit to customize
$EDITOR ~/.config/Aeth/config.hs
```

For detailed configuration options, see [docs/CONFIGURATION.md](docs/CONFIGURATION.md).

---

## Documentation

-   [Configuration Guide](docs/CONFIGURATION.md) - Customize prompts, colors, icons, and add structured commands
-   [Architecture Overview](docs/ARCHITECTURE.md) - Understanding the hybrid shell design

---

## Current Limitations

This is an early prototype. Known limitations:

-    No quoting/escaping in parser yet (whitespace-based)
-    Multi-stage raw pipelines are delegated to `/bin/sh -c`
-    Structured pipelines (`@a | @b`) not yet implemented
-    Limited I/O redirection support
-    Small set of built-in structured commands

---

## Roadmap

### Near Term

1.  Minimal REPL with line editing and history
2.  Basic parser for `@prefix` mode selection
3.  Builtins: `cd`, `exit`
4.  First structured commands: `@ls`, `@pwd`
5.  Table filtering with expressions

### Next Milestones

1. Replace placeholder parser with full Megaparsec parser (quoting, escapes, redirects)
2. Implement real process-level piping for raw pipelines
3. Add structured registry + typed piping bridge (Raw ↔ Structured)
4. Expand structured command library
5. Improve display engine (sorting, column selection, themes)

---

## Contributing

Please read the [architecture docs](docs/ARCHITECTURE.md) to understand the basic design.

---

## License

MIT License - see [LICENSE](LICENSE) file for details.