# Configuration Guide

This guide explains how to customize Aeth.

## Quick Start

```bash
mkdir -p ~/.config/Aeth
# From the Aeth repository root:
cp config/config.toml ~/.config/Aeth/config.toml
# Or from an installed location:
# cp /usr/share/Aeth/config.toml ~/.config/Aeth/config.toml
```

## Configuration Files

| File                         | Purpose                        |
| ---------------------------- | ------------------------------ |
| `~/.config/Aeth/config.toml` | Main configuration             |
| `~/.config/Aeth/rc`          | Startup commands               |
| `~/.config/Aeth/history`     | Command history (auto-managed) |

---

## config.toml

```toml
# UI Mode
ui_mode = "normal"        # 'normal' or 'tui'

# Prompt Style
prompt_style = "minimal"  # 'minimal', 'powerline', 'simple', or 'custom'

# Custom prompt format (when prompt_style = 'custom')
# Placeholders: {cwd}, {user}, {host}, {branch}, {exit}, {git}
# prompt = "{user}@{host}:{cwd} {git}$ "

# Prompt features
show_git_branch = true
show_exit_code = true
show_duration = false

# Colors: black, red, green, yellow, blue, magenta, cyan, white, gray
cwd_color = "cyan"
git_color = "magenta"
error_color = "red"
success_color = "green"

# Security
safe_mode = false

# History
history_size = 10000

# Aliases
alias.ll = "ls -la"
alias.la = "@ls -a"
alias.g = "git"
alias.cls = "clear"
```

## Options Reference

| Option            | Values                                     | Default   | Description                 |
| ----------------- | ------------------------------------------ | --------- | --------------------------- |
| `ui_mode`         | `normal`, `tui`                            | `normal`  | Interface mode              |
| `prompt_style`    | `minimal`, `powerline`, `simple`, `custom` | `minimal` | Prompt style                |
| `prompt`          | format string                              | -         | Custom prompt format        |
| `show_git_branch` | `true`, `false`                            | `true`    | Show git branch             |
| `show_exit_code`  | `true`, `false`                            | `true`    | Show error indicator        |
| `show_duration`   | `true`, `false`                            | `false`   | Show execution time         |
| `safe_mode`       | `true`, `false`                            | `false`   | Restrict dangerous commands |
| `history_size`    | integer                                    | `10000`   | Max history entries         |
| `cwd_color`       | color name                                 | `cyan`    | Directory color             |
| `git_color`       | color name                                 | `magenta` | Git branch color            |
| `error_color`     | color name                                 | `red`     | Error color                 |
| `success_color`   | color name                                 | `green`   | Success color               |

---

## Prompt Styles

**minimal:**

```
~/projects (main) >
```

**powerline:**

```
~/projects main ok
```

**simple:**

```
user@host:~/projects$
```

**custom:** Uses the `prompt` setting with placeholders.

### Custom Prompt Placeholders

| Placeholder | Description                   |
| ----------- | ----------------------------- |
| `{cwd}`     | Current directory (shortened) |
| `{user}`    | Username                      |
| `{host}`    | Hostname                      |
| `{branch}`  | Git branch name               |
| `{git}`     | Git branch in parentheses     |
| `{exit}`    | Last exit code                |

---

## Aliases

Define command aliases in config.toml:

```toml
alias.ll = "ls -la"
alias.la = "@ls -a"
alias.g = "git"
alias.gst = "git status"
alias.gco = "git checkout"
```

Aliases expand before command execution. They can reference both raw and structured commands.

---

## rc (Startup Commands)

The `rc` file runs shell commands at startup:

```sh
# Environment variables
export EDITOR=vim
export PATH="$HOME/bin:$PATH"
```

---

## Structured Commands

### @ls Options

| Flag | Description                    |
| ---- | ------------------------------ |
| `-a` | Show hidden files              |
| `-l` | Long format                    |
| `-h` | Human-readable sizes (default) |
| `-S` | Sort by size                   |
| `-r` | Reverse sort                   |

### Filter Syntax

```bash
@ls | filter { .size > 1MB }
@ls | filter { .kind == dir }
@ls | filter { .name contains config }
```

**Operators:** `==`, `!=`, `>`, `>=`, `<`, `<=`, `contains`

**Size units:** `B`, `KB`, `MB`, `GB`

### @env Options

The `@env` command outputs structured data with environment variables.

**Fields:** `.Variable`, `.Value`

```bash
@env                                      # List all environment variables
@env | filter { .Variable contains PATH } # Filter variables by name
```

### Sort and Select

```bash
@ls | sort .size
@ls | select .name .size
@env | filter { .Variable contains PATH } | select .Value
```

---

## Troubleshooting

### Config Not Loading

1. Check path: `~/.config/Aeth/config.toml`
2. Verify syntax (key = value format)

### Terminal Issues

If terminal is broken after exit, run `reset` in your normal shell.

---

## See Also

-   [Architecture Overview](ARCHITECTURE.md)
