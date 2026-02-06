# Experiment

This is a part Claude Code experiment, proting the https://github.com/pichichi91/phitodo into different languages:
* https://github.com/serebrov/phitodo-rust - rust
* https://github.com/serebrov/phitodo-lisp - common lisp
* https://github.com/serebrov/phitodo-go - go
* https://github.com/serebrov/phitodo-python - python
* https://github.com/serebrov/phitodo-ocaml - ocaml
* https://github.com/serebrov/phitodo-zig - zig

Overall, it worked the best for Rust, the app was built relatively quickly and without much of issues.
Go and python are fine (had some issues, but mostly worked).
Lisp had some problems, but fixed more or less easily.
Zig and Ocaml were the hardest (lots of issues, not working properly even after rounds of fixes).

# phitodo-tui-lisp

A terminal UI clone of phitodo task management, built with Common Lisp (SBCL) using croatoan (ncurses).

A port of [phitodo-tui](https://github.com/serebrov/phitodo-rust) (Rust) to Common Lisp.
Ported by Claude Code.

## Features

- Full task management (create, edit, complete, delete)
- Multiple views: Inbox, Today, Upcoming, Anytime, Completed, Review
- Project organization
- **GitHub integration** with automatic task sync:
  - Assigned issues, your PRs, and review requests become tasks
  - Auto-creates projects per repository
  - Tasks auto-complete when GitHub items are closed
  - Distinct task kinds: `[ISS]` issues, `[PR]` your PRs, `[REV]` review requests
- Inline task editing in detail pane
- Multi-line notes editing
- **Readline-style text editing** with Emacs keybindings (Ctrl-A/E, Ctrl-K, Ctrl-Y, etc.)
- Vim-like keyboard navigation
- Open task URLs directly in browser
- Configurable settings

## Requirements

- SBCL (Steel Bank Common Lisp)
- Quicklisp
- ncurses library (for croatoan)

## Installation

```bash
# Install Quicklisp if not already installed
# See: https://www.quicklisp.org/beta/

# Clone and run
cd phitodo-tui-lisp
make run
```

Or manually:

```bash
sbcl --load ~/quicklisp/setup.lisp \
     --eval '(push #P"./" asdf:*central-registry*)' \
     --eval '(ql:quickload "phitodo-tui")' \
     --eval '(phitodo-tui:main)'
```

## Keyboard Shortcuts

### View Switching
| Key | View |
|-----|------|
| `1` | Inbox |
| `2` | Today |
| `3` | Upcoming |
| `4` | Anytime |
| `5` | Completed |
| `6` | Review |
| `7` | GitHub |
| `8` | Toggl |
| `9` | Settings |

### Navigation
| Key | Action |
|-----|--------|
| `j/k` or `Down/Up` | Navigate list |
| `h/l` or `Left/Right` | Move between sidebar/list/detail |
| `g/G` | Go to first/last item |
| `Tab` | Cycle focus forward |
| `Enter` | Edit task (in list) / Save (in detail) |

### Task Actions
| Key | Action |
|-----|--------|
| `Space` | Toggle task completion |
| `n` | New task |
| `e` | Edit selected task |
| `d` | Delete task |
| `o` | Open task URL in browser |

### Detail Pane (Edit Mode)
| Key | Action |
|-----|--------|
| `Tab` | Next field |
| `Enter` | Save changes (or newline in Notes field) |
| `Escape` | Cancel editing |
| `Space` | Cycle value (for select fields) |
| `Left/Right` | Move cursor (in text fields) |

### Text Editing (Readline-style)
| Key | Action |
|-----|--------|
| `Ctrl-A` | Move to beginning of line |
| `Ctrl-E` | Move to end of line |
| `Ctrl-B` | Move backward one character |
| `Ctrl-F` | Move forward one character |
| `Ctrl-K` | Kill text to end of line |
| `Ctrl-U` | Kill text to beginning of line |
| `Ctrl-W` | Kill word backward |
| `Ctrl-Y` | Yank (paste) killed text |
| `Delete` | Delete character forward |
| `Backspace` | Delete character backward |

### Other
| Key | Action |
|-----|--------|
| `r` | Refresh data (also syncs GitHub) |
| `?` | Show/hide help |
| `q` | Quit |

## GitHub Integration

When you configure a GitHub token in Settings and refresh (`r`), the app will:

1. **Sync GitHub items to tasks**:
   - Issues assigned to you
   - Pull requests you authored
   - PRs where your review is requested

2. **Auto-create projects** for each repository

3. **Track task types** with distinct indicators:
   - `[ISS]` - GitHub issues
   - `[PR]` - Your pull requests
   - `[REV]` - Review requests

4. **Auto-complete tasks** when the corresponding GitHub issue/PR is closed

5. **Store issue description** in task notes

Press `o` on any GitHub-synced task to open it in your browser.

## Configuration

Configuration is stored at `~/.config/phitodo-tui/config.toml`:

```toml
db_path = "/path/to/phitodo.db"
github_token = "ghp_..."
toggl_token = "..."
```

Database is stored at `~/.local/share/phitodo-tui/phitodo.db` by default.

## Views

1. **Inbox** - Tasks with status=inbox
2. **Today** - Tasks due today or overdue
3. **Upcoming** - Tasks with future due dates
4. **Anytime** - Tasks with no due date (active status)
5. **Completed** - Completed tasks
6. **Review** - Overdue tasks needing attention
7. **GitHub** - Shows synced GitHub items by category
8. **Toggl** - Time tracking (placeholder)
9. **Settings** - GitHub token, Toggl token, database path

## Task Fields

- **Title** - Task name
- **Notes** - Multi-line description (Enter adds newline)
- **Due Date** - YYYY-MM-DD format
- **Project** - Associated project
- **Priority** - None, Low, Medium, High
- **Status** - Inbox, Active, Scheduled, Completed, Cancelled
- **Kind** - Task, Bug, Feature, Chore, GitHub types
- **Size** - XS, S, M, L

## Tech Stack

- **Runtime**: SBCL (Steel Bank Common Lisp)
- **TUI Framework**: croatoan (ncurses bindings)
- **Database**: cl-dbi + dbd-sqlite3
- **HTTP Client**: dexador
- **JSON**: yason
- **Date/Time**: local-time
- **Config**: toml (via cl-toml)
- **Build System**: ASDF

## Project Structure

```
phitodo-tui-lisp/
├── phitodo-tui.asd      # ASDF system definition
├── Makefile             # Build/run commands
├── src/
│   ├── packages.lisp    # Package definitions
│   ├── main.lisp        # Entry point
│   ├── models/
│   │   ├── task.lisp    # Task model
│   │   ├── project.lisp # Project model
│   │   └── tag.lisp     # Tag model
│   ├── db/
│   │   ├── schema.lisp  # Database schema
│   │   └── repository.lisp # CRUD operations
│   ├── config/
│   │   └── config.lisp  # Configuration loading
│   ├── services/
│   │   ├── filter.lisp  # Task filtering
│   │   └── github.lisp  # GitHub API integration
│   └── ui/
│       ├── app.lisp     # Main application logic
│       ├── components.lisp # UI components
│       ├── views.lisp   # View implementations
│       └── sidebar.lisp # Sidebar navigation
```

## License

MIT
