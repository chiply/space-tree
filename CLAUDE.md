# space-tree

Emacs Lisp package for tree-based workspace management.

## Build & Test

```sh
eask install-deps --dev   # install dependencies
eask compile              # byte compile
eask test ert test/space-tree-test.el  # run tests
```

## Lint

CI runs all four linters. Run them all locally before pushing:

```sh
eask lint package
eask lint checkdoc
eask lint elisp-lint
eask lint relint
```

## Git Workflow

- Never push directly to `main` — always work on a feature branch
- Branch from `main` and open a PR for all changes
- Use conventional commits (`feat:`, `fix:`, `chore:`) — release-please automates versioning
- Public symbols: `space-tree-` prefix; private symbols: `space-tree--` prefix
- Docstrings on all public functions and variables
- `;;; -*- lexical-binding: t; -*-` on every source file

## Code Review Configuration

Used by the `/review-loop` skill.

### Pre-flight
- Compile: `eask compile`
- Lint: `eask lint package && eask lint checkdoc && eask lint elisp-lint && eask lint relint`
- Tests: `eask test ert test/space-tree-test.el`

### Local Review
- Severity threshold: ignore "nitpick"
- Max iterations: 3

### CI
- Platform: GitHub Actions
- Expected workflows and jobs:
  - `CI / test` — runs on matrix: (ubuntu-latest + windows-latest) x (Emacs 28.2 + 29.4 + 30.2 + snapshot) = 8 jobs
  - `CI / lint` — runs on ubuntu-latest with Emacs 29.4
- All 9 jobs must pass before proceeding to remote review
- No known flaky tests

### Copilot Review
- Max iterations: 3
