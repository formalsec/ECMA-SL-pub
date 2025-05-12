# Contributing

Thank you for your interest in contributing!
We follow a trunk-based development methodology to ensure fast iteration and continuous integration.

## Development Workflow

- All changes should be small, incremental, and merged directly into the `main` branch via pull requests.
- Feature branches are discouraged. Instead, create a pull request to merge directly into `main` whenever possible.
- Large changes should be broken down into smaller, independent steps that can be merged separately.

## Continuous Integration

- The `main` branch should ideally remain green (passing CI), but breaking changes may be introduced if necessary.
- If CI is failing due to an intentional breaking change, ensure there is a clear path to resolution in subsequent commits.
- Fixes should be prioritised to restore CI to a passing state as soon as possible.

## Code Quality and Review

- All contributions should be reviewed before merging.
- Keep changes minimal and focused on a single improvement or fix.
- Write clear commit messages that describe what and why, rather than how.

## Getting Started

1. Clone the repository:
   ```sh
   git clone git@github.com:formalsec/ECMA-SL.git
   cd ECMA-SL
   ```

2. Create a new branch for your changes:
   ```sh
   git checkout -b my-feature-branch
   ```

3. Make your changes and commit them:
   ```sh
   git add .
   git commit -m "Brief description of change"
   ```

4. Push your branch and create a pull request:
   ```sh
   git push origin my-feature-branch
   ```

5. Once approved, your changes will be merged into `main`.

6. If your change requires additional context, document it in a relevant place (e.g., an issue, a README update, or inline comments).

## Reporting Issues

If you encounter a bug or have a feature request, please open an issue with:
- A clear description of the problem.
- Steps to reproduce (if applicable).
- Any relevant logs or error messages.
