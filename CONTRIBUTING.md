# Contributing to rdataviz

Thank you for your interest in contributing to rdataviz! This document provides guidelines for contributing.

## How to Contribute

### Reporting Issues

- Check existing issues before creating a new one
- Include a minimal reproducible example when reporting bugs
- Use issue templates when available

### Pull Requests

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/your-feature`
3. Make your changes following the style guide below
4. Add tests for new functionality
5. Run `devtools::check()` to ensure no errors
6. Submit a pull request

### Development Setup

```r
# Install development dependencies
install.packages(c("devtools", "testthat", "roxygen2", "pkgdown"))

# Clone and install
git clone https://github.com/user/rdataviz.git
devtools::install_dev_deps()
devtools::load_all()
```

## Style Guide

- Follow the [tidyverse style guide](https://style.tidyverse.org/)
- Use `snake_case` for function and variable names
- Use roxygen2 for documentation
- Include `@examples` in all exported functions

## Testing

- Write tests using testthat
- Aim for >80% code coverage
- Run tests with `devtools::test()`

## Documentation

- Document all exported functions with roxygen2
- Update vignettes when adding features
- Include examples that work without external data

## Questions?

Open an issue with the "question" label.
