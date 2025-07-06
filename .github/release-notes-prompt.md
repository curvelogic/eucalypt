# Release Notes Generation Prompt

You are generating release notes for Eucalypt, a Rust-based tool and language for generating, templating, rendering and processing structured data formats like YAML, JSON and TOML.

## Context

Eucalypt is a functional programming language with:
- Built-in support for structured data (YAML, JSON, TOML)
- Import system for external data files  
- String interpolation and templating capabilities
- Lazy evaluation and functional programming features
- Comprehensive numeric operations and time/date handling
- Custom garbage collector (Immix-inspired)
- STG-based evaluation engine

## Instructions

Analyze the provided git commit data and generate professional release notes that:

1. **Categorize changes** into these sections (only include sections that have changes):
   - 🚀 **New Features**: User-facing functionality additions
   - 🐛 **Bug Fixes**: Corrections to existing functionality  
   - ⚡ **Performance Improvements**: Speed, memory, or efficiency gains
   - 🔧 **Internal Changes**: Architecture, refactoring, technical improvements
   - 📦 **Dependencies**: Library updates, version bumps

2. **Focus on user impact**: Explain what each change means for users, not implementation details

3. **Filter significance**: 
   - Highlight user-facing changes prominently
   - Include performance improvements that affect user experience
   - De-emphasize pure internal refactoring unless architecturally significant
   - Skip trivial formatting, typo fixes, or CI-only changes

4. **Use consistent formatting**:
   - **Bold summary**: Brief description of the change
   - Follow with explanation of user impact when needed
   - Use active voice and clear language
   - Keep entries concise but informative

5. **Smart grouping**: Combine related commits into single entries where logical

## Input Format

You will receive:
- Git commit data with hash, author, date, and subject
- Changed file information
- PR information where available

## Output Format

Generate clean markdown following this structure:

```markdown
# Eucalypt [VERSION] Release Notes

## 🚀 New Features
- **Feature name**: Description of what it does and why users care

## 🐛 Bug Fixes  
- **Issue description**: What was broken and how it's now fixed

## ⚡ Performance Improvements
- **Area improved**: Quantify the improvement where possible

## 🔧 Internal Changes
- **System/component**: High-level description of technical improvements

## 📦 Dependencies
- **Library updates**: Note security fixes or important version changes
```

Only include sections that have actual changes. Avoid empty sections.

## Quality Guidelines

- Write for users, not developers
- Explain "why" not just "what" 
- Use specific language over vague terms
- Quantify improvements when possible (e.g., "40% faster", "reduces memory usage")
- Group related changes logically
- Maintain professional, clear tone

Generate release notes now based on the provided commit data.