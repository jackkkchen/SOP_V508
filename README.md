# Accessibility Demo - Health Data Visualization

## Overview
An accessible Shiny application demonstrating health data visualization with comprehensive accessibility features. This demo showcases how to build web applications that are usable by everyone, including people with disabilities.

## Features

### Accessibility Features
- **Keyboard Navigation**
  - `Tab`: Navigate through all interactive elements
  - `Alt + T`: Toggle dark/light mode
  - `Alt + H`: Toggle high contrast mode
  - `Alt + ↑/↓`: Adjust font size
  - `Enter/Space`: Activate buttons and controls

- **Visual Accessibility**
  - Dark/Light mode for different lighting preferences
  - High contrast mode for better readability
  - Adjustable font size (80-150%)
  - Color scheme following accessibility guidelines
  - Clear visual hierarchy and spacing

- **Screen Reader Support**
  - ARIA labels for all interactive elements
  - Semantic HTML structure
  - Meaningful headings and landmarks
  - Clear focus indicators

### Core Functionality
- Interactive health data visualization
- State-wise health metrics comparison
- Search and filter capabilities
- Responsive layout design

## Getting Started

### Installation
```R
# Install required packages
install.packages(c(
  "shiny",
  "bslib",
  "shinyjs",
  "shinyWidgets",
  "ggplot2",
  "dplyr",
  "tidyr"
))

# Run the application
shiny::runApp("app.R")
```

### Basic Usage
1. Launch the application
2. Use the sidebar controls to:
   - Search for specific states
   - Select health metrics to compare
   - Adjust visualization settings
3. Use keyboard shortcuts for quick access to accessibility features
4. Explore the data visualization with screen reader support

## Accessibility Design Principles
This demo implements the following accessibility principles:
- Perceivable: Information can be perceived regardless of user abilities
- Operable: Interface can be operated through various input methods
- Understandable: Content and operation are clear and consistent
- Robust: Content can be interpreted by various assistive technologies
