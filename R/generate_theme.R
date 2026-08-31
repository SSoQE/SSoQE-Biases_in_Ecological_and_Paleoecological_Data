#!/usr/bin/env Rscript

# Theme Generation Script for SSOQE Presentation
# This script automatically generates SCSS files from JSON configuration
# It runs before every Quarto render via the pre-render hook

library(here)
library(jsonlite)
library(purrr)

# Helper function to calculate luminance and determine contrast color
calculate_contrast_color <- function(hex_color) {
  # Remove # if present
  hex_color <- gsub("#", "", hex_color)

  # Convert hex to RGB
  r <- as.numeric(paste0("0x", substr(hex_color, 1, 2))) / 255
  g <- as.numeric(paste0("0x", substr(hex_color, 3, 4))) / 255
  b <- as.numeric(paste0("0x", substr(hex_color, 5, 6))) / 255

  # Calculate relative luminance using sRGB formula
  luminance <- 0.2126 * r + 0.7152 * g + 0.0722 * b

  # Return black for light backgrounds, white for dark backgrounds
  if (luminance > 0.5) {
    return("$black") # Use black text on light backgrounds
  } else {
    return("$white") # Use white text on dark backgrounds
  }
}

# Function to generate colors SCSS from JSON
generate_colors_scss <- function(
    input_file = here::here("Presentation/colors.json"),
    output_file = here::here("Presentation/_colors.scss")) {
  message("Generating _colors.scss...\n")

  # Check if colors.json exists
  if (
    !file.exists(input_file)
  ) {
    stop("colors.json not found. Please create this file first.")
  }

  # Read colors from JSON
  colors_data <- jsonlite::fromJSON(input_file)

  # Handle both old format (flat) and new format (nested)
  if (
    "primary" %in% names(colors_data)
  ) {
    # New nested format
    primary_colors <- colors_data$primary
    semantic_colors <- colors_data$semantic

    # For utility class generation, we need resolved hex values
    resolved_semantic_colors <- list()
    for (semantic_name in names(semantic_colors)) {
      semantic_value <- semantic_colors[[semantic_name]]
      if (semantic_value %in% names(primary_colors)) {
        resolved_semantic_colors[[semantic_name]] <- primary_colors[[semantic_value]]
      } else {
        resolved_semantic_colors[[semantic_name]] <- semantic_value
      }
    }

    all_colors <- c(primary_colors, resolved_semantic_colors)
  } else {
    # Old flat format (backward compatibility)
    primary_colors <- colors_data
    semantic_colors <- list()
    resolved_semantic_colors <- list()
    all_colors <- colors_data
  }

  # First, generate all color variable definitions
  color_variables <-
    purrr::imap(
      .x = all_colors,
      .f = ~ {
        paste0(
          "$", .y, ": ", .x, ";"
        )
      }
    ) |>
    paste(collapse = "\n")

  # Then, generate all utility classes
  color_classes <-
    purrr::imap(
      .x = all_colors,
      .f = ~ {
        # Calculate contrast color using the hex value
        contrast_color <-
          calculate_contrast_color(.x)

        paste0(
          "// Color: ", .y, "\n",
          ".reveal .bg-", .y, " { background-color: ", .x, "; }\n",
          ".text-color-", .y, " { color: ", .x, " !important; }\n",
          ".text-background-", .y, " {\n",
          "  background-color: ", .x, ";\n",
          "  padding: $smallMargin;\n",
          "  border-radius: 5px;\n",
          "}\n",
          ".text-highlight-", .y, " {\n",
          "  background-color: ", .x, ";\n",
          "  color: ", contrast_color, ";\n",
          "  padding: 2px 4px;\n",
          "  border-radius: 3px;\n",
          "}\n"
        )
      }
    ) |>
    paste(collapse = "\n")

  # Combine color variables and classes
  colors_definition <-
    paste0(
      "// Color definitions\n",
      color_variables, "\n",
      "\n", "// Color classes\n",
      color_classes
    )

  # Add semantic color mappings at the end
  semantic_mappings <-
    paste0(
      "\n// SSoQE Brand Guidelines - direct color usage\n",
      "$body-color: $black !default;\n",
      "$backgroundColor: $white !default;\n",
      "$headingColor: $midnightGreen !default;\n",
      "$link-color: $persianGreen !default;\n",
      "$selection-bg: $persianGreen !default;\n",
      "$code-color: $black !default;\n",
      "$accent-color: $satinSheenGold !default;\n",
      "\n// HTML-specific color variables for compatibility\n",
      "$body-bg: $backgroundColor !default;\n",
      "$link-hover-color: darken($persianGreen, 15%) !default;\n",
      "\n// Additional SSoQE brand utilities\n",
      ".text-color-body { color: $body-color !important; }\n",
      ".text-color-background { color: $backgroundColor !important; }\n",
      ".text-color-heading { color: $headingColor !important; }\n",
      ".text-color-link { color: $link-color !important; }\n",
      ".text-color-accent { color: $accent-color !important; }\n"
    )

  # Write to _colors.scss
  writeLines(
    text = c(
      "// This file is auto-generated from colors.json. Do not edit directly.\n",
      colors_definition,
      semantic_mappings
    ),
    con = output_file
  )

  message("_colors.scss generated successfully\n")
}

# Function to generate fonts SCSS from JSON
generate_fonts_scss <- function(
    input_file = here::here("Presentation/fonts.json"),
    output_file = here::here("Presentation/_fonts.scss"),
    use_html_sizes = FALSE) {
  message("Generating _fonts.scss...\n")

  # Check if fonts.json exists
  if (
    !file.exists(input_file)
  ) {
    stop("fonts.json not found. Please create this file first.")
  }

  # Read fonts from JSON
  fonts <- jsonlite::fromJSON(input_file)

  # Choose which size configuration to use
  if (
    use_html_sizes && !is.null(fonts$htmlSizes)
  ) {
    size_config <- fonts$htmlSizes
    size_comment <- "// Font sizes optimized for HTML documents"
  } else {
    size_config <- fonts$sizes
    size_comment <- "// Font sizes from fonts.json"
  }

  # Generate SCSS font definitions
  fonts_definition <-
    c(
      "// This file is auto-generated from fonts.json. Do not edit directly.\n",
      paste0('$mainFont: "', fonts$body, '", "Arial", sans-serif !default;\n'),
      paste0('$headingFont: "', fonts$heading, '", "Arial", sans-serif !default;\n'),
      paste0('$monospaceFont: "', fonts$monospace, '", "Courier New", monospace !default;\n'),
      "\n",
      size_comment,
      paste0("$mainFontSize: ", size_config$mainFontSize, " !default;"),
      paste0("$heading1Size: ", size_config$heading1Size, " !default;"),
      paste0("$heading2Size: ", size_config$heading2Size, " !default;"),
      paste0("$heading3Size: ", size_config$heading3Size, " !default;"),
      paste0("$heading4Size: ", size_config$heading4Size, " !default;"),
      paste0("$body-line-height: ", size_config$bodyLineHeight, " !default;"),
      paste0("$headingLineHeight: ", size_config$headingLineHeight, " !default;"),
      "\n",
      "// Font weights from fonts.json",
      paste0("$headingFontWeight: ", fonts$weights$headingFontWeight, " !default;"),
      paste0("$bodyFontWeight: ", fonts$weights$bodyFontWeight, " !default;"),
      paste0("$boldFontWeight: ", fonts$weights$boldFontWeight, " !default;"),
      "\n",
      "// Font spacing from fonts.json",
      paste0("$headingLetterSpacing: ", fonts$spacing$headingLetterSpacing, " !default;"),
      paste0("$bodyLetterSpacing: ", fonts$spacing$bodyLetterSpacing, " !default;"),
      "\n",
      "// HTML-specific font variables for compatibility",
      "$font-family-sans-serif: $mainFont !default;",
      "$headings-font-family: $headingFont !default;",
      "$font-family-monospace: $monospaceFont !default;",
      "$font-size-base: $mainFontSize !default;",
      "$line-height-base: $body-line-height !default;",
      "$headings-line-height: $headingLineHeight !default;",
      "$headings-font-weight: $headingFontWeight !default;",
      "$headings-margin-bottom: 1rem !default;",
      if (use_html_sizes && !is.null(size_config$maxWidth)) {
        paste0("$content-max-width: ", size_config$maxWidth, " !default;")
      } else {
        "$content-max-width: 900px !default;"
      },
      if (use_html_sizes && !is.null(size_config$blockMargin)) {
        paste0("$block-margin: ", size_config$blockMargin, " !default;")
      } else {
        "$block-margin: 1.5rem !default;"
      },
      "\n",
      "// Spacing variables used in utility classes",
      if (use_html_sizes && !is.null(size_config$smallMargin)) {
        paste0("$smallMargin: ", size_config$smallMargin, " !default;")
      } else {
        "$smallMargin: 5px !default;"
      },
      "$largeMargin: 20px !default;",
      "\n",
      "// Utility classes for font families",
      ".text-font-body { font-family: $mainFont; }",
      ".text-font-heading { font-family: $headingFont; }",
      ".text-font-monospace { font-family: $monospaceFont; }",
      "\n",
      "// Utility classes for font sizes",
      ".text-size-main { font-size: $mainFontSize !important; }",
      ".text-size-heading1 { font-size: $heading1Size !important; }",
      ".text-size-heading2 { font-size: $heading2Size !important; }",
      ".text-size-heading3 { font-size: $heading3Size !important; }",
      ".text-size-heading4 { font-size: $heading4Size !important; }",
      ".text-size-body { font-size: $mainFontSize !important; }",
      if (!use_html_sizes) {
        c(
          paste0(".text-smaller { font-size: calc($mainFontSize * ", fonts$sizes$textSizeSmall, ") !important; }"),
          paste0(".text-tiny { font-size: calc($mainFontSize * ", fonts$sizes$textSizeTiny, ") !important; }"),
          paste0(".text-larger { font-size: calc($mainFontSize * ", fonts$sizes$textSizeLarge, ") !important; }")
        )
      } else {
        c(
          ".text-smaller { font-size: calc($mainFontSize * 0.8) !important; }",
          ".text-tiny { font-size: calc($mainFontSize * 0.7) !important; }",
          ".text-larger { font-size: calc($mainFontSize * 1.2) !important; }"
        )
      },
      "\n",
      "/* Debug font loading - this will help us see if fonts are loaded */",
      '@supports (font-family: "Space Grotesk") {',
      "  .debug-font-heading::before {",
      '    content: "Space Grotesk font is supported";',
      "    display: block;",
      "    font-size: 12px;",
      "    color: green;",
      "  }",
      "}",
      '@supports (font-family: "JetBrains Mono") {',
      "  .debug-font-mono::before {",
      '    content: "JetBrains Mono font is supported";',
      "    display: block;",
      "    font-size: 12px;",
      "    color: green;",
      "  }",
      "}"
    )

  # Write to _fonts.scss
  writeLines(
    text = c(
      fonts_definition
    ),
    con = output_file
  )

  message("_fonts.scss generated successfully\n")
}

# Function to generate exercise SCSS theme for HTML documents
generate_exercise_scss <- function(
    colors_file = here::here("Presentation/colors.json"),
    fonts_file = here::here("Presentation/fonts.json"),
    output_file = here::here("R/Exercises/_exercise_theme.scss")) {
  message("Generating _exercise_theme.scss...\n")

  # Check if input files exist
  if (!file.exists(colors_file)) {
    stop("colors.json not found. Please create this file first.")
  }
  if (!file.exists(fonts_file)) {
    stop("fonts.json not found. Please create this file first.")
  }

  # Generate exercise-specific SCSS content
  exercise_scss_content <- c(
    "// This file is auto-generated from colors.json and fonts.json. Do not edit directly.",
    "// SSoQE Exercise Theme for HTML Documents",
    "",
    "/*-- scss:defaults --*/",
    "",
    "// Define spacing variables before imports (needed by color utility classes)",
    "$smallMargin: 0.5rem !default;",
    "$blockMargin: 1.5rem !default;",
    "$largeMargin: 20px !default;",
    "",
    "// Import local color and font definitions",
    '@import "_colors";',
    '@import "_fonts";',
    "",
    "/*-- scss:rules --*/",
    "",
    generate_html_body_styles(),
    "",
    generate_html_heading_styles(),
    "",
    generate_html_link_styles(),
    "",
    generate_exercise_code_styles(),
    "",
    generate_exercise_quarto_fixes(),
    "",
    generate_html_blockquote_styles(),
    "",
    generate_html_table_styles(),
    "",
    generate_html_utility_classes(),
    "",
    generate_html_responsive_styles()
  )

  # Write to file
  writeLines(
    text = exercise_scss_content,
    con = output_file
  )

  message("_exercise_theme.scss generated successfully\n")
}

# Function to generate custom theme SCSS for presentations
generate_custom_theme_scss <- function(
    colors_file = here::here("Presentation/colors.json"),
    fonts_file = here::here("Presentation/fonts.json"),
    custom_theme_file = here::here("Presentation/custom_theme.json"),
    output_file = here::here("Presentation/custom_theme.scss")) {
  message("Generating custom_theme.scss...\n")

  # Check if input files exist
  if (
    !file.exists(colors_file)
  ) {
    stop("colors.json not found. Please create this file first.")
  }
  if (
    !file.exists(fonts_file)
  ) {
    stop("fonts.json not found. Please create this file first.")
  }
  if (
    !file.exists(custom_theme_file)
  ) {
    stop("custom_theme.json not found. Please create this file first.")
  }

  # Read configuration from JSON files
  colors <- jsonlite::fromJSON(colors_file)
  fonts <- jsonlite::fromJSON(fonts_file)
  custom_theme <- jsonlite::fromJSON(custom_theme_file)

  # Generate custom theme SCSS content
  custom_theme_scss_content <-
    c(
      "// This file is auto-generated from colors.json, fonts.json, and custom_theme.json. Do not edit directly.",
      "// SSoQE Custom Theme for Reveal.js Presentations",
      "",
      "/*-- scss:defaults --*/",
      "",
      generate_custom_theme_defaults(custom_theme, fonts),
      "",
      "// Colors are now loaded from _colors.scss, which is auto-generated from colors.json.",
      "// Do not define color variables here. Edit colors.json and regenerate _colors.scss if needed.",
      '@import "_colors";',
      "",
      "// Background of the presentation",
      "// All color assignments use direct color names from colors.json",
      "",
      "// Typography is now loaded from _fonts.scss, which is auto-generated from fonts.json.",
      "// Do not define font variables here. Edit fonts.json and regenerate _fonts.scss if needed.",
      '@import "_fonts";',
      "",
      generate_custom_theme_font_overrides(fonts),
      "",
      "// Layout",
      paste0("$content-max-width: ", custom_theme$layout$contentMaxWidth, ";"),
      "",
      "/*-- scss:rules --*/",
      "",
      generate_custom_theme_body_styles(custom_theme),
      "",
      generate_custom_theme_code_styles(custom_theme, colors),
      "",
      generate_custom_theme_scrollbar_styles(),
      "",
      generate_custom_theme_quarto_fixes(),
      "",
      generate_custom_theme_link_styles(),
      "",
      generate_custom_theme_heading_styles(custom_theme),
      "",
      generate_custom_theme_list_styles(custom_theme),
      "",
      generate_custom_theme_utility_classes(custom_theme),
      "",
      generate_custom_theme_slide_layout(custom_theme),
      "",
      generate_custom_theme_image_styles(),
      "",
      generate_custom_theme_specialized_sections(),
      "",
      generate_custom_theme_blockquote_styles(custom_theme),
      "",
      generate_custom_theme_code_block_styles(custom_theme),
      "",
      generate_custom_theme_table_styles(custom_theme),
      "",
      generate_custom_theme_exercise_overrides()
    )

  # Write to file
  writeLines(
    text = custom_theme_scss_content,
    con = output_file
  )

  message("custom_theme.scss generated successfully\n")
}

# ==============================================================================
# HELPER FUNCTIONS FOR EXERCISE THEME GENERATION
# ==============================================================================

generate_exercise_code_styles <- function() {
  c(
    "// Code styling",
    "code {",
    "  background-color: $midnightGreen !important;",
    "  color: $satinSheenGold !important;",
    "  font-family: $font-family-monospace;",
    "  font-style: italic;",
    "  padding: 0.125rem 0.25rem;",
    "  border-radius: 0.25rem;",
    "  font-size: 0.875em;",
    "}",
    "",
    "pre {",
    "  background-color: $cambridgeBlue !important;",
    "  border-radius: 0.375rem;",
    "  padding: 1rem;",
    "  margin-bottom: $block-margin;",
    "  overflow-x: auto;",
    "",
    "  code {",
    "    background-color: transparent !important;",
    "    color: inherit !important; // Let Quarto handle syntax highlighting colors",
    "    font-style: normal !important; // Override italic from inline code",
    "    border: none;",
    "    padding: 0;",
    "    font-size: 0.875rem;",
    "  }",
    "}"
  )
}

generate_exercise_quarto_fixes <- function() {
  c(
    "// Fix Quarto code block positioning and container issues",
    ".cell-code, .sourceCode {",
    "  margin: 0 !important;",
    "  padding: 0 !important;",
    "  position: relative !important;",
    "  left: 0 !important;",
    "  right: 0 !important;",
    "}",
    "",
    ".cell-code pre, .sourceCode pre {",
    "  margin: 0 !important;",
    "  padding: 1rem !important;",
    "  background-color: $cambridgeBlue !important;",
    "  border-radius: 0.375rem;",
    "  overflow-x: auto;",
    "}",
    "",
    "// Hide the cell-code label that's appearing",
    ".cell-code::before, .sourceCode::before {",
    "  display: none !important;",
    "}",
    "",
    "// Ensure proper code block styling",
    ".cell-code code, .sourceCode code {",
    "  background-color: transparent !important;",
    "  color: inherit !important; // Let Quarto handle syntax highlighting colors",
    "  font-style: normal !important; // Override italic from inline code",
    "  padding: 0 !important;",
    "  border: none !important;",
    "}",
    "",
    "// Additional Quarto-specific code element fixes",
    ".sourceCode code {",
    "  background-color: transparent !important;",
    "  color: inherit !important;",
    "  font-style: normal !important; // Override italic from inline code",
    "}"
  )
}

# ==============================================================================
# SHARED HELPER FUNCTIONS FOR HTML STYLING
# ==============================================================================

# Shared helper functions for HTML styling
generate_color_variables <- function(colors_data) {
  # Handle both old format (flat) and new format (nested)
  if (
    "primary" %in% names(colors_data)
  ) {
    # New nested format
    primary_colors <- colors_data$primary
    semantic_colors <- colors_data$semantic

    primary_vars <-
      c(
        "// SSoQE Brand Colors",
        paste0("$black: ", primary_colors$black, ";"),
        paste0("$white: ", primary_colors$white, ";"),
        paste0("$midnightGreen: ", primary_colors$midnightGreen, ";"),
        paste0("$persianGreen: ", primary_colors$persianGreen, ";"),
        paste0("$cambridgeBlue: ", primary_colors$cambridgeBlue, ";"),
        paste0("$satinSheenGold: ", primary_colors$satinSheenGold, ";")
      )

    # Generate semantic color variables, resolving references to primary colors
    semantic_vars <-
      c(
        "",
        "// Semantic Colors"
      )

    for (semantic_name in names(semantic_colors)) {
      semantic_value <- semantic_colors[[semantic_name]]

      # If the semantic color references a primary color, use the primary color
      if (
        semantic_value %in% names(primary_colors)
      ) {
        semantic_vars <- c(semantic_vars, paste0("$", semantic_name, ": $", semantic_value, ";"))
      } else {
        # If it's a hex value, use it directly
        semantic_vars <- c(semantic_vars, paste0("$", semantic_name, ": ", semantic_value, ";"))
      }
    }

    return(
      c(primary_vars, semantic_vars)
    )
  } else {
    # Old flat format (backward compatibility)
    return(
      c(
        "// SSoQE Brand Colors",
        paste0("$black: ", colors_data$black, ";"),
        paste0("$white: ", colors_data$white, ";"),
        paste0("$midnightGreen: ", colors_data$midnightGreen, ";"),
        paste0("$persianGreen: ", colors_data$persianGreen, ";"),
        paste0("$cambridgeBlue: ", colors_data$cambridgeBlue, ";"),
        paste0("$satinSheenGold: ", colors_data$satinSheenGold, ";")
      )
    )
  }
}

# Note: generate_html_typography, generate_html_font_sizes, generate_html_layout,
# and generate_html_semantic_colors functions have been removed as they duplicate
# functionality now provided by shared _colors.scss and _fonts.scss

# Helper function to generate HTML body styles
generate_html_body_styles <- function() {
  c(
    "// Main content styling",
    "body {",
    "  font-family: $font-family-sans-serif;",
    "  font-size: $font-size-base;",
    "  line-height: $line-height-base;",
    "  color: $body-color;",
    "  background-color: $body-bg;",
    "}",
    "",
    "// Content width constraint",
    ".quarto-container {",
    "  max-width: $content-max-width;",
    "}"
  )
}

# Helper function to generate HTML heading styles
generate_html_heading_styles <- function() {
  c(
    "// Heading styles with SSoQE brand colors",
    "h1, .h1 {",
    "  color: $midnightGreen;",
    "  font-family: $headings-font-family;",
    "  font-weight: $headings-font-weight;",
    "  line-height: $headings-line-height;",
    "  margin-bottom: $headings-margin-bottom;",
    "}",
    "",
    "h2, .h2 {",
    "  color: $persianGreen;",
    "  font-family: $headings-font-family;",
    "  font-weight: $headings-font-weight;",
    "  line-height: $headings-line-height;",
    "  margin-bottom: $headings-margin-bottom;",
    "}",
    "",
    "h3, .h3 {",
    "  color: $midnightGreen;",
    "  font-family: $headings-font-family;",
    "  font-weight: $headings-font-weight;",
    "  line-height: $headings-line-height;",
    "  margin-bottom: $headings-margin-bottom;",
    "}",
    "",
    "h4, .h4, h5, .h5, h6, .h6 {",
    "  color: $black;",
    "  font-family: $headings-font-family;",
    "  font-weight: $headings-font-weight;",
    "  line-height: $headings-line-height;",
    "  margin-bottom: $headings-margin-bottom;",
    "}"
  )
}

# Helper function to generate HTML link styles
generate_html_link_styles <- function() {
  c(
    "// Link styling",
    "a {",
    "  color: $link-color;",
    "  text-decoration: none;",
    "  transition: color 0.2s ease;",
    "",
    "  &:hover {",
    "    color: $link-hover-color;",
    "    text-decoration: underline;",
    "  }",
    "}"
  )
}

# Helper function to generate HTML blockquote styles
generate_html_blockquote_styles <- function() {
  c(
    "// Blockquote styling",
    "blockquote {",
    "  border-left: 4px solid $persianGreen;",
    "  background-color: rgba($cambridgeBlue, 0.1);",
    "  padding: 1rem;",
    "  margin: 1rem 0;",
    "  margin-bottom: $block-margin;",
    "  font-style: italic;",
    "  border-radius: 0.375rem;",
    "}"
  )
}

# Helper function to generate HTML table styles
generate_html_table_styles <- function() {
  c(
    "// Table styling",
    "table {",
    "  border-collapse: collapse;",
    "  margin-bottom: $block-margin;",
    "  width: 100%;",
    "",
    "  th, td {",
    "    border: 1px solid rgba($black, 0.2);",
    "    padding: 0.75rem;",
    "    text-align: left;",
    "  }",
    "",
    "  th {",
    "    background-color: $midnightGreen !important;",
    "    color: $white !important;",
    "    font-weight: 600;",
    "  }",
    "",
    "  tr:nth-child(odd) {",
    "    background-color: rgba($cambridgeBlue, 0.5) !important; // Very light Cambridge Blue",
    "  }",
    "}"
  )
}

# Helper function to generate HTML utility classes
generate_html_utility_classes <- function() {
  c(
    "// Utility classes for SSoQE brand colors",
    ".text-midnight-green { color: $midnightGreen !important; }",
    ".text-persian-green { color: $persianGreen !important; }",
    ".text-cambridge-blue { color: $cambridgeBlue !important; }",
    ".text-satin-sheen-gold { color: $satinSheenGold !important; }",
    "",
    ".bg-midnight-green { background-color: $midnightGreen !important; }",
    ".bg-persian-green { background-color: $persianGreen !important; }",
    ".bg-cambridge-blue { background-color: $cambridgeBlue !important; }",
    ".bg-satin-sheen-gold { background-color: $satinSheenGold !important; }"
  )
}

# Helper function to generate HTML responsive styles
generate_html_responsive_styles <- function() {
  c(
    "// Responsive adjustments",
    "@media (max-width: 768px) {",
    "  .quarto-container {",
    "    max-width: 100%;",
    "    padding: 0 1rem;",
    "  }",
    "",
    "  h1, .h1 { font-size: 1.75rem; }",
    "  h2, .h2 { font-size: 1.5rem; }",
    "  h3, .h3 { font-size: 1.25rem; }",
    "}"
  )
}

# Function to generate R theme file from JSON
generate_r_theme <- function(
    colors_file = here::here("Presentation/colors.json"),
    fonts_file = here::here("Presentation/fonts.json"),
    output_file = here::here("R/set_r_theme.R")) {
  message("Generating set_r_theme.R...\n")

  # Check if input files exist
  if (
    !file.exists(colors_file)
  ) {
    stop("colors.json not found. Please create this file first.")
  }

  if (
    !file.exists(fonts_file)
  ) {
    stop("fonts.json not found. Please create this file first.")
  }

  # Read colors and fonts from JSON
  colors_data <- jsonlite::fromJSON(colors_file)
  fonts <- jsonlite::fromJSON(fonts_file)

  # Handle both old format (flat) and new format (nested)
  if (
    "primary" %in% names(colors_data)
  ) {
    # New nested format
    colors <- colors_data$primary
  } else {
    # Old flat format (backward compatibility)
    colors <- colors_data
  }

  # Generate R theme content
  r_theme_content <-
    c(
      "# This file is auto-generated from colors.json and fonts.json. Do not edit directly.",
      "# Generated by generate_theme.R based on SSoQE Brand Guidelines v1.1",
      "",
      "#----------------------------------------------------------#",
      "# SSoQE Brand Compliant R Theme Settings",
      "#----------------------------------------------------------#",
      "",
      "# Load required packages for theme",
      'if (!require("ggplot2")) stop("ggplot2 package is required")',
      'if (!require("sysfonts")) stop("sysfonts package is required for custom fonts")',
      'if (!require("showtext")) stop("showtext package is required for custom fonts")',
      "",
      "# Load Google Fonts using sysfonts with error handling",
      paste0("# Try to load ", fonts$body, " (may be named differently in Google Fonts)"),
      "tryCatch({",
      paste0('  sysfonts::font_add_google("', fonts$body, '", "ssoqe_body")'),
      "}, error = function(e) {",
      "  # Try alternative names for Plus Jakarta Sans",
      "  tryCatch({",
      '    sysfonts::font_add_google("Jakarta Sans", "ssoqe_body")',
      "  }, error = function(e2) {",
      "    # Fallback to a similar font",
      '    message("Plus Jakarta Sans not found, using Inter as fallback")',
      '    sysfonts::font_add_google("Inter", "ssoqe_body")',
      "  })",
      "})",
      "",
      paste0("# Load ", fonts$heading),
      "tryCatch({",
      paste0('  sysfonts::font_add_google("', fonts$heading, '", "ssoqe_heading")'),
      "}, error = function(e) {",
      paste0('  message("', fonts$heading, ' not found, using Roboto Condensed as fallback")'),
      '  sysfonts::font_add_google("Roboto Condensed", "ssoqe_heading")',
      "})",
      "",
      paste0("# Load ", fonts$monospace),
      "tryCatch({",
      paste0('  sysfonts::font_add_google("', fonts$monospace, '", "ssoqe_mono")'),
      "}, error = function(e) {",
      paste0('  message("', fonts$monospace, ' not found, using Fira Code as fallback")'),
      '  sysfonts::font_add_google("Fira Code", "ssoqe_mono")',
      "})",
      "",
      "# Enable showtext for rendering custom fonts",
      "showtext::showtext_auto()",
      "",
      "# SSoQE Brand Colors (from brand guidelines)",
      "ssoqe_cols <- c(",
      paste0('  black = "', colors$black, '",'),
      paste0('  white = "', colors$white, '",'),
      paste0('  midnight_green = "', colors$midnightGreen, '",'),
      paste0('  persian_green = "', colors$persianGreen, '",'),
      paste0('  cambridge_blue = "', colors$cambridgeBlue, '",'),
      paste0('  satin_sheen_gold = "', colors$satinSheenGold, '"'),
      ")",
      "",
      "# SSoQE Color Scales for ggplot2",
      "scale_colour_ssoqe <- function(...) ggplot2::scale_colour_manual(values = ssoqe_cols, ...)",
      "scale_fill_ssoqe <- function(...) ggplot2::scale_fill_manual(values = ssoqe_cols, ...)",
      "",
      "# Primary color sequence (for ordered data)",
      paste0('ssoqe_primary_sequence <- c("', colors$midnightGreen, '", "', colors$persianGreen, '", "', colors$cambridgeBlue, '")'),
      "",
      "# Diverging color palette (for diverging data)",
      paste0('ssoqe_diverging <- c("', colors$midnightGreen, '", "', colors$white, '", "', colors$satinSheenGold, '")'),
      "",
      "# Text and background colors",
      paste0('ssoqe_text_color <- "', colors$black, '"'),
      paste0('ssoqe_background_color <- "', colors$white, '"'),
      paste0('ssoqe_accent_color <- "', colors$satinSheenGold, '"'),
      "",
      "# Define typography (based on SSoQE brand guidelines)",
      "# Using registered font names from sysfonts",
      'ssoqe_base_font <- "ssoqe_body"',
      'ssoqe_heading_font <- "ssoqe_heading"',
      'ssoqe_mono_font <- "ssoqe_mono"',
      "",
      "# Define text sizes (following brand hierarchy from fonts.json)",
      "# Convert px to pt: 1px ≈ 0.75pt (at 96 DPI)",
      paste0("text_size_main_px <- ", gsub("px", "", fonts$sizes$mainFontSize)),
      "text_size_main_pt <- text_size_main_px * 0.75  # Convert px to pt",
      "text_size_small <- text_size_main_pt * 0.8",
      "text_size_base <- text_size_main_pt * 0.9",
      "text_size_medium <- text_size_main_pt * 1.0",
      "text_size_large <- text_size_main_pt * 1.2",
      "text_size_xlarge <- text_size_main_pt * 1.4",
      "",
      "# Define line sizes",
      "line_size_thin <- 0.25",
      "line_size_base <- 0.5",
      "line_size_thick <- 1.0",
      "",
      "# Define output dimensions (standard sizes)",
      "image_width <- 16",
      "image_height <- 12",
      'image_units <- "cm"',
      "image_dpi <- 300",
      "",
      "# Create SSoQE-compliant ggplot2 theme",
      "theme_ssoqe <- function(base_size = text_size_base,",
      "                       base_family = ssoqe_base_font,",
      "                       base_line_size = line_size_base,",
      "                       base_rect_size = line_size_base) {",
      "  ggplot2::theme_bw(base_size = base_size,",
      "                   base_family = base_family,",
      "                   base_line_size = base_line_size,",
      "                   base_rect_size = base_rect_size) +",
      "  ggplot2::theme(",
      "    # Text elements",
      paste0('    text = ggplot2::element_text(family = ssoqe_base_font, colour = "', colors$black, '"),'),
      paste0('    plot.title = ggplot2::element_text(family = ssoqe_heading_font, colour = "', colors$midnightGreen, '", face = "bold", size = ggplot2::rel(1.4)),'),
      paste0('    plot.subtitle = ggplot2::element_text(family = ssoqe_base_font, colour = "', colors$black, '", size = ggplot2::rel(1.1)),'),
      paste0('    axis.title = ggplot2::element_text(family = ssoqe_base_font, colour = "', colors$black, '", size = ggplot2::rel(1.0)),'),
      paste0('    axis.text = ggplot2::element_text(family = ssoqe_base_font, colour = "', colors$black, '", size = ggplot2::rel(0.9)),'),
      paste0('    legend.title = ggplot2::element_text(family = ssoqe_base_font, colour = "', colors$black, '", size = ggplot2::rel(1.0)),'),
      paste0('    legend.text = ggplot2::element_text(family = ssoqe_base_font, colour = "', colors$black, '", size = ggplot2::rel(0.9)),'),
      paste0('    strip.text = ggplot2::element_text(family = ssoqe_heading_font, colour = "', colors$midnightGreen, '", face = "bold"),'),
      "    ",
      "    # Background elements",
      paste0('    plot.background = ggplot2::element_rect(fill = "', colors$white, '", colour = NA),'),
      paste0('    panel.background = ggplot2::element_rect(fill = "', colors$white, '", colour = NA),'),
      "    legend.background = ggplot2::element_blank(),",
      "    legend.key = ggplot2::element_blank()",
      "  )",
      "}",
      "",
      "# Set the default ggplot2 theme to SSoQE theme",
      "ggplot2::theme_set(theme_ssoqe())",
      "",
      "# Helper functions for common SSoQE visualizations",
      "ssoqe_discrete_colors <- function(n) {",
      "  if (n <= length(ssoqe_cols)) {",
      "    return(ssoqe_cols[1:n])",
      "  } else {",
      "    # Generate more colors using colorRampPalette if needed",
      "    return(colorRampPalette(ssoqe_cols)(n))",
      "  }",
      "}",
      "",
      "# Export commonly used values for backward compatibility",
      "text_size <- text_size_base",
      "line_size <- line_size_base"
    )

  # Write to set_r_theme.R
  writeLines(
    text = r_theme_content,
    con = output_file
  )

  message("set_r_theme.R generated successfully\n")
}

generate_fonts_html <- function(
    input_file = here::here("Presentation/fonts.json"),
    output_file = here::here("Presentation/fonts-include.html")) {
  message("Generating fonts-include.html...\n")

  # Check if fonts.json exists
  if (
    !file.exists(input_file)
  ) {
    stop("fonts.json not found. Please create this file first.")
  }

  # Read fonts from JSON
  fonts <- jsonlite::fromJSON(input_file)

  # Generate Google Fonts links
  font_links <-
    c(
      "<!-- Auto-generated Google Fonts links from fonts.json -->"
    )

  # Read fonts from JSON
  fonts <- jsonlite::fromJSON(input_file)

  # Generate Google Fonts links
  font_links <-
    c(
      "<!-- Auto-generated Google Fonts links from fonts.json -->"
    )

  # Add specific font links for Google Fonts
  if (
    !is.null(fonts$body) && fonts$body != ""
  ) {
    body_font_url <-
      paste0('<link href="https://fonts.googleapis.com/css2?family=', gsub(" ", "+", fonts$body), ':wght@400;500;600;700&display=swap" rel="stylesheet">')
    font_links <-
      c(font_links, body_font_url)
  }

  if (
    !is.null(fonts$heading) && fonts$heading != ""
  ) {
    heading_font_url <-
      paste0('<link href="https://fonts.googleapis.com/css2?family=', gsub(" ", "+", fonts$heading), ':wght@500;600;700&display=swap" rel="stylesheet">')

    font_links <-
      c(font_links, heading_font_url)
  }

  if (
    !is.null(fonts$monospace) && fonts$monospace != ""
  ) {
    mono_font_url <-
      paste0('<link href="https://fonts.googleapis.com/css2?family=', gsub(" ", "+", fonts$monospace), ':wght@400;600&display=swap" rel="stylesheet">')
    font_links <-
      c(font_links, mono_font_url)
  }

  # Write to fonts-include.html
  writeLines(
    text = c(
      font_links
    ),
    con = output_file
  )

  message("fonts-include.html generated successfully\n")
}

# ==============================================================================
# HELPER FUNCTIONS FOR CUSTOM THEME GENERATION
# ==============================================================================

# Helper function to resolve color names to SCSS variables or hex values
resolve_color_name <- function(color_name, colors_data) {
  # Handle both old format (flat) and new format (nested)
  if (
    "primary" %in% names(colors_data)
  ) {
    # New nested format
    primary_colors <- colors_data$primary
    semantic_colors <- colors_data$semantic
    all_colors <- c(primary_colors, semantic_colors)
  } else {
    # Old flat format (backward compatibility)
    all_colors <- colors_data
    primary_colors <- colors_data
    semantic_colors <- list()
  }

  # If it's a hex color (starts with #), return as is
  if (
    grepl("^#", color_name)
  ) {
    return(color_name)
  }

  # If it's a semantic color that references a primary color
  if (
    color_name %in% names(semantic_colors)
  ) {
    referenced_color <- semantic_colors[[color_name]]
    # If the referenced color is a primary color name, resolve it
    if (referenced_color %in% names(primary_colors)) {
      return(paste0("$", referenced_color))
    } else {
      # If it's already a hex value, return as SCSS variable
      return(paste0("$", color_name))
    }
  }

  # If it's a known primary color name, return as SCSS variable
  if (
    color_name %in% names(primary_colors)
  ) {
    return(paste0("$", color_name))
  }

  # If it's a known color name, return as SCSS variable
  if (
    color_name %in% names(all_colors)
  ) {
    return(paste0("$", color_name))
  }

  # If not found, return as is (might be a SCSS variable already)
  return(color_name)
}

generate_custom_theme_defaults <- function(custom_theme, fonts) {
  c(
    "// Vertical spacing between blocks of text",
    paste0("$smallMargin: ", custom_theme$margins$smallMargin, ";"),
    paste0("$blockMargin: ", custom_theme$margins$blockMargin, ";"),
    paste0("$largeMargin: ", custom_theme$margins$largeMargin, ";")
  )
}

generate_custom_theme_font_overrides <- function(fonts) {
  c(
    paste0("$mainFontSize: ", fonts$sizes$mainFontSize, ";"),
    paste0("$body-line-height: ", fonts$sizes$bodyLineHeight, ";"),
    "",
    paste0("$headingMargin: 0;"),
    paste0("$headingLineHeight: ", fonts$sizes$headingLineHeight, ";"),
    paste0("$headingLetterSpacing: ", fonts$spacing$headingLetterSpacing, "; /* -2% tracking as per guidelines */"),
    "$headingTextTransform: none; /* SSoQE uses title case, not uppercase */",
    "$headingTextShadow: none;",
    paste0("$headingFontWeight: ", fonts$weights$headingFontWeight, "; /* Semibold as per guidelines */"),
    "",
    paste0("$heading1Size: ", fonts$sizes$heading1Size, "; /* 36-44px equivalent */"),
    "$heading1TextShadow: none;",
    paste0("$heading2Size: ", fonts$sizes$heading2Size, "; /* 28-32px equivalent */"),
    paste0("$heading3Size: ", fonts$sizes$heading3Size, "; /* 22-24px equivalent */"),
    paste0("$heading4Size: ", fonts$sizes$heading4Size, ";")
  )
}

generate_custom_theme_body_styles <- function(custom_theme) {
  c(
    "/* General text styling */",
    "body {",
    "  font-family: $mainFont;",
    "  font-size: $mainFontSize;",
    "  line-height: $body-line-height;",
    "  color: $body-color;",
    "  background-color: $backgroundColor !important;",
    "}"
  )
}

generate_custom_theme_code_styles <- function(custom_theme, colors_data) {
  # Resolve the color name to SCSS variable or hex value
  resolved_bg_color <- resolve_color_name(custom_theme$code$codeBackgroundColor, colors_data)

  c(
    "/* Code styling (default and reveal) */",
    "pre,",
    ".reveal pre,",
    "code,",
    ".reveal code {",
    paste0("  background-color: ", resolved_bg_color, " !important; /* Light Cambridge Blue */"),
    "  color: $body-color;",
    paste0("  border-radius: ", custom_theme$code$codeBorderRadius, "; /* More rounded as per brand guidelines */"),
    paste0("  line-height: ", custom_theme$code$codeLineHeight, ";"),
    "  font-family: $monospaceFont;",
    "  border: none; /* Remove border for cleaner look */",
    "}",
    "",
    "pre,",
    ".reveal pre {",
    "  padding: $smallMargin;",
    "}",
    "",
    "code,",
    ".reveal code {",
    "  background-color: $midnightGreen !important;",
    "  color: $satinSheenGold !important;",
    "  font-style: italic;",
    paste0("  padding: ", custom_theme$code$inlineCodePadding, ";"),
    "}",
    "",
    "pre code,",
    ".reveal pre code {",
    "  background-color: transparent !important;",
    "  color: $body-color !important; /* Black text for code blocks */",
    "  font-style: normal !important; /* Override italic from inline code */",
    "  padding: $smallMargin;",
    "  position: relative;",
    "  /* Ensure line highlighting works */",
    "  z-index: 0;",
    "  /* Prevent conflicts with overlays */",
    "}"
  )
}

generate_custom_theme_scrollbar_styles <- function() {
  c(
    "/* Hide scrollbars for WebKit browsers */",
    "pre::-webkit-scrollbar,",
    ".reveal pre::-webkit-scrollbar {",
    "  display: none !important;",
    "}",
    "",
    "/* Hide scrollbars and ensure proper text wrapping */",
    ".reveal pre,",
    ".reveal .slides pre,",
    ".reveal .slides section pre,",
    "div.sourceCode,",
    "div.sourceCode pre,",
    ".cell-code pre,",
    ".hljs {",
    "  overflow: visible !important;",
    "  max-width: 100% !important;",
    "  white-space: pre-wrap !important;",
    "  word-wrap: break-word !important;",
    "}",
    "",
    ".reveal pre::-webkit-scrollbar,",
    ".reveal .slides pre::-webkit-scrollbar,",
    ".reveal .slides section pre::-webkit-scrollbar,",
    "div.sourceCode::-webkit-scrollbar,",
    "div.sourceCode pre::-webkit-scrollbar,",
    ".cell-code pre::-webkit-scrollbar,",
    ".hljs::-webkit-scrollbar {",
    "  display: none !important;",
    "  width: 0px !important;",
    "  height: 0px !important;",
    "}"
  )
}

generate_custom_theme_quarto_fixes <- function() {
  c(
    "/* Quarto-specific code elements */",
    ".sourceCode code {",
    "  background-color: transparent !important;",
    "  color: inherit !important;",
    "}",
    "",
    "/* Fix for code blocks in HTML output */",
    ".sourceCode {",
    "  margin-left: 0 !important;",
    "  margin-right: 0 !important;",
    "}"
  )
}

generate_custom_theme_link_styles <- function() {
  c(
    "pre a,",
    ".reveal pre a,",
    "code a,",
    ".reveal code a {",
    "  color: $body-color;",
    "  text-decoration: underline;",
    "",
    "  &:hover {",
    "    color: $link-color;",
    "    background-color: $body-color;",
    "  }",
    "}",
    "",
    "a,",
    ".reveal a,",
    ".reveal .footer a {",
    "  color: $link-color !important;",
    "  text-decoration: underline !important;",
    "",
    "  &:hover {",
    "    color: $body-color !important;",
    "    background-color: $link-color !important;",
    "  }",
    "}",
    "",
    "/* Specific styling for links in title slides and slides with background colors */",
    ".reveal .slides section a,",
    ".reveal .title-slide a,",
    ".reveal .bg-midnightGreen a,",
    ".reveal .slide-background a {",
    "  color: $link-color !important;",
    "  text-decoration: underline !important;",
    "",
    "  &:hover {",
    "    color: $backgroundColor !important;",
    "    background-color: $link-color !important;",
    "  }",
    "}",
    "",
    "/* Force color for all links, overriding any reveal.js defaults */",
    ".reveal a:link,",
    ".reveal a:visited,",
    ".reveal a:active {",
    "  color: $link-color !important;",
    "}"
  )
}

generate_custom_theme_heading_styles <- function(custom_theme) {
  c(
    "/* Headings */",
    "h1,",
    "h2,",
    "h3,",
    "h4,",
    ".reveal h1,",
    ".reveal h2,",
    ".reveal h3,",
    ".reveal h4 {",
    "  font-family: $headingFont !important;",
    "  line-height: $headingLineHeight;",
    "  letter-spacing: $headingLetterSpacing;",
    "  text-transform: $headingTextTransform;",
    "  font-weight: $headingFontWeight;",
    "  margin: $headingMargin;",
    "  color: $headingColor;",
    "  text-shadow: $heading1TextShadow;",
    "}",
    "",
    "h1,",
    ".reveal h1 {",
    "  font-size: $heading1Size !important;",
    "  margin-top: $blockMargin;",
    "}",
    "",
    "h2,",
    ".reveal h2 {",
    "  font-size: $heading2Size !important;",
    "  margin-top: $blockMargin;",
    "}",
    "",
    "h3,",
    ".reveal h3 {",
    "  font-size: $heading3Size !important;",
    "  margin-top: $smallMargin;",
    "}",
    "",
    "h4,",
    ".reveal h4 {",
    "  font-size: $heading4Size !important;",
    "  margin-top: $smallMargin;",
    "}",
    "",
    "/* Additional Reveal.js specific heading font rules */",
    ".reveal .slides section .fragment h1,",
    ".reveal .slides section .fragment h2,",
    ".reveal .slides section .fragment h3,",
    ".reveal .slides section .fragment h4,",
    ".reveal .slides section h1,",
    ".reveal .slides section h2,",
    ".reveal .slides section h3,",
    ".reveal .slides section h4 {",
    "  font-family: $headingFont !important;",
    "}",
    "",
    "/* Ensure heading classes also use the correct font */",
    ".text-font-heading,",
    ".reveal .text-font-heading {",
    "  font-family: $headingFont !important;",
    "}"
  )
}

generate_custom_theme_list_styles <- function(custom_theme) {
  c(
    "/* List styling */",
    "ul {",
    "  padding-left: 0;",
    "  margin-left: $blockMargin;",
    "}",
    "",
    "ul ul {",
    "  margin-left: $blockMargin;",
    "}"
  )
}

generate_custom_theme_utility_classes <- function(custom_theme) {
  c(
    "/* Utility text classes */",
    ".text-bold {",
    "  font-weight: bold;",
    "}",
    "",
    ".text-italic {",
    "  font-style: italic;",
    "}",
    "",
    ".text-underline {",
    "  text-decoration: underline;",
    "}",
    "",
    ".text-strike {",
    "  text-decoration: line-through;",
    "}",
    "",
    ".text-uppercase {",
    "  text-transform: uppercase;",
    "}",
    "",
    ".text-lowercase {",
    "  text-transform: lowercase;",
    "}",
    "",
    ".text-capitalize {",
    "  text-transform: capitalize;",
    "}",
    "",
    ".text-normal {",
    "  text-transform: none;",
    "}",
    "",
    ".text-center {",
    "  text-align: center;",
    "}",
    "",
    ".text-left {",
    "  text-align: left;",
    "}",
    "",
    ".text-right {",
    "  text-align: right;",
    "}",
    "",
    ".text-smaller {",
    "  font-size: $mainFontSize * 0.7 !important;",
    "}",
    "",
    ".text-tiny {",
    "  font-size: $mainFontSize * 0.3 !important;",
    "}",
    "",
    ".text-larger {",
    "  font-size: $mainFontSize * 1.3 !important;",
    "}",
    "",
    ".text-size-heading1 {",
    "  font-size: $heading1Size !important;",
    "}",
    "",
    ".text-size-heading2 {",
    "  font-size: $heading2Size !important;",
    "}",
    "",
    ".text-size-heading3 {",
    "  font-size: $heading3Size !important;",
    "}",
    "",
    ".text-size-heading4 {",
    "  font-size: $heading4Size !important;",
    "}",
    "",
    ".text-size-body {",
    "  font-size: $mainFontSize !important;",
    "}",
    "",
    ".text-shadow-light {",
    paste0("  text-shadow: ", custom_theme$shadows$lightTextShadow, " !important;"),
    "}",
    "",
    ".text-shadow-dark {",
    paste0("  text-shadow: ", custom_theme$shadows$darkTextShadow, " !important;"),
    "}",
    "",
    ".text-margin-bottom-15 {",
    "  margin-bottom: 15px !important;",
    "}",
    "",
    ".text-margin-top-15 {",
    "  margin-top: 15px;",
    "}"
  )
}

generate_custom_theme_slide_layout <- function(custom_theme) {
  c(
    "/* Slide content layout */",
    ".reveal .slides {",
    "  max-width: $content-max-width;",
    "  margin: auto;",
    paste0("  padding: ", custom_theme$slides$slidePadding, ";"),
    "}",
    "",
    ".slide-margin-top-15 {",
    "  margin: 0;",
    "  position: absolute;",
    paste0("  top: ", custom_theme$positioning$slideMarginTop15, ";"),
    "  -ms-transform: translateY(-15%);",
    "  transform: translateY(-15%);",
    "}",
    "",
    ".slide-margin-top-25 {",
    "  margin: 0;",
    "  position: absolute;",
    paste0("  top: ", custom_theme$positioning$slideMarginTop25, ";"),
    "  -ms-transform: translateY(-25%);",
    "  transform: translateY(-25%);",
    "}",
    "",
    ".center-vertical {",
    "  margin: 0;",
    "  position: absolute;",
    paste0("  top: ", custom_theme$positioning$centerVertical, ";"),
    "  -ms-transform: translateY(-50%);",
    "  transform: translateY(-50%);",
    "}"
  )
}

generate_custom_theme_image_styles <- function() {
  c(
    "/* Constrain figures and other large block elements */",
    ".reveal .slides img,",
    ".reveal .slides figure,",
    ".reveal .slides video,",
    ".reveal .slides .reveal-image {",
    "  max-width: 100%;",
    "  width: auto;",
    "  height: auto;",
    "  display: block;",
    "  margin: 0 auto;",
    "  box-sizing: border-box;",
    "}"
  )
}

generate_custom_theme_specialized_sections <- function() {
  c(
    "/* Specialized sections */",
    ".reveal .title {",
    "  background-color: $headingColor;",
    "  color: $backgroundColor;",
    "  text-align: center;",
    "}",
    "",
    ".reveal .subtitle {",
    "  background-color: $persianGreen;",
    "  color: $backgroundColor;",
    "  text-align: center;",
    "}",
    "",
    ".reveal .inverse {",
    "  color: $backgroundColor;",
    "  background-color: $headingColor;",
    "}",
    "",
    ".reveal .exercise {",
    "  background-color: $link-color;",
    "  color: $backgroundColor;",
    "}"
  )
}

generate_custom_theme_blockquote_styles <- function(custom_theme) {
  c(
    "/* Blockquote */",
    ".reveal .blockquote,",
    "blockquote {",
    paste0("  padding: ", custom_theme$blockquote$blockquotePadding, "; /* Following brand guidelines */"),
    paste0("  border-radius: ", custom_theme$blockquote$blockquoteBorderRadius, "; /* 2xl rounded corners */"),
    "  background-color: rgba($cambridgeBlue, 0.1);",
    paste0("  border-left: ", custom_theme$blockquote$blockquoteBorderWidth, " solid $persianGreen;"),
    "  border-top: 2px solid $cambridgeBlue;",
    "  border-bottom: 2px solid $cambridgeBlue;",
    "  border-right: 2px solid $cambridgeBlue;",
    paste0("  box-shadow: ", custom_theme$shadows$blockquoteShadow, "; /* Soft shadow with Midnight Green */"),
    paste0("  transition: background-color ", custom_theme$transitions$defaultTransition, ", border-color ", custom_theme$transitions$defaultTransition, ";"),
    paste0("  margin: ", custom_theme$blockquote$blockquoteMargin, ";"),
    "  font-style: italic;",
    "}"
  )
}

generate_custom_theme_code_block_styles <- function(custom_theme) {
  c(
    "/* Code block */",
    "code {",
    "  background-color: $backgroundColor;",
    "  padding: $smallMargin;",
    paste0("  border-radius: ", custom_theme$code$codeBorderRadius, ";"),
    "  font-family: $monospaceFont;",
    "  border: 1px solid $cambridgeBlue;",
    "}"
  )
}

generate_custom_theme_table_styles <- function(custom_theme) {
  c(
    "/* Table styling */",
    ".reveal table,",
    "table {",
    "  border-collapse: collapse;",
    "  margin-bottom: $blockMargin;",
    "  width: 100%;",
    "}",
    "",
    ".reveal table th,",
    ".reveal table td,",
    "table th,",
    "table td {",
    paste0("  border: 1px solid rgba(31, 41, 55, ", custom_theme$table$tableBorderOpacity, "); /* Black with opacity */"),
    paste0("  padding: ", custom_theme$table$tableCellPadding, ";"),
    "  text-align: left;",
    "}",
    "",
    ".reveal table th,",
    "table th {",
    "  background-color: $midnightGreen !important;",
    "  color: $white !important;",
    "  font-weight: 600;",
    "  font-family: $headingFont;",
    "}",
    "",
    ".reveal table tr:nth-child(odd),",
    "table tr:nth-child(odd) {",
    "  background-color: rgba(161, 195, 179, 0.5) !important; /* Very light Cambridge Blue */",
    "}"
  )
}

generate_custom_theme_exercise_overrides <- function() {
  c(
    "/* EXERCISE SECTION LINK OVERRIDES - Must be last to ensure precedence */",
    ".reveal .exercise a,",
    ".reveal .exercise * a,",
    ".reveal .slides .exercise a,",
    ".reveal .slides .exercise * a,",
    ".reveal .slides section.exercise a,",
    ".reveal .slides section.exercise * a {",
    "  color: $backgroundColor !important; /* Pure white for maximum contrast against Persian Green */",
    "  text-decoration: underline !important;",
    "  text-decoration-color: $backgroundColor !important;",
    "  font-weight: 600 !important;",
    "  background-color: transparent !important;",
    "",
    "  &:hover,",
    "  &:focus,",
    "  &:active {",
    "    color: $headingColor !important;",
    "    background-color: $backgroundColor !important;",
    "    border-radius: 3px !important;",
    "    padding: 2px 4px !important;",
    "    text-decoration: underline !important;",
    "    text-decoration-color: $headingColor !important;",
    "  }",
    "}"
  )
}

# Main execution
message("SSOQE Theme Generation\n")
message("Starting theme generation process...\n\n")

tryCatch(
  {
    # Generate all theme files
    # Generate colors and fonts SCSS for Presentation folder
    generate_colors_scss()
    generate_fonts_scss()

    # Generate colors and fonts SCSS for Exercises folder
    generate_colors_scss(
      input_file = here::here("Presentation/colors.json"),
      output_file = here::here("R/Exercises/_colors.scss")
    )
    generate_fonts_scss(
      input_file = here::here("Presentation/fonts.json"),
      output_file = here::here("R/Exercises/_fonts.scss"),
      use_html_sizes = TRUE
    )

    # Generate other theme files
    generate_fonts_html()
    generate_custom_theme_scss()
    generate_exercise_scss()
    generate_r_theme()

    message("\nTheme generation completed successfully!\n")
    message("Generated files:\n")
    message("  - Presentation/_colors.scss\n")
    message("  - Presentation/_fonts.scss\n")
    message("  - Presentation/fonts-include.html\n")
    message("  - Presentation/custom_theme.scss\n")
    message("  - R/Exercises/_colors.scss\n")
    message("  - R/Exercises/_fonts.scss\n")
    message("  - R/Exercises/_exercise_theme.scss\n")
    message("  - R/set_r_theme.R\n")
  },
  error = function(e) {
    message("\nError during theme generation:\n")
    message(paste("Error:", e$message, "\n"))
    quit(status = 1)
  }
)
