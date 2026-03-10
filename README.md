# matchR: Entity Matching Tool

`matchR` is a Shiny-based GUI tool designed to make manual record linkage and human adjudication of potential matches between two datasets easier.

## Features

- **Side-by-Side Comparison:** Upload two CSV files and view them side-by-side.
- **Dynamic Field Selection:** Choose which columns to display from each dataset for easier identification.
- **Manual Matching:** Select a row from each table and record it as a match with an optional comment.
- **Export Results:** Download your recorded matches as a CSV file.
- **Serverless Hosting:** Designed to work with `shinylive` for hosting on GitHub Pages without needing an R server.

## How to Run Locally
1. The app.R script will run the tool locally.  


## Online version 
- There's an online version available for use by those without R/RStudio on their local machines. 


## Example Data

The `example_data/` folder contains several CSV files that can be used to test the matching tool:
- `fbi_list.csv`
- `intake_cards.csv`
- `repatriation_lists.csv`
