# Gender Dynamics in Academia: Analyzing Regrade Requests and Course Load Trends, 2010-2016

## Overview of Repository

This repository contains data, code and a paper that analyzes the gender identities of college students and whether or not this affects their regrade requests among professors based on course load and instructor gender. The dataset used for analysis was retrieved from openICPSR, under the package `Data and Code for: Ask and You Shall Receive?` by Li and Zahar. 

## File Structure

The repo is structured as the following:

-   `input` contains the data sources used in analysis including raw and cleaned data and relevant literature.

-   `outputs/paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper.

-   `scripts` contains the R scripts used to simulate, download and clean data.

## How to Run

1.  Run `scripts/00-simulate_data.R` to simulate envisioned data
2.  Run `scripts/01-download_data.R` to download raw data
3.  Run `scripts/02-data_cleaning.R` to generate cleaned data
4.  Run `outputs/paper/paper.qmd` to generate the PDF of the paper

## LLM Usage Statement

-   ChatGPT 3.5 was used to help write a certain code portion within this paper, receipts can be found in `usage.text`. 

