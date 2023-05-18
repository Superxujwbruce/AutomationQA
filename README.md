# Spend/Taxonomy QA Automation
This R Shiny app, created by Bruce Xu from the Solution Architect team, automates the quality assurance process for spend/taxonomy data. This app allows you to compare spend/taxonomy match rate between P1 and SFTP. Here is a screenshot of our Spend/Taxonomy QA Automation tool, as you can see below, the tool has a simple and intuitive interface that allows you to select desired columns and view the match rate graph.

## Features
- Uploads spend/taxonomy file from P1 and SFTP
- Allows user to choose desired columns for match rate testing
- Displays a graph of the match rate for each join key selected

## Installation
To run this app, you will need to have R installed on your machine. You can download R from the R website. If you have already installed Shiny packages before, you can skip this step. If you have not installed any package in R before, you can write the following code in R: <br>`install.packages(c('shiny', 'plyr', 'dplyr', 'shinythemes', 'shinycssloaders', 'formattable', 'ggplot2'))`<br>

## Usage
To run the app, type the following code in R: `shiny::runGitHub('AutomationQA', 'superxujwbruce')`<br><br>
In this way, it will launch the app in your default web browser. From there, you can follow the on-screen instructions to compare spend/taxonomy files and view the match rate graph. 

## UI Preview
Here is a screenshot of our Spend/Taxonomy QA Automation tool. As you can see below, the tool has a simple and intuitive interface that allows you to select desired columns and view the match rate graph.
<img src="https://github.com/Superxujwbruce/AutomationQA/blob/main/screenshot.png">

## Contact
If you have any questions or feedback, please contact me at bruce.xu@team.neustar
