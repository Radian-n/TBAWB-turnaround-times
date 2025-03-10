# Black and White Box Turnaround Times Page

## Description

This project creates a static web page that displays the turnaround times for various services provided by [The Black and White Box photo lab](https://theblackandwhitebox.co.nz/) in Auckland, NZ.

The page is designed for staff (or anyone really) to be able to, at a glance, determine the expected due date of a specific service provided by the lab.

## How it works

The expected due dates are pulled from the lab's [current turnaround times](https://theblackandwhitebox.co.nz/turn-around-times/). These turnaround times are updated by Black and White Box staff based on the current workload.

The dates of any upcoming holidays are pulled from the [NZ Public Holidays API](https://www.public-holidays.nz/).

The due dates are calculated based on the current date, the current lab turnaround times published on the website, and any public holidays (NZ and Auckland). Public holidays will add an extra day to the turnaround times.

Each day at 4:30pm UTC (4:30am or 5:30am NZT depending on daylight savings), the webpage is rebuilt, and the new due dates are re-calcaulted.

The web page is built using a Quarto document with R. The daily github action executes the R code and renders a new HTML page from the Quarto document. Then github deploys the HTML as a github page.

## Limitations

### Currently a work in progress

Website isnt fully implemented yet. It's currently just a proof of concept.

### Page is only updated daily.

If the turnaround times webpage is updated at 2pm so that all the turnaround times are now 1 working day longer, then the new expected due dates won't be calculated until the website is rebuilt the following day at 4am.