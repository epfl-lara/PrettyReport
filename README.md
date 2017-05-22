PrettyReport
============

Convert [Stainless](https://github.com/epfl-lara/stainless)'s JSON reports into pretty HTML reports.


Instructions
------------

Build with `sbt clean script`, then add `bin/` to your `$PATH`.

To run this tool, simply invoke `prettyreport report.json > report.html` from where you ran Stainless with the `--json=report.json` option.

