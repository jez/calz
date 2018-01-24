# calz

> A feature-rich command line calendar tool


## Usage

Note: the option parser accepts some flags that haven't been implemented yet.
Cross-reference with the TODO section below to see if it's not implemented yet.

```
Usage:
  calz [options] [<phrase>...]

Options:
  -l, --layout=<layout>  Can be 'grid' or 'flow' [default: flow]
  -n, --columns=<n>      If layout is 'grid': how many columns to use
                         [default: 3]
  -S, --separators       If layout is 'flow': show month separators
  -C, --no-color         Disable all color
  -h, --hide-labels      Don't show month labels
  -p, --pad              Complete the first and last weeks of every month with
                         the first and last days of surrounding months

Phrase:
  calz <month> [<year>]
  calz <year>
  calz (last|this|next) (month|year)
  calz <n> (months|years) ago
  calz <n> (months|years) from (now|today)
  calz from <phrase>... to <phrase>...

Examples:
  dec 2017
  next month
  3 months ago
  from 2 months from now to next year
```

## TODO

2. Grid layout
  - potentially handy: `transpose`, `chunksOf`
3. Highlight arbitrary dates
  - must be ISO 8601 format
  - read from stdin

Up for consideration:

- drop separators flag?
- flags to customize the color options?

## License

[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](https://jez.io/MIT-LICENSE.txt)

