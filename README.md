
### A concurrent Haskell website crawler.

## Install

You need Stack to install this Haskell program.
[Get it here.](https://docs.haskellstack.org/en/stable/README/#how-to-install)

Once you're in the the project git directory, run `stack install`.

You can run the small tests with `stack build --test`.

## Run

The program takes a list of homepage URLs to crawl as a command-line argument.

`crawler 'http://example.com/'`

Optionally you can specify the amount of threads to use.

`crawler -t 10 'http://example.com/'`

## Design

[DESIGN.md](DESIGN.md)

## License

BSD3. See the LICENSE file for more information.

