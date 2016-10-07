
This is a concurrent Haskell website crawler.

## Install

You need Stack to install this Haskell program.
[Get it here.](https://docs.haskellstack.org/en/stable/README/#how-to-install)

Once you're in the the project git directory, run `stack install`.

You can run the tests with `stack build --test` and the benchmark with `stack build --bench`.

## Run

The program simply takes a list of homepage URLs to crawl as a command-line argument.

`crawler 'http://example.com/'`

## Design

[DESIGN.md](DESIGN.md)

## License

BSD3. See the LICENSE file for more information.

