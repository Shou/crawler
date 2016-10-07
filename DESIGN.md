
# Project structure

### `app/Main.hs`

The `main`; we just do command-line parsing here and delegate it to the
initialiser in `src/Lib.hs`.

### `src/Lib.hs`

This is where everything happens. Because it's ~500 LOC I've decided not to
split the program into other modules and keep it simple, however if I were to
do so I would choose:

* `src/Types.hs` — data structures and types
* `src/Crawler.hs` — Main crawler library
* `src/Parser.hs` — written parsers
* `src/Web.hs` — Web utilities

### `test/Spec.hs`

Some unit tests have been written for the URL parser.

# Concurrency

State Transactional Memory is used in order to avoid worry about deadlocks.

### Shared state

There are three global shared state variables. There is also
one additonal final variable given to each thread.

* `linkCache`
* `linkQueue`
* `threadDoneCache`
* `assetCache`

#### linkCache

A Set of seen links. Each thread adds to this when they're done downloading and
parsing a page.

#### linkQueue

A queue of links to be downloaded and parsed. Each thread adds to this similarly.

#### threadDoneCache

An array of thread IDs stating whether they've just finished a task of
downloading and parsing a page. When it's full, it's a potential signal to the
main thread that we're done; if the queue is simultaneously empty, the main
thread unblocks and procedes with the exit process.

#### assetCache

A key-value map containing each asset and their associated frequency and which
pages contained them.

# HTTP

In order to be efficient, we re-use HTTP sockets through sessions. One session
handler runs per thread, to download and parse pages, reading from the shared
queue.

# Parsing

### Robot.txt

The combinator parsing library `attoparsec` was used to write a parser
following the format of `robots.txt` files, in order to obtain the disallowed
paths.

### URLs

The combinator parsing library `attoparsec` was used to write a parser
mostly following the URL spec. Each part of the URL is stored in a
data-structure for easy access to e.g. just the domain, or path.

### HTML

A fast HTML parsing library called `taggy` was used. This is using `attoparsec`
under the hood.

