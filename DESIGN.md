
Overall I'm quite happy with how nicely concurrent the program is.

Some things I'm not quite so content with on the other hand, are that one,
it could be faster, and two, it's using a lot of memory. There are some parts
of the program where instead of writing a smaller modular function that takes
part of processed data I instead just write one bigger and re-run it over data
wholly. An example of this is `sanitiseLinks` which resides inside the body of
`crawl` in `Lib.hs`. This function is run twice on the links, once with the
`allowVisited` flag set to `True` and once `False`. Instead we could simply
split that off and filter visited links afterwards, because there is a parser
being run within which should be more expensive than simply recursing over the
structure again.

As for the memory usage, there is a space-leak somewhere in the program hasn't
been found, as it can be seen growing to gigabytes of memory
usage in some instances. Considering the data stored should only be in kilobytes
or megabytes;
the throughput coming from the HTTP requests is of course much more but this
should result in a relatively flat, and not increasing, memory usage in theory.

I'm using a Dell XPS 13 (9350) laptop to run the program. My Internet speed is
approximately 30 Mbps down and upload capped at 2 Mbps. With this, my results
are as follows.

```sh
$ crawler -t 1 'http://gocardless.com/'
Crawling "https://gocardless.com/"...
Finished with "https://gocardless.com/".
        Done threads: 1 [ThreadId 3]
        URLs crawled: 1795
        Unique asset links: 1787
        Unique asset imgs: 456
        Unique asset scripts: 58
        Unique asset styles: 58
        Time elapsed (HTTP): 421.3290823s
        Time elapsed (total): 428.7137666s
Sitemap file: gocardless.com

$ crawler -t 2 'http://gocardless.com/'
...
        Time elapsed (HTTP): 171.0120849s
        Time elapsed (total): 178.4599005s

$ crawler -t 6 'http://gocardless.com/'
...
        Time elapsed (HTTP): 80.7108952s
        Time elapsed (total): 99.5526153s

$ crawler -t 10 'http://gocardless.com/'
...
        Time elapsed (HTTP): 47.0789825s
        Time elapsed (total): 56.8279261s

$ crawler -t 16 'http://gocardless.com/'
...
        Time elapsed (HTTP): 39.5091904s
        Time elapsed (total): 48.4870745s

$ crawler -t 32 'http://gocardless.com/'
...
        Time elapsed (HTTP): 30.7671733s
        Time elapsed (total): 39.3936259s

$ crawler -t 64 'http://gocardless.com/'
...
        Time elapsed (HTTP): 28.3103691s
        Time elapsed (total): 37.0040264s

```

The benefit of more threads seems to taper off logarithmically.

# Project structure

### `app/Main.hs`

The `main`; we just do command-line parsing here and delegate it to the
initialiser in `src/Lib.hs`.

### `src/Lib.hs`

This is where everything happens. Because it's ~500 LOC I've decided not to
split the program into other modules and keep it simple, however if I were to
do so I would choose:

* `src/Types.hs` — data structures and types
* `src/Crawler.hs` — main crawler library
* `src/Parser.hs` — written parsers
* `src/Web.hs` — web utilities

### `test/Spec.hs`

Some unit tests have been written for the URL parser.

# Concurrency

State Transactional Memory is used in order to avoid worry about deadlocks.

### Shared state

There are three global shared state variables. There is also
one additonal final variable given to each thread.

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
pages contained them. Each thread has its own `assetCache` because they aren't
required to be shared but are needed so the main thread can collect the data on
its exit procedure. This spreading allows for more efficiency.

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

