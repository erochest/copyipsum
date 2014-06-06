# copyipsum

Copies lorem ipsum text from the Loripsum.net API.

## Usage

```
copyipsum [-h] [-p PARAGRAPHS] [-s {short,medium,long,verylong}] [-a]
                 [-b] [-c] [-d] [-H] [-l] [-q] [-t] [-o] [-u]
                 [-O {print,copy,both}]
```

## Arguments

```
  -h, --help            show this help message and exit
  -p, --paragraphs      The number of paragraphs to generate. Defaults to 4.
  -s, --size            The size of paragraphs. Options are short, medium,
                        long, verylong. Defaults to medium.
  -a, --allcaps         Use ALL CAPS.
  -b, --decorate        Add decorated text, including bold, italic and mark.
  -c, --code            Add code samples.
  -d, --dl              Add definition lists.
  -H, --headers         Add headers.
  -l, --link            Add links.
  -q, --bq              Add blockquotes.
  -t, --plaintext       Return plain text, no HTML.
  -o, --ol              Add ordered lists.
  -u, --ul              Add unordered lists.
  -O, --output          Output option. Options are copy, print, both.
                        Defaults to copy.
```

## Examples

### Copy four paragraphs.

```
> copyipsum
```

### Copy 10 paragraphs with headers

```
> copyipsum -H -i 10
```

### Print out 10 paragraphs with headers

```
> copyipsum -H -i 10 -O print
```

## Thanks

* [Eric Rochester](http://github.com/erochest) for more or less telling
  me what to type.
