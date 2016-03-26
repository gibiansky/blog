In this post, I'd like to summarize why I prefer to store data in JSON rather
than CSV or TSV files, and show how `jq` can be used to process JSON files just
as effectively as traditional Unix tools (such as `cut`, `sed`, `awk`, `grep`,
etc) can be used to process CSV and TSV files.

# Why JSON?

When working with and analyzing data, we often have to deal with the data in
many formats, and must choose what format we would *like* to have the data in
for easiest processing. In the Python and R ecosystems, where tools such as
[Pandas](http://pandas.pydata.org/) and 
[data frames](http://www.r-tutor.com/r-introduction/data-frame) are common,
comma-delimited files (csv) or tab-delimited files (tsv) are common.

However, CSV and TSV files suffer from a few problems:

- Not all languages make it trivial to read these files. It is rarely
  difficult, but CSV and TSV files do not follow and rigid specification, and so
  parsing libraries can sometimes make differnt choices about how to deal with
  quotes, spaces, and so on, so incompatibility between languages or applications
  (Excel, Google Sheets, Numbers, etc) can sometimes become an issue.
- CSV and TSV files are hard to understand without context. If I look at a file
  with a bunch of columns, some of which are numeric, I will not know what each
  column is, unless there is a header. If there a header, parsing becomes
  trickier, because you must account for the header.
- CSV and TSV files enforce a very rigid format (a table of values). Not all
  data fits those formats; more importantly, while data may initially start out
  following that format, as the data source or generation procedure changes, it
  may not retain that format. For example, if a dataset originally has one type
  of row, and then another type of row is added with a few different columns, the
  columns must be added for *all* values, and must be blank for many of them.

None of these problems are showstoppers, and all of them are fairly easy to
work around. However, instead of working around problems caused by an
inflexible and fairly old format, we can can use an alternative format.

## JSON

JSON stands for JavaScript Object Notation, and is a text-based format which
can encode numbers, booleans, arrays, and string-keyed dictionaries (objects).

- Unlike CSV, there exists a specification of JSON, and libraries for parsing
  JSON are as a result often slightly simpler to import and use than libraries
  for CSV files.
- JSON allows for a variety of rich structure and hierarchy in your data
  representation, so your data representation can grow with the complexity of
  your data.
- JSON requires less context and is more type-safe, since each
  dictionary allows access to its fields only through the field names, and not
  through an index, and the field names are immediately visible next to the data.

# `jq` Primer

A common complaint about JSON as a data exchange format is that it is harder to
work with in an ad-hoc manner using command-line tools. In the Unix world, we have 
access to a large suite of tools for processing data:

- `wc` for counting.
- `grep` for searching.
- `sed` for editing by pattern matching with a regular expression.
- `cat` for combining files.
- `cut` for selecting fields from tabular formats.
- `sort` for sorting data.
- `uniq` for removing duplicates.
- `head` for selecting the beginning of the data.
- `tail` for selecting the end of the data.

All of these can be combined with pipes and fed a variety of options to produce
very expressive and powerful computational pipelines on the fly, and they work
mostly on raw and delimited text files, not JSON.

The `jq` tool fixes this. `jq` is a command-line tool for processing JSON in every imaginable way; its website describes it as the `sed` of the JSON world. In the remainder of this post, I'd like to give a bunch of uses of standard command-line utilities and show how then can easily be replicated with `jq`.

All code in this article applies to `jq` version 1.5, the latest version as of
this writing.

# Unix vs `jq`

In this section, I will directly compare standard Unix commands with their corresponding `jq` commands. I will not include any test input or output data; if you would like to test these commands with something, you can use these two files as starting points:

**data.tsv:**
```
Andrew,31,Honda,2015,California
Jane,27,Ford,2002,Maryland
Igor,55,Toyota,1998,Alaska
Igor,55,Toyota,1998,Kansas
Igor,55,Toyota,1998,Alaska
Ann,15,,,Kansas
```


**data.json:**
```json
{ "name": "Andrew", "age": 31, "car_type": "Honda", "car_make": 2015, "state": "California" }
{ "name": "Jane", "age": 27, "car_type": "Ford", "car_make": 2002, "state": "Maryland" }
{ "name": "Igor", "age": 55, "car_type": "Toyota", "car_make": 1998, "state": "Alaska" }
{ "name": "Igor", "age": 55, "car_type": "Toyota", "car_make": 1998, "state": "Kansas" }
{ "name": "Igor", "age": 55, "car_type": "Toyota", "car_make": 1998, "state": "Alaska" }
{ "name": "Ann", "age": 15, "car_type": null, "car_make": null, "state": "Kansas" }
```

# `cat`

`cat` is used for printing data to the screen and combining multiple files.

### Outputting data:

```bash
# Bash
$ cat data.tsv

# jq
$ jq . data.json
```

`jq` takes a *filter* as its first argument, which describes the transformation to do to the JSON. The filter `.` is the empty filter, which just outputs the data directly.

This `jq` command outputs:
```json
{
  "name": "Andrew",
  "age": 31,
  "car_type": "Honda",
  "car_make": 2015,
  "state": "California"
}
{
  "name": "Jane",
  "age": 27,
  "car_type": "Ford",
  "car_make": 2002,
  "state": "Maryland"
}
...
```

### Combining files

```bash
# Bash
$ cat data.tsv data.tsv

# jq
$ jq . data.json data.json
```

`jq` takes as many files as you need to read, just like `cat`.

### `jq` Options

Since JSON is a more complex format, you have more options on how to output it.
You can use `jq -c` to output to a single line and `jq -r` to not put quotes
around outputted strings.

# `cut`

`cut` lets you select fields from a tab-delimited file, with options to use any
character as a  delimiter.

### Selecting One Field

To select the first field, we would use:

```bash
# Bash
$ cut -f1

# jq
$ jq '.field1'
```

When using `jq`, we have to provide the name of the field in the JSON objects,
rather than the index of the field in the columns.

### Selecting Many Fields

To select the first field and third field, we would use:

```bash
# Bash
$ cut -f1,3

# jq
$ jq '{field1, field2}'
```

In `jq`, this will create objects with `field1` and `field2` as fields, but
nothing else. If you would like to create arrays instead, you can use:

```bash
# jq with arrays
$ jq '[.field1, .field3]'
```

This will create arrays with two elements each, the first element being
`field1` and the second element being `field2`.

# `head` and `tail`

`head` lets you select the first elements of a file, whereas `tail` lets you get the last elements of a file. Together these let you take any contiguous subset of the file.

### Take the First Elements

To select the first ten elements, we could use:

```bash
# Bash
$ head -n 10

# jq
$ jq -s '.[:10]'
```

Here we use the `-s` option, which tells `jq` to treat the entire file as a
single array, and applying its transformation to the array as a whole, rather
than applying the transformation to each element individually.

The `.[:10]` notation takes a slice of an array, starting at the beginning and
ending at the 10th element.

### Take the Last Elements

To select the last ten elements, we could use:

```bash
# Bash
$ tail -n 10

# jq
$ jq -s '.[-10:]'
```

Using a negative value counts backwards from the end of an array, so starting at -10 means starting 10 elements before the end of the array.

### Take the 10-20th Elements

To select elements 10 through 20, we could use:

```bash
# Bash
$ tail -n +11 | head -n 10

# jq
$ jq -s '.[10:20]'
```

In `bash`, using the `+N` value for `tail -n` or a `-N` value for `head -n` will
display everything *except* those elements; thus, `tail -n +11` will display
everything *but* the first ten elements.

In `jq`, we use the same consistent and simple array slice notation to select
our range of indices.

# `wc`

To count the number of values in a file, we could use:

```bash
# Bash
$ wc -l

# jq
$ jq -s length
```

This assumes in the case of `bash` that there is one value per line.

# `sort`

`sort` lets you sort a file.

### Sorting a File

To sort a file, we could use:

```bash
# Bash
$ sort

# jq
$ jq -s sort
```

Both of these will sort their inputs, though `sort` will sort on the first field, while `jq` has a more complex sort order.

### Sorting on a Key

To sort a file on the second field, we could use:

```bash
# Bash
$ sort -k2,2

# jq
$ jq -s 'sort_by(.field2)
```

Any filter could be used inside the `sort_by`, which makes `jq` much more
powerful than `sort`, as the sort order can be a complex bit of logic and not
just a field. For example, you can easily sort a list of arrays by their
average value, whereas that would require writing code with a tab-delimited
file.

# `uniq` and `sort -u`

To remove duplicates, we can use the `uniq` tool, or the `-u` option to `sort`.

### Getting Rid of Duplicates

```bash
# Bash (option 1)
$ sort | uniq

# Bash (option 2)
$ sort -u

# jq (option 1)
$ jq -s unique

# jq (option 1, unique on a field)
$ jq -s 'unique_by(.field1)'
```

These examples show different ways of removing duplicates. The `bash` examples
are identical. With `jq`, we can choose what we could as a duplicate, and we
can remove duplicates based on only some fields or based on some function of
the fields. For example, from a list of arrays, we could find one array of
every length by using `jq -s 'unique_by(length)'`.

### Counting Number of Repeats

To count the number of repeats of values in a file, we could use:

```bash
# Bash
$ sort | uniq -c

# jq
$ jq -s 'group_by(.) | map({(.[0]): length}) | add'
```

In this case, it's pretty clear that the `bash` representation is shorter and
easier; however, it's impressive that we can build `uniq -c` out of more basic tools provided to us by `jq`, and shows us how powerful it can be when we combine these tools.

We use a few new bits in the above command. First of all, we use `jq` pipes (the `|` operator). Pipes will take the output of the filter on the left and pipe it as input to the filter on the right, just like in `bash`; however, instead of the data being text, the data is JSON objects.

This composite filter has three pieces, each one piped into the next:

1. `group_by(.)`: This filter takes an array and separates it into buckets, where each bucket contains the same value.

2. `map({(.[0]): length})`: This filter is a gnarly beast. `map` is a filter that applies a filter to each element of an array. The filter `map` applies is `{(.[0]): length}`. This filter constructs an object where the key is obtained from the `.[0]` filter and the value is obtained from the `length` filter; in other words, given an input that is an array, the key is the first element of the array and the value is the length of the array. Taken all together, this transforms something like `[["a", "a"], ["b", "b", "b", "b"], ["c"]]` into `[{"a": 2}, {"b": 4}, {"c": 1}]`.

3. `add`: The filter `add` adds an array of things together; in the case of numbers, it is
numeric addition; in the case of objects, it merges them together and creates a
new object whose keys are the union of the keys of the objects being merged.

Altogether, we group our input array by the value, then turn the groups into `{value: count}` dictionaries, and then merge those dictionaries together. Here is an example:
```bash
$ echo '["a", "b", "a"]' | jq -c 'group_by(.)'
[["a","a"], ["b"]]

$ echo '["a", "b", "a"]' | jq -c 'group_by(.) | map({(.[0]): length})'
[{"a":2}, {"b":1}]

$ echo '["a", "b", "a"]' | jq -c 'group_by(.) | map({(.[0]): length}) | add'
{"a":2, "b":1}
```

# `grep`

`grep` lets us search through our data for strings or regular expressions.

### Searching for a String

To search for the string `string` in our data, we could use:

```bash
# Bash
$ grep string

# jq
$ jq 'select(.key == "string")'
```

When using `jq`, we have to choose what key we'd like to check. The `select`
filter applies a filter to every element, and then only keeps elements for
which the filter is true.

### Searching for a Regex

To search for the strings `string`, `bother`, or `mother` in our data, we could use:

```bash
# Bash
$ grep -E '(string|[bm]other)'

# jq
$ jq 'select(test("(string|[bm]other)"))'
```

We use the `test` filter, which applies a regex to a string and returns
true if it matched.

# `sed`

`sed` lets us apply regex search and replace to every line of a file. To do
this, we could do something like this, which replaces capital letters with
dots:

```bash
# Bash
$ sed 's/[A-Z]/./g`

# jq
$ jq 'gsub("[A-Z]"; ".")'
```

We use the `gsub` filter, which applies a regex search and replace as many
times as it can. To only do the search and replace once we could have used
`sub` instead.

# `awk`

`awk` is the Swiss army knife of Unix text processing. It is an entire programming languagee built for quick and dirty text manipulation, just like `jq` is built for JSON manipulation. As a result, they can do quite similar things. 

### Filter a File

To select only elements where the third column is greater than ten, we could use:

```bash
# Bash
$ awk '$3 >= 10'

# jq
$ jq 'select(.field3 >= 10)'
```

### Sum a Field

To sum all the first columns of a file, we could use:

```bash
# Bash
$ awk '{ sum += $1 } END { print sum }'

# jq
$ jq -s 'map(.field1) | add'
```

We use the `add` filter for `jq`, which can sum a list of numbers.

### Average a Field

To average all the first columns of a file, we could use:

```bash
# Bash
$ awk '{ sum += $1; n += 1; } END { print (sum / n) }'

# jq
$ jq -s 'add / length'
```

Our `jq` filter applies the `add` and the `length` filters and divides the
result of `add` by the result of `length`.

# Conclusion

The commands above barely scratch the surface of what `jq` can do. `jq`
supports variables, functions, modules, control over how it reads its input,
streaming huge (many gigabyte-sized) JSON files while processing them, math,
recursion, and a variety of other things, including plenty more built-in
functions than the ones we looked at. Although there is certainly a learning
curve, it is an incredibly productive and effective tool once you achieve a
basic fluency init.

For me, this makes working with JSON *much* easier than working with similar
tab or comma delimited files. In addition, `jq` has native support for reading
and writing tab-delimited files, so it is easy to convert between TSV and JSON
at will.

Finally, while I portrayed `jq` as an alternative to the standard Unix toolset,
it is really more of a complement. In my own daily usage, I regularly combine
`jq -c` with `head` and `tail` (when I need subsets of records), `grep` (when I
am searching for particular strings, but don't know what keys to look in),
`sed` (when I have some string substitution to make across many keys), and so
on. Always use the best tool for the job!

For more information, I recommend reading the
[`jq` manual](https://stedolan.github.io/jq/manual/); it is extensive and
well-written.

Happy `jq` hacking!
