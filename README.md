# text-selections

Run text through expressive pipelines using all your favourite tools.

Let's jump right in; here're some commands

- `~ 'regex-pattern'`: match ALL non-overlapping occurances of 'regex-pattern' in text and focus them for the next
    pipeline step
- `! cmd arg1 arg2`: run shell command using currently focused text as stdin. Currently it will execute the command once
    for each selected block of text. Send the output down the pipeline in the same chunks we received it.
- `!{ cmd arg1 ? }`: Interpolate the selected block of text into the arguments of a shell command. This can be
    handy for doing file operations; e.g. if you have a filename selected you can replace the stream of filenames
    with a stream of files using `!{ cat ? }`.

That may not sound like much, but you can do a LOT with just those few combinators. More are coming soon :)

Please don't report bugs; this is still very alpha and I'll be iterating quickly.

# Installation

This is super new; so stack is the only way to run it for now. Try `stack install` then use `text-selections-exe`.
New and cool name coming soon.


# Examples

## File interpolation/replacement

Given input.txt:

```markdown
# First file
a.txt

---
# Second file
b.txt
```

- a.txt: `Contents of file A`
- b.txt: `Contents of file B`

We can select the filenames within the file using a simple regex `\w+\.txt`,
capitalize the contents, and interpolate it back into the source file:

```sh
cat input.txt | text-selections-exe '~ "\w+\.txt" | !{ cat ? } | ! tr a-z A-Z'
```

Resulting in:
```markdown
# First file
CONTENTS OF FILE A


---
# Second file
CONTENTS OF FILE B

```

More cool tricks coming soon 😎
