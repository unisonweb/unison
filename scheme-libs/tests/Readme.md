
# Testing our chez backend

First make a codebase that has the basic things you need, that the other transcripts can run off of.
```bash
$ ucm transcript --save-codebase-to base.unison base.md
```

Then run the transcripts!
```bash
$ ucm transcript.fork -c base.unison test1.md
```
