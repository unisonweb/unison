This transcript will error, because we’re claiming that the stanza has a bug, but `help` works as expected.

``` ucm :bug
> help edit

  edit
  `edit foo` prepends the definition of `foo` to the top of the most recently saved file.
  `edit` without arguments invokes a search to select a definition for editing, which requires that `fzf` can be found within your PATH.

  See also `edit.to` to send the definitions to a file of your
  choice.
```

🎉

## You fixed a bug\!

The stanza above with `:bug` is now passing\! You can remove `:bug` and close any appropriate Github issues.
