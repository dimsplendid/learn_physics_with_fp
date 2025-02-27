# Learn Physics with FP

Following the lecture of the the book: Learn Physics with Functional Programming by Scott N. Walck

## Install

The install on Windows is a little tricky and the easiest way is using docker/podman.

To work with `ghci` in the dev folder, cd to the objective path and type:

```bash
podman run -it --rm -v .:/workspace -w /workspace haskell:9
```

All is well :D