# Audio DSP

My own collection of audio DSP plugins/applications written in Haskell.

# Usage

You need [Haskell Tool Stack](https://haskellstack.org) tool to be installed first.

```bash
stack build
stack install
```

Now you could run for example JACK standalone application:

```bash
thick-distortion.jack
```

Or if you didn't run `stack install` you could run it locally:

```bash
stack exec thick-distortion.jack
```

# Author

Viacheslav Lotsmanov

# License

[GPLv3](LICENSE)
