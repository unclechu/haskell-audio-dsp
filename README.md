# Audio DSP

My own collection of audio DSP plugins/applications written in Haskell.

## A list of implemented plugins/applications

- [**Thick Distortion**](ThickDistortion) — An implementation of simple idea of distortion effect
  which is processing clipping signal and make one sample dependent on previous one by changing to
  new value only by some “thickness” coefficient keeping value of previous sample by opposite
  coefficient. For example if thickness is 0.8 then N sample will by multiplied by 0.2 (1 - 0.8) and
  summed with N-1 multiplied by 0.8. In this case even if we have square signal it will be smoothed.

## Usage

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
