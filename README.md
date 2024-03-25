# d7012e

Install Haskell and cabal:

```
sudo apt update
sudo apt install ghc
sudo apt install cabal-install
```

Compile and run:

```
cabal build
cabal run
```

To run an individual main haskell file without creating an executable file:

```
runhaskell Main.hs
```

Use ghci to load a haskell script:

```
ghci
:l yourFile.hs
```
