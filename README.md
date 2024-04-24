# d7012e

## Install haskell

```
sudo apt update
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

## Cabal

```
cabal init
cabal build
cabal run
```

## Stack

```
stack new myproject
cd myproject
stack setup
stack build
stack run
```

## Extra

To run an individual main haskell file without creating an executable file:

```
runhaskell Main.hs
```

Use ghci to load haskell modules:

```
ghci
:l yourFile.hs
```

## Install prolog

```
sudo apt update
sudo apt install swi-prolog
```

## Run prolog

```
swipl
[file_name].
```
