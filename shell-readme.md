# try-stack-reflex

using `stack` latest `ghcjs` support to effortlessly compile `reflex-todomvc` 


## Setup

make sure you have installed `stack` >= 0.1.7  https://github.com/commercialhaskell/stack

``` sh
git clone https://github.com/luigy/try-stack-reflex
cd try-stack-reflex
```


### ghcjs-0.1

```sh
# currently doing some custom patching for relaxing bounds for bifunctor package
./try-stack-reflex old
```

### ghcjs-0.2 (aka improved-base)

```sh
./try-stack-reflex
```

### ghcjs --interactive
[video demo](https://cldup.com/K4Ub9rOl9V.mp4)
```sh
./try-stack-reflex ghcjsi
```

## Caveats

requires having `ghc` compiler in your `PATH` https://github.com/commercialhaskell/stack/issues/1258
