# Twitter API Dojo

[![Build Status](https://travis-ci.org/jproyo/twitter-api-hs.svg?branch=master)](https://travis-ci.org/jproyo/twitter-api-hs.svg?branch=master)

This is a test application build in Haskell with stack, nix, scotty in order to help me improve my Haskell Skills.

Basically i want to write a REST app which proxy Twitter's User Timeline consumption.

I am going to evolve the app to achieve the following results:

- Learn more Haskell
- Learn Scotty
- Learn [Service Pattern](https://www.schoolofhaskell.com/user/meiersi/the-service-pattern)
- Learn integration with build tools Stack and Nix

## TODO

- Add Monad Transformer Stack to pass config, logger and other down
- Testing

## Run

### Prerequisites

In order to run this solution you are going to need the following distributions installed.

- Stack 1.6

### Run tests

```shell
bash.$ stack test
```

### Run tests

```shell
bash.$ stack test
```

### Startup Server

```shell
bash.$ stack build
bash.$ TWITTER_CONSUMER_KEY=XXXX TWITTER_CONSUMER_SECRET=YYYYY stack exec twitter-api-hs-exe
```

#### Run without Nix integration

```shell
bash.$ stack build --no-nix
bash.$ TWITTER_CONSUMER_KEY=XXXX TWITTER_CONSUMER_SECRET=YYYYY stack --no-nix exec twitter-api-hs-exe
```

#### Test Server

```shell
curl -v http://localhost:8080/user/haskelltips/timeline?limit=1
```
