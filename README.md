# hs-trillian

[![Build status](https://travis-ci.com/f-o-a-m/hs-trillian.svg?branch=master)](https://travis-ci.org/f-o-a-m/hs-trillian?branch=master)

Haskell bindings to google's [trillian](https://github.com/google/trillian), a verifiable data store

## Generated modules
You can find all the generate modules from the `*.proto` files with the help of 

```bash
find hs-trillian-protos/.stack-work -path '*autogen/Proto'
```

## Examples

### prereqs
There is an examples repo that contains a `docker-compose` file that you will need to launch before running any of the executables. The Makefile is used to launch the examples and exports environment variables that are needed to match the examples with the configuration in the docker-compose file, so this is probably the way you want to run these.

### simple-storage

```bash
> make run-simple-storage
```

simple-storage is a simple put/get test of the log. It has a rest server that exposes endpoints to put/get, you can test the
endpoints via curl with

```bash
> curl -X POST \
  http://localhost:3030/tx/increase_count \
  -H 'Content-Type: application/json' \
  -d '{ "newCount": 101, "username": "Martin"}'
```

which should give you back a merkle hash index into the tree, in this case something like

```bash
"a577ff5cf13898e7a4ffe71dddd384bbbe15331a7af61392807242c5778db426"
```

You can then query the store with

```bash
> curl -X GET \
  'http://localhost:3030/tx/increase_count?hash=a577ff5cf13898e7a4ffe71dddd384bbbe15331a7af61392807242c5778db426' \
  -H 'Content-Type: application/json'
```

which should give back 

```json
{ "username":"Martin"
, "newCount":101
}
```
