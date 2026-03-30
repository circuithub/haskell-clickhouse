# clickhouse-client

`clickhouse-client` is a small Haskell client for ClickHouse built around the `RowBinary` format.

It is intentionally narrow:

- typed query parameters
- typed result decoding
- efficient RowBinary inserts
- support for primitive values plus `Nullable`, `Array`, `Map`, and tuples

The public API lives under `Database.ClickHouse` with lower-level modules exposed for composition.

## Status

This library has been in production use at Scarf for over 2 years, but has not been developed or generalized beyond our own needs. We are happy to accept PRs for any PRs and bugfixes. 

## Installation

Add the package to your `.cabal` file:

```cabal
build-depends:
  base,
  clickhouse-client
```

## Connecting

```haskell
import Database.ClickHouse

connection <-
  newConnection
    ConnectionOptions
      { url = "http://localhost:8123"
      , database = Nothing
      , user = Nothing
      , password = Nothing
      , httpManager = Nothing
      , responseTimeoutSeconds = Nothing
      }
```

## Running a query

```haskell
import Database.ClickHouse
import Database.ClickHouse.Params qualified as Params
import Database.ClickHouse.Result qualified as Result

answer <-
  runQuery
    connection
    "SELECT { x : UInt64 }"
    (Params.uint64 "x")
    (singleRow (Result.column Result.uint64))
    42
```

For queries without parameters, use `mempty` and `()`:

```haskell
runQuery connection "SELECT 1" mempty noResult ()
```

## Inserting rows

```haskell
import Data.Functor.Contravariant (contramap)
import Database.ClickHouse
import Database.ClickHouse.Insert qualified as Insert
import Database.ClickHouse.Value qualified as Value

let rowEncoder =
      contramap fst Value.uint32
        <> contramap snd Value.string

let ins = Insert.insert "events" ["id", "name"] rowEncoder mempty

runInsert connection ins () [(1, "signup"), (2, "purchase")]
```

## Examples

- `examples/SimpleQuery.hs`
- `examples/SimpleInsert.hs`

## Supported value shapes

The library currently includes helpers for:

- integers and floating-point types
- `String`, `UUID`, `Date`, `Date32`, `DateTime`, `DateTime64`
- `Nullable`
- `Array`
- `Map`
- tuples (`tuple` through `tuple7` on the write side)

See `Database.ClickHouse.Params`, `Database.ClickHouse.Result`, and `Database.ClickHouse.Value` for the exact API surface.

## Testing and development

Tests are pure unit tests over param rendering, insert SQL rendering, and RowBinary encoding/decoding helpers.
No Docker dependency is required for the default test suite.

Typical local commands:

```bash
cabal build
cabal test
```

If `cabal` is not installed directly, `nix shell` with `ghc` and `cabal-install` is a straightforward fallback.

## Scope notes

This package is intentionally focused on the HTTP interface plus `RowBinary` encoding/decoding. It does not try to be a full ORM or cover every ClickHouse feature.

## License

Apache-2.0 (`LICENSE`)
