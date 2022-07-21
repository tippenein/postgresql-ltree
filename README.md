# postgresql-ltree

Haskell support for PostgreSQL's [ltree](https://www.postgresql.org/docs/current/ltree.html).

## postgresql-ltree [![postgresql-ltree](https://img.shields.io/hackage/v/postgresql-ltree.svg?logo=haskell&color=blueviolet)](https://hackage.haskell.org/package/postgresql-ltree)

Core types and functions for use in Haskell code.
Does not provide support for any database library; use one of the
following libraries instead.


## postgresql-simple-ltree [![postgresql-simple-ltree](https://img.shields.io/hackage/v/postgresql-simple-ltree.svg?logo=haskell&color=blueviolet)](https://hackage.haskell.org/package/postgresql-simple-ltree)

Contains type class instances for `FromField` and `ToField` for use with [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple).
Re-exports types and functions from `postgresql-ltree` so there should be no
need to depend on both.
