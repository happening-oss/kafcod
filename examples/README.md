# Examples

## Creating a new example

```sh
# pick one
rebar3 new escript name=example   # for an escript
rebar3 new app name=example       # or for an app
```

### Add kafcod dependency

```sh
cd example
mkdir _checkouts
ln -s ../../.. _checkouts/kafcod
```

Add `kafcod` to `rebar.config`:

```erlang
{deps, [kafcod]}.
```

Add `kafcod` to the `apps` entry in `src/example.app.src`:

```erlang
  {applications,
   [kernel,
    stdlib,
    kafcod
   ]},
```

### Remove the Apache license

Remove the license from `src/example.app.src`.

Remove the license file:

```sh
rm LICENSE.md
```

### Add a Makefile (escript)

```makefile
PROJECT := example

all:
	rebar3 escriptize
	ln -sf _build/default/bin/$(PROJECT) .
```

### Add a Makefile (app)

```makefile
PROJECT := example

all:
	rebar3 compile
```
