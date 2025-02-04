atproto-lexicon -- Generate OCaml code from AT Protocol Lexicon Schemas
=======================================================================

_Status: WIP_

This code parses a subset of [AT Protocol Lexicon](https://atproto.com/specs/lexicon) files and generates corresponding OCaml types. This library can be used to bootstrap bindings to atproto APIs. It is a non-goal for this library to be perfect and plenty of manual work may be needed to get your library into a working and desirable state.

# Usage

```sh
atproto-lexicon path/to/lexicon.json
```
