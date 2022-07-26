This package provides utilities to easily check that an opam switch is
consistent with a given `opam.export` file generated by `opam switch
export`.

- The `check_opam_switch` binary reads the `opam.export` file passed
  as an argument, and checks that all of the packages are installed in
  the current switch at the specified version.

- The `dune_wrapper` binary does this same check before invocating dune.
By default, it looks for the `opam.export` file at the root of the dune project,
unless the `DUNE_WRAPPER_OPAM_FILE` environment variable is set to the path of another file.

In order to continue using dune as usual but be warned early that the opam switch needs to be updated, we can use one of the following aliases.
```bash
dune=dune_wrapper #if the `opam.export` file is at the root of the dune project, 
```
or

```bash
dune="DUNE_WRAPPER_OPAM_FILE=/path/to/opam.export dune_wrapper" #otherwise
```
