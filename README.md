# Nammayatri Extended

An example repo showcasing how to use Nammayatri as a library and extend it, whilst also enabling that public repo to build ourselves.

## Differences from a normal Haskell project

- Because this flake uses `mkNammayatriFlake`, dependency overrides must be specified in the public nammayatri repo. Private repos can't have their own overrides, but we can change this in future if there is a need.

## Notes on building this from the public repo

This repo is referenced in, and automatically built in the CI of, the public nammayatri repo (the [`ny-example` branch](https://github.com/nammayatri/nammayatri/compare/ny-example) of it, currently). You can run `nix build .#ny-example` in that branch of the public repo to build this repo indirectly whilst using the packages in the public repo as its dependency.

Jenkins does not need to have access to the private repo assuming the private repo too is built by the same Jenkins. Because, the source is cached in the Nix store, which the public repo can use. This does mean that the CI for private repo must run successfully and finish before the CI for the public repo starts.

## Limitations

You may notice that the CI for this repo builds nammayatri afresh, rather than using the local cache of the public repo's nammayatri packages. This is a limitation in [haskell-flake](https://github.com/srid/haskell-flake) which will be addressed in versions latter to 0.3.0 (specifically once we switch to [using this PR](https://github.com/srid/haskell-flake/pull/162)).