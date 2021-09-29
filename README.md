# Multi-package Pure Skeleton with Development Environment

![CI](https://github.com/grumply/pure-project-reanimated/workflows/CI/badge.svg)

This repo builds on [pure-project-skeleton](https://github.com/grumply/pure-simple-blog) with [pure-admin](https://github.com/grumply/pure-admin) and [pure-conjurer](https://github.com/grumply/pure-conjurer) as a simple starting point for resource-oriented web applications.

## First run

Follow the steps at [purehs.cachix.org](https://purehs.cachix.org) to install [nix](https://nixos.org/nix/) and [cachix](https://cachix.org).

## Development

If you're not running an OS and CPU architecture combination for which Cachix has a pre-built and cached copy of Pure's custom `ghcjs-base`, the first build will be very, very slow. Subsequent builds will take advantage of nix memoization. Most recent linux and macos systems will benefit from the binary cache.

### Backend Development

To run a backend development server that will:

- rebuild configurations
- watch backend and shared Haskell and Cabal files for changes
- run tests
- rebuild and restart the server when necessary

```bash
$ ./develop --ghc
```

### Frontend Development

To run a frontend development server that will:

- serve your application at `localhost`
- watch frontend and shared Haskell and Cabal files for changes
- rebuild the application when necessary

```bash
$ ./develop --ghcjs
```

### IDE Tools

I suggest running `./develop --ghc` and `./develop --ghcjs` in two terminals, and allowing your editor to run `HIE`.

#### VS Code

* Install [Haskell Language Server for VS Code](https://marketplace.visualstudio.com/items?itemName=alanz.vscode-hie-server).

VS Code must be launched from a shell with access to the necessary libraries, etc...

```bash
$ nix-shell default.nix -A shells.ghc
[nix-shell:pure-project-skeleton]$ code .
```

#### nvim

* Tell `cachix` to use the `all-hies` build cache.
  ```bash
  $ cachix use all-hies
  ```

* Install the ghc 8.6.5 hie to match pure-platform's ghc version.
  ```bash
  $ nix-env -iA selection --arg selector 'p: { inherit (p) ghc865; }' -f https://github.com/infinisil/all-hies/tarball/master
  ```

* Install [coc.nvim](https://github.com/neoclide/coc.nvim).

* Follow the [instructions here](https://github.com/neoclide/coc.nvim/wiki/Language-servers#haskell), but change the command to `./develop` and the root patterns to `["cabal.config"]`.

#### ghcid

[ghcid](https://github.com/ndmitchell/ghcid) integration is also available, but has fewer features than `hie`.

```bash
$ nix-shell default.nix -A shells.ghc --run "ghcid -c \"cabal new-repl $project\""
```

where `$project` can be any of `frontend`, `backend`, or `shared`.

## Production

### `nix-build`

Nix can be used for creating deterministic, production-ready build products. You can use the `nix-build` command to build all or parts of your multi-package project with Nix.

- Build everything

  ```bash
  $ nix-build
    {.. build output omitted ..}
  $ tree result
  result
  ├── ghc
  │   ├── backend -> /nix/store/{..}-backend-0.1.0.0
  │   ├── frontend -> /nix/store/{..}-frontend-0.1.0.0
  │   └── shared -> /nix/store/{..}-shared-0.1.0.0
  └── ghcjs
      ├── frontend -> /nix/store/{..}-frontend-0.1.0.0
      └── shared -> /nix/store/{..}-shared-0.1.0.0

  7 directories, 0 files
  ```

- Build the backend only

  ```bash
  $ nix-build -o backend-result -A ghc.backend
  ```

- Build the frontend only

  ```bash
  $ nix-build -o frontend-result -A ghcjs.frontend
  ```
- Minimize the frontend

  ```bash
  $ closure-compiler --compilation_level ADVANCED_OPTIMIZATIONS --jscomp_off="*" --js frontend-result/bin/frontend.jsexe/all.js --js_output_file dist/all.js
  ```
  
  After minimization, the payload is reduced to 30% of the original.

  ```bash
  $ ls -lh frontend-result/bin/frontend.jsexe/all.js
  2.5M ... frontend-result/bin/frontend.jsexe/all.js
  $ ls -lh dist/all.js
  754K ... dist/all.js
  ```

  The style of code produced by GHCJS is quite well compressed. When served with gzip, the payload is reduced to 8% of the original.

  ```bash 
  $ gzip < dist/all.js > dist/all.js.gz
  $ ls -lh dist/all.js.gz
  212K ... dist/all.js.gz
  ```

## License

The `LICENSE` files in this project exist only as demonstrative templates. You have the right to modify those files in any way that is compatible with the following BSD-3 license for this project.

```LICENSE
Copyright (c) 2021, Sean Hickman

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sean Hickman nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```

## Thanks

Thanks to [Will Fancher](https://github.com/elvishjerricco) for [reflex-project-skeleton](https://github.com/elvishjerricco/reflex-project-skeleton) on which the structure of this project was originally based.

