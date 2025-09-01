# gmgx

**gmgx** is a personal GNU Guix channel by
[@gmg48](https://codeberg.org/gmg48).

This channel provides packages and services that are either awaiting inclusion
in or not yet submitted to upstream GNU Guix, or cannot be included due to
upstream licensing guidelines and policies (such as non-free software).

## Usage

Add the channel to your `~/.config/guix/channels.scm`:

```scheme
(cons* (channel
        (name 'gmgx)
        (url "https://codeberg.org/gmg48/gmgx.git")
        (branch "main"))
       %default-channels)
```

Then fetch the channel definitions:

```bash
guix pull
```

### Using the channel locally

Clone the repository:

```bash
git clone https://codeberg.org/gmg48/gmgx.git
```

Set the environment variable to include this repository:

```bash
export GUIX_PACKAGE_PATH=/path/to/gmgx:$GUIX_PACKAGE_PATH
```

Then use Guix commands normally:

```bash
guix install opentofu
```

### Testing individual Packages

```bash
# Build a package
guix build -L /path/to/gmgx claude-code

# Test in temporary environment
guix shell -L /path/to/gmgx opentofu -- opentofu --version
```

## License

gmgx is made available under the GNU General Public License version 3 or
later (GPL-3.0-or-later).

## Contributing

This is a personal channel, but feel free to open issues or submit patches via
Codeberg.
