# gdris

A toy [gopher][rfc1436] client written in [Idris2][idris web].

## Building

This software requires a working [idris2][idris2 github] installation.
So far, it has only been tested with `v0.3.0` which is the most recent
version at the time of writing. After installing idris2, compile this
software using the following command:

	$ idris2 --build gdris.ipkg

This will create an executable in `./build/exec/gdris`.

## Usage

The `gdris` program expects a `HOST` and `PORT` argument and starts an
interactive read–eval–print loop (REPL) afterwards. For example, to
connect to the `sdf.org` gopherhole start `gdris` as follows:

	$ ./build/exec/gdris sdf.org 70

This will print a menu for the initial directory listing on `sdf.org`.
Each menu entry will have an associated numeric identifier. The `goto`
command can be used to retrieve a specific document or to navigate to a
subdirectory. The `goto` command expects a numeric menu entry identifier
as an argument. The currently available menu entries can be retrieved
using the `menu` command.

## License

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
Public License for more details.

You should have received a copy of the GNU General Public License along
with this program. If not, see <http://www.gnu.org/licenses/>.

[rfc1436]: https://tools.ietf.org/html/rfc1436
[idris web]: https://idris-lang.org
[idris2 github]: https://github.com/idris-lang/Idris2
