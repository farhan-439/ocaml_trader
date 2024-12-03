Hello! In order to use Stock Simulator 3110, please ensure the following is downloaded:

Installations:

- Opam
- OCaml CSV library (opam install csv)
- OUnit 2 (opam install ounit2)
- cohttp-lwt-unix (http requests) (opam install cohttp-lwt-unix)
- lwt_ssl (lightweight thread for the asynchronous call) (opam install lwt_ssl)
- yojson (for parsing the api responses) (opam install yojson)
- ANSITerminal (for coloring terminal output) (opam install ANSITerminal)

Running code:

dune exec bin/main.exe
If the title looks strange, please widen your terminal window.
In simulation - use data/stocks.csv or your own data file to load preset stocks.

- If you save your portfolio, make sure to use the same data file to load your portfolio!

Thank you!
