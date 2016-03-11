# geoboner
![yep](https://i.imgur.com/IFdssi1.png "yep")

## FAQ

### The fuck?

It's a meme u dip

### What do I need?

* OCaml
* OPAM
* Lwt
* Opium
* Yojson
* Batteries
* Safepass
* ppx_blob
* Gensqlite (OPAM package may be broken)

### How do I use this?

For now, there is only a backend in this repository. To deploy the backend you can run (with the necessary OPAM packages):
```bash
$ ./configure
$ make
$ sudo ./main.native -p 80
```

### How do I make a frontend?

Short overview of the interface:

| Function | Request Type | Arguments                               | Description                   |
|----------|--------------|-----------------------------------------|-------------------------------|
| /        | GET          |                                         | Test API                      |
| /new/    | POST         | firstname, lastname, username, password | Creates a new user            |
| /user/   | GET          | username (in URL)                       | View all of a user's logs     |
| /log/    | POST         | username, password, longitude, latitude | Log a new location            |
| /near/   | GET          | longitude, latitude (in URL)            | Get nearby activity (geojson) |
