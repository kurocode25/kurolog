Kurolog - Personal Blog Backend System
=======
日本語版は[README\_ja.md](https://github.com/kurocode25/kurolog/blob/main/README_ja.md)をお読みください。

## About this project
Kurolog is a personal blog backend system written in [Haskell](https://www.haskell.org/). In addition to providing an API, it also has the ability to dynamically generate sitemaps and Atom feeds. It is intended to be used in combination with a front end made with React, Vue, etc.

__Note: This project is in beta. In practical use, there may be some parts where the functionality is unstable. __

## Features
`Kurolog` has the following features:

+ Provides API for Blog
+ Supports dynamic OGP in SPA sites
+ Sitemap generation function
+ Atom feed generation function
+ Database migration function

## Build
Run the following command after cloning this project.

```bash
stack build
```

## setting file
### config.dhall file
Kurolog uses the `config.dhall` file written in [dhall language](https://dhall-lang.org/) as a configuration file. Please copy `sample/config.dhall` to `/etc/kurolog/` and edit it.

```bash
sudo mkdir -p /etc/kurolog
sudo cp sample/config.dhall /etc/kurolog/
```

Use this file to configure database connection settings, blog title, etc.

### Location of config.dhall file
The default location for this file is `/etc/kurolog/config.dhall`. However, you can also place it anywhere you like and specify the path when running the command.

## Execution environment
### Development environment
If you want to run it in a local environment, run the following command.

```bash
stack run -- serve [path/to/config.dhall]
```

### Production environment
Create a binary file with the following command.

```bash
stack build
stack install
```
Binary files are usually created under `~/.local/bin/`, but you can run them anywhere.

#### When used with Systemd
If you want to run it using Systemd, create a `kurolog.service` file like the one below and place it under `/etc/sysatemd/system/`.

__kurolog.service(sample)__

```
# This is sample file
[Unit]
Description = Blog backend service

[Service]
ExecStart = /bin/bash -c '/home/username/.local/bin/kurolog serve'
Restart = always
Type = simple

[Install]
WantedBy=multi-user.target
```
Register and start the service using the `systemctl` command.
```bash
sudo systemctl enable kurolog.service
sudo systemctl start kurolog.service
```

## How to use commands
Use the `kurolog` command as shown below.

```bash
kurolog [COMMAND] [ARGS]
```
__Command list__

|Command|Description|
|---|---|
|serve|API now available|
|initdb|Initialize the database|
|migrate|Database migration|
|createuser|Create user|
|--help|Help screen display|

### Example of use
An example of command execution is shown below.

+ Started providing API

```bash
kurolog serve [path/to/config.dhall]
```

+ User creation

```bash
kurolog createuser [path/to/config.dhall]
```

+ Initialize database

```bash
kurolog initdb [path/to/config.dhall]
```

+ Database migration

```bash
kurolog migrate path/to/migration_dir [path/to/config.dhall]
```

+ Display help screen

```bash
kurolog --help
```

## TODO
+ Support for second foreign language
+ API documentation creation with servant-docs

## License
This project is licensed under the [MIT](http://www.opensource.org/licenses/mit-license.php) License, see the LICENSE file for details
