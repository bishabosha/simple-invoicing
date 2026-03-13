# Simple Invoicing

This is an evolving script that you can clone and run yourself.

## Usage

```bash
./invoicer.sc --inconsolata resources/sample-config.scala
./invoicer.sc resources/sample-config.scala
```

either put config files in resources with `conf_` prefix to be ignored by git, or you
can create a symlinked directory `linked-configs` that will be ignored (e.g. for storing config files in a private space)

the script takes 1 argument that is a Scala file (formatted as [Scala Object Notation](https://github.com/bishabosha/scala-object-notation)), following the schema in [Configs.scala](Configs.scala).

> Hint: the config file is read like data, not compiled!

output is generated to a single file in the same directory `Invoice.pdf` that you can rename as you wish.

## Required Dependencies

- [scala](https://www.scala-lang.org/download/) on PATH as `scala`.
- Java virtual machine
- with `--inconsolata` mode you need `Inconsolata-Regular.ttf` installed to `fonts/inconsolata-4/Inconsolata-Regular.ttf` relative to repo root. This font is more readable than the default Courier (zeros have a strike compared to the letter `O`).
