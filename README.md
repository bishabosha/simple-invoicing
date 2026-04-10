# Simple Invoicing

This is an evolving script that you can clone and run yourself.

## Usage

```bash
./invoicer.sc resources/sample-config.scala
./invoicer.sc --output invoices/acme-2026-01.pdf resources/sample-config.scala
./invoicer.sc --monospace-font /path/to/your-monospace.ttf resources/sample-config.scala
```

either put config files in resources with `conf_` prefix to be ignored by git, or you
can create a symlinked directory `linked-configs` that will be ignored (e.g. for storing config files in a private space)

the script takes 1 config argument that is a Scala file (formatted as [Scala Object Notation](https://github.com/bishabosha/scala-object-notation)), following the schema in [Configs.scala](Configs.scala).

> Hint: the config file is read like data, not compiled!

by default the output is written to `Invoice.pdf`, and you can override that with `--output <file>`. if you want a custom monospace typeface for bank details and TWINT fields, pass the full file path with `--monospace-font <file>`.

the generator now validates common config mistakes up front, such as missing items, invalid tax rates, bad date formats, and missing custom font files.

## Required Dependencies

- [scala](https://www.scala-lang.org/download/) on PATH as `scala`.
- Java virtual machine
- optional: a local `.ttf` or similar font file if you want to use `--monospace-font`.
