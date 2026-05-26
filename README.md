# Simple Invoicing

Simple Invoicing app interprets config files describing an invoice and produces a formatted PDF document.

The config file lets you describe business details, as well as a list of itemized products and prices, and even appendices to describe further details.

The `invoicer.sc` script validates the config file, calculates the full total and applies optional sales tax.

## Usage

```bash
./invoicer.sc resources/sample-config.sc
./invoicer.sc --output invoices/acme-2026-01.pdf resources/sample-config.sc
./invoicer.sc --monospace-font /path/to/your-monospace.ttf resources/sample-config.sc
```

- either put config `*.sc` files in resources (with `conf_` prefix to be ignored by git),
- or you can create a symlinked directory `linked-configs` that will be ignored (e.g. for storing config files in a private space)

the script takes 1 config argument that is a Scala file (formatted as [Scala Object Notation](https://github.com/bishabosha/scala-object-notation)), as a top-level expression following the schema in [Configs.scala](Configs.scala).

> Hint: the config file is read like data, not compiled!

by default the output is written to `Invoice.pdf`, and you can override that with `--output <file>`. if you want a custom monospace typeface for bank details and TWINT fields, pass the full file path with `--monospace-font <file>`.

> Note I recommend Inconsolata 4 monospace font, as it strikes-out zeros.

## Required Dependencies

- [scala](https://www.scala-lang.org/download/) on PATH as `scala`.
- Java virtual machine
- optional: a local `.ttf` or similar font file if you want to use `--monospace-font`.

## License

Licensed under the Apache License, Version 2.0. See [LICENSE](LICENSE).
