# Simple Invoicing

Simple Invoicing app interprets config files describing an invoice and produces a formatted PDF document.

The config file lets you describe business details, as well as a list of itemized products and prices, and even appendices to describe further details.

The `invoicer.sh` script validates the config file, calculates the full total and applies optional sales tax.

## Usage

```bash
./invoicer.sh resources/sampleConfig.scala
./invoicer.sh --output invoices/acme-2026-01.pdf resources/sampleConfig.scala
./invoicer.sh --monospace-font /path/to/your-monospace.ttf resources/sampleConfig.scala
```

### Layouts

Two layouts are available via `--layout` (default `classic`):

- `classic` — the original plain layout: bordered table, monospace bank details.
- `studio` — a single-page style with a letterpress feel: centred serif
  masthead between framing rules, billed-to / payable-to columns up top,
  a tinted listings header band, a tinted totals card, and a full-bleed
  footer band with centred contact details. Optional image assets:
  `--logo` (footer roundel, bleeding off the lower-left page edge),
  `--paper-texture` (page background), `--accent-texture` (footer band
  background); textures fall back to flat fills when omitted. Sample assets
  live in `resources/images`. The studio labels are localizable (fork to add your own variant).

```bash
./invoicer.sh --layout studio \
  --logo resources/images/logo.png \
  --paper-texture resources/images/paper-page.png \
  --accent-texture resources/images/paper-accent.png \
  resources/sampleConfigStudio.scala
```

Config extras used by the studio layout (ignored by classic): a top-level
`copyright` line shown above the total, and a per-item `body` paragraph shown
in grey italics under the item description. Both are optional — leave the
field out entirely (or set `null`); older config files parse unchanged.

### Fonts

`--font-dir <dir>` overrides the built-in fonts with files from a directory.
A file claims a slot when its name *ends* with the slot name (separated by
`-`, `_` or `.`), so the rest of the name is free to identify the font for
humans, e.g. `Lato-Regular.sans.ttf` or `PTSerif-Bold.serif-bold.ttf`. Slots:
`sans`, `sans-bold`, `sans-italic`, `serif`, `serif-bold`, `serif-italic`,
`monospace` (`.ttf` or `.otf`); unmatched slots keep the built-in font, two
files matching one slot is an error, and `--monospace-font` still wins for
the monospace slot. To reproduce the studio design faithfully, fetch from
Google Fonts and suffix:

| file | font |
| --- | --- |
| `PTSerif-Regular.serif.ttf` | PT Serif Regular |
| `PTSerif-Bold.serif-bold.ttf` | PT Serif Bold |
| `PTSerif-Italic.serif-italic.ttf` | PT Serif Italic |
| `Lato-Regular.sans.ttf` | Lato Regular |
| `Lato-Bold.sans-bold.ttf` | Lato Bold |
| `Lato-Italic.sans-italic.ttf` | Lato Italic |

> **Try out SIP-72 dedented multiline string literals:**
> ```bash
> ./invoicer.sh --experimental resources/sampleConfigSip72.scala
> ```
> **Try out collection literal syntax:**
> ```bash
> ./invoicer.sh --experimental --literal-maps resources/sample-config-literals.scon
> ```

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
