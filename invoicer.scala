import flagged.*

@run
@version("1.0.1")
@name("./invoicer.sh")
@help("generate an invoice from a config file")
def invoicer(
    @help("Use a custom monospace font file for bank details fields")
    monospaceFont: Option[os.Path] = None,
    @help("Write the PDF to a custom path")
    output: os.Path = os.pwd / "Invoice.pdf",
    @help("the config file to read from") @positional
    configPath: os.Path,
    @help("Use the experimental config reader")
    experimental: Boolean = false,
    @help("Use literal maps for config parsing")
    literalMaps: Boolean = false,
    @help("Invoice layout: classic or studio")
    layout: String = "classic",
    @help("Logo image for the studio layout footer")
    logo: Option[os.Path] = None,
    @help("Paper texture image drawn behind studio layout pages")
    paperTexture: Option[os.Path] = None,
    @help("Texture image for the studio layout footer band")
    accentTexture: Option[os.Path] = None,
    @help(
      "Directory of font files overriding the built-in fonts. Matched on the" +
        " file name suffix: sans, sans-bold, sans-italic, serif, serif-bold," +
        " serif-italic, monospace (.ttf or .otf), so the name can keep the" +
        " font identity, e.g. Lato-Regular.sans.ttf or PTSerif-Bold.serif-bold.ttf"
    )
    fontDir: Option[os.Path] = None
): Unit = {
  if (literalMaps && !experimental) {
    Logger.error("--literal-maps are not supported without --experimental")
    return
  }
  requireValid(os.isFile(configPath), s"config file not found: ${configPath.toString}")
  requireValid(
    !os.isDir(output),
    s"output path points to a directory: ${output.toString}"
  )
  monospaceFont.foreach { fontPath =>
    requireValid(
      os.isFile(fontPath),
      s"monospace font file not found: ${fontPath.toString}"
    )
  }
  requireValid(
    layout == "classic" || layout == "studio",
    s"unknown layout '${layout}', expected one of: classic, studio"
  )
  fontDir.foreach { dir =>
    requireValid(os.isDir(dir), s"font directory not found: ${dir.toString}")
  }
  for
    (flag, path) <- Seq(
      "--logo" -> logo,
      "--paper-texture" -> paperTexture,
      "--accent-texture" -> accentTexture
    )
    imagePath <- path
  do
    requireValid(
      os.isFile(imagePath),
      s"$flag image file not found: ${imagePath.toString}"
    )

  Logger.info(s"Begin - config file: ${configPath.toString}")
  Logger.info(s"Output file: ${output.toString}")

  val fontFiles: Map[FontRef, os.Path] = {
    val fontSlots = Seq(
      "sans" -> FontRef.Helvetica,
      "sans-bold" -> FontRef.HelveticaBold,
      "sans-italic" -> FontRef.HelveticaOblique,
      "serif" -> FontRef.TimesRoman,
      "serif-bold" -> FontRef.TimesBold,
      "serif-italic" -> FontRef.TimesItalic,
      "monospace" -> FontRef.Monospace
    )
    val separators = Seq('-', '_', '.')
    val fromDir =
      for
        dir <- fontDir.toSeq
        candidates = os
          .list(dir)
          .filter(p => os.isFile(p) && Seq("ttf", "otf").contains(p.ext.toLowerCase))
        (slot, ref) <- fontSlots
        path <- {
          val matches = candidates.filter { path =>
            val base = path.baseName.toLowerCase
            base == slot || separators.exists(sep => base.endsWith(s"${sep}${slot}"))
          }
          requireValid(
            matches.sizeIs <= 1,
            s"ambiguous fonts for '${slot}' in ${dir.toString}: " +
              matches.map(_.last).mkString(", ")
          )
          matches.headOption
        }
      yield ref -> path
    // an explicit --monospace-font wins over the font directory
    fromDir.toMap ++ monospaceFont.map(FontRef.Monospace -> _)
  }
  fontFiles.foreach((ref, path) => Logger.info(s"Font ${ref.toString}: ${path.toString}"))
  val conf = configs.readConfig(configPath, experimental, literalMaps)
  Logger.info("parsed config")

  val validConf = validateConfig(conf)

  val invoiceDocument =
    if layout == "studio" then
      StudioInvoiceMarkup(
        conf,
        validConf.issueDate,
        StudioAssets(logo, paperTexture, accentTexture),
        StudioLabels.english
      ).build
    else InvoiceMarkup(conf, validConf.issueDate).build
  Logger.info(s"Built markup (${layout} layout).")

  PdfRenderer.render(output, fontFiles, invoiceDocument)

  Logger.info("Invoice created successfully.")
}

@main def main(args: String*): Unit = Flagged.parseOrExit[this.type](args)
