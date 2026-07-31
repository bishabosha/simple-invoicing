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
    literalMaps: Boolean = false
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

  Logger.info(s"Begin - config file: ${configPath.toString}")
  Logger.info(s"Output file: ${output.toString}")
  monospaceFont.foreach(fontPath => Logger.info(s"Monospace font file: ${fontPath.toString}"))
  val conf = configs.readConfig(configPath, experimental, literalMaps)
  Logger.info("parsed config")

  val validConf = validateConfig(conf)

  val invoiceDocument = InvoiceMarkup(conf, validConf.issueDate).build
  Logger.info("Built markup.")

  PdfRenderer.render(output, monospaceFont, invoiceDocument)

  Logger.info("Invoice created successfully.")
}

@main def main(args: String*): Unit = Flagged.parseOrExit[this.type](args)
