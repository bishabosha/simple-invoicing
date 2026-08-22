// French-language sample config for the studio layout; all details are
// fictional placeholders — replace them with your own.
// Render with:
//   ./invoicer.sh --layout studio --lang fr \
//     --logo resources/images/logo.png \
//     --paper-texture resources/images/paper-page.png \
//     --accent-texture resources/images/paper-accent.png \
//     resources/sampleConfigStudio.scala
val Invoice = (
  invoice = (
    id = 35,
    period = (
      start = "2026/7/20", // year/month/day
      days = 30
    )
  ),
  client = (
    id = 0,
    name = "Example Media SA",
    address = "Rue de l'Exemple 10,\n1006 Lausanne",
    contactPerson = null
  ),
  listings = (
    items = Vector(
      (
        desc = "Editorial Licence - use of three photographs",
        body = "non-exclusive Licence for utilisation of three photographs" +
          " in ...",
        qty = 3,
        price = 75_00 // centimes
      )
    ),
    taxRate = 0,
    useHours = false
  ),
  business = "Lisa Simpson,\n" +
    "60 Old Kent Road,\n" +
    "London, SE1,\n" +
    "United Kingdom,\n" +
    "example@example.com",
  copyright = "Credit photo : © Lisa Simpson",
  currency = (code = "CHF", symbol = "CHF", left = false),
  bank = (
    `Beneficiary` = "President Business",
    `IBAN` = "CHXX 0000 0000 0000 0000 X",
    `Recipient SWIFT/BIC` = "UBSXXXXX99X",
    `Message for payee` = "$INVOICE_NO",
    `Bank Name and Address` = "UBS Switzerland AG,\n" +
      "25 Rue de Geneve,\n" +
      "1003 Lausanne,\n" +
      "Switzerland"
  ),
  appendices = Vector()
)
