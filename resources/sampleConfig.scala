// Define the invoice details
// NOTE: clone and rename as `conf_<title>.sc` to ignore from Git.
val Invoice = (
  invoice = (
    id = 1, // ticker per client
    period = (
      start = "2026/1/31", // year/month/day
      days = 30
    )
  ),
  client = (
    id = 3, // global client ID
    name = "ACME corp.",
    address = "Example street, London, United Kingdom",
    contactPerson = "John Snow" // or null
  ),
  listings = (
    items = Vector(
      (
        desc = "simplified description e.g. SKU, or reference to service in appendix",
        qty = 15, // raw numbers of the 'desc'
        price = 75_00 // in the smallest denomination (only decimal currencies supported)
      ),
      (
        desc = "fractional orders are supported",
        qty = 7.5,
        price = 15
      )
    ),
    taxRate = 0, // if Value Added Tax equivalent is needed [0-100]
    useHours = true // changes label of quantity to hours
  ),
  business = "Lisa Simpson,\n" +
    "60 Old Kent Road,\n" +
    "London, SE1,\n" +
    "United Kingdom,\n" +
    "example@example.com",
  currency = (code = "EUR", symbol = "€", left = true),
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
  appendices = Vector(
    (
      title = "Work Packages",
      description = "Descriptions from the associated agreement with <Client>",
      sections = Vector(
        (
          title = "Work Package X: ...",
          desc = "",
          itemsTitle = "Tasks",
          items = (
            `X.1 ...` = "This work package focuses on ... \n" +
              "text split over multiple lines \n" +
              "  - indendation is respected\n" +
              "  - have fun!",
            `X.2 ...` = "another work package"
          )
        )
        // (
        //   title = "Work Package A: ...",
        //   desc = "",
        //   itemsTitle = "Tasks",
        //   items = (
        //     `A.1 ...` =
        //       "placeholder for recurring descriptions you could comment out."
        //   )
        // )
      )
    )
  )
)
