package configs

import steps.result.Result

type InvoiceSchema = (
    invoice: (
        id: Int,
        period: (start: String, days: Int)
    ),
    client: (
        id: Int,
        name: String,
        address: String,
        contactPerson: Option[String]
    ),
    listings: (
        items: Vector[(desc: String, qty: Int, price: Int)],
        taxRate: Int,
        useHours: Boolean
    ),
    business: (
        name: String,
        address: String,
        contact: String
    ),
    currency: (code: String, symbol: String, left: Boolean),
    bank: (
        holder: String,
        name: String,
        address: String,
        userAddress: Option[String],
        account: String,
        swift: String,
        intermediary: Option[String],
        routing: Option[String]
    ),
    twint: Option[String],
    appendices: Vector[
      (
          title: String,
          description: String,
          sections: Vector[
            (
                title: String,
                desc: String,
                itemsTitle: String,
                items: Vector[(id: String, desc: String)]
            )
          ]
      )
    ]
)

def readConfig(resource: os.RelPath): InvoiceSchema =
  val text = os.read(os.pwd / resource)
  scalanotation.Parser.parseValueAs[InvoiceSchema](text, name = "conf") match
    case Result.Ok(value) => value
    case Result.Err(error) =>
      sys.error(
        s"failed to read config from ${resource}:${error.format}"
      )
