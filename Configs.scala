package configs

import scala.NamedTuple.AnyNamedTuple
import steps.result.Result
import scala.collection.immutable.SeqMap

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
        items: Vector[(desc: String, qty: Double, price: Int)],
        taxRate: Int,
        useHours: Boolean
    ),
    business: String,
    currency: (code: String, symbol: String, left: Boolean),
    bank: SeqMap[String, String],
    appendices: Vector[
      (
          title: String,
          description: String,
          sections: Vector[
            (
                title: String,
                desc: String,
                itemsTitle: String,
                items: SeqMap[String, String]
            )
          ]
      )
    ]
)

type Search[Key <: String, Keys <: Tuple, Values <: Tuple] =
  (Keys, Values) match
    case (Key *: _, v *: _) => v
    case (_ *: ks, _ *: vs) => Search[Key, ks, vs]

type SelectField[NT <: AnyNamedTuple, F <: String] = NT match
  case NamedTuple.NamedTuple[ns, ts] =>
    Search[F, ns, ts]

type SelectElem[F <: scala.collection.Seq[?]] = F match
  case scala.collection.Seq[e] => e

def defaultReader[V: scalanotation.Reader]: scalanotation.Reader[SeqMap[String, V]] =
  summon[scalanotation.Reader[SeqMap[String, V]]]

def readConfig(
    path: os.Path,
    experimental: Boolean = false,
    literalMaps: Boolean = false
): InvoiceSchema =
  given [V: scalanotation.Reader] => scalanotation.Reader[SeqMap[String, V]] =
    if literalMaps then scalanotation.Reader.pairSeqAsDict
    else defaultReader

  readConfigVia(path)(str =>
    if experimental then
      scalanotation.Readers.experimental.readDeclAs[InvoiceSchema](str, rootName = "Invoice")
    else scalanotation.Readers.readDeclAs[InvoiceSchema](str, rootName = "Invoice")
  )

private def readConfigVia(path: os.Path)(
    read: String => Result[InvoiceSchema, scalanotation.DecodeError]
): InvoiceSchema =
  val text = os.read(path)
  read(text) match
    case Result.Ok(value) => value
    case Result.Err(error) =>
      sys.error(
        s"failed to read config from ${path.toString}:${error.format}"
      )
