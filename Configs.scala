package configs

import scala.NamedTuple.AnyNamedTuple
import scala.collection.immutable.SeqMap
import scalanotation.{Configured, DefaultValues, Reader, Readers}
import steps.result.Result
import scalanotation.schema.RawSchema

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
        items: Vector[(desc: String, body: Option[String], qty: Double, price: Int)],
        taxRate: Int,
        useHours: Boolean
    ),
    business: String,
    copyright: Option[String],
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

def defaultReader[V: Reader]: Reader[SeqMap[String, V]] =
  summon[Reader[SeqMap[String, V]]]

def readConfig(
    path: os.Path,
    experimental: Boolean = false,
    literalMaps: Boolean = false
): InvoiceSchema =
  given [V: Reader] => Reader[SeqMap[String, V]] =
    if literalMaps then Reader.pairSeqAsDict
    else defaultReader

  given Reader[InvoiceSchema] = invoiceReader

  readConfigVia(path)(str =>
    if experimental then
      Readers.experimental.readDeclAs[InvoiceSchema](str, rootName = "Invoice")
    else Readers.readDeclAs[InvoiceSchema](str, rootName = "Invoice")
  )

/** The standard implicit reader, with decode-time defaults installed so that
  * config files predating a field keep parsing.
  *
  * `Reader.configured.derived` would re-derive the schema through `Mirror.Of`,
  * which cannot resolve this schema's container fields without a pile of
  * hand-written component instances. The defaults transform itself is
  * schema-agnostic, so instead apply it to the schema of the implicit reader —
  * reflectively, until the library grows a public `withConfig`
  * (`Configured.applyToSchema` is `private[scalanotation]`, which is public in
  * bytecode).
  */
private def invoiceReader(using Reader[SeqMap[String, String]]): Reader[InvoiceSchema] =
  val defaults = DefaultValues.of[InvoiceSchema] { c =>
    Seq(
      c.copyright := None,
      c.listings.items.each.body := None
    )
  }
  val config = Configured.default[InvoiceSchema].withDefaultValues(using defaults)
  val plain = summon[Reader[InvoiceSchema]]
  applyConfiguredToSchema(plain, config)

/** ugly hack to reflectively run Configured.applyToSchema - it should be exposed publicly */
private def applyConfiguredToSchema[T](plain: Reader[T], config: Configured[T]): Reader[T] = {
  import scala.reflect.Selectable.reflectiveSelectable
  val ConfiguredRefl = Configured.asInstanceOf[{
    def applyToSchema(schema: RawSchema[?], config: Configured[?]): RawSchema[?]
  }]
  val ReaderRefl = Reader.asInstanceOf[{
    def fromSchema(schema: RawSchema[?]): Reader[?]
  }]
  val patched = ConfiguredRefl.applyToSchema(plain.schema, config)
  val reader = ReaderRefl.fromSchema(patched)
  reader.asInstanceOf[Reader[T]]
}

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
