package configs

import scala.NamedTuple.AnyNamedTuple
import steps.result.Result
import scala.collection.immutable.SeqMap

type ItemSchema = (desc: String, body: Option[String], qty: Double, price: Int)

type SectionSchema = (
    title: String,
    desc: String,
    itemsTitle: String,
    items: SeqMap[String, String]
)

type AppendixSchema = (
    title: String,
    description: String,
    sections: Vector[SectionSchema]
)

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
        items: Vector[ItemSchema],
        taxRate: Int,
        useHours: Boolean
    ),
    business: String,
    copyright: Option[String],
    currency: (code: String, symbol: String, left: Boolean),
    bank: SeqMap[String, String],
    appendices: Vector[AppendixSchema]
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

  // the unused-lint cannot see uses from inside the inline derivation below
  import scala.annotation.nowarn
  given optStringReader: scalanotation.Reader[Option[String]] =
    scalanotation.Reader.OptionSchema[String]
  @nowarn("msg=unused local definition")
  given itemReader: scalanotation.Reader[ItemSchema] = scalanotation.Reader.derived
  @nowarn("msg=unused local definition")
  given itemsReader: scalanotation.Reader[Vector[ItemSchema]] =
    scalanotation.Reader.VectorSchema[ItemSchema]
  @nowarn("msg=unused local definition")
  given sectionReader: scalanotation.Reader[SectionSchema] = scalanotation.Reader.derived
  @nowarn("msg=unused local definition")
  given sectionsReader: scalanotation.Reader[Vector[SectionSchema]] =
    scalanotation.Reader.VectorSchema[SectionSchema]
  @nowarn("msg=unused local definition")
  given appendixReader: scalanotation.Reader[AppendixSchema] = scalanotation.Reader.derived
  @nowarn("msg=unused local definition")
  given appendicesReader: scalanotation.Reader[Vector[AppendixSchema]] =
    scalanotation.Reader.VectorSchema[AppendixSchema]

  // fields introduced after the original schema default to None when omitted,
  // so older config files keep parsing
  given scalanotation.DefaultValues[InvoiceSchema] = scalanotation.DefaultValues.of { c =>
    Seq(
      c.copyright := None,
      c.listings.items.each.body := None
    )
  }
  given scalanotation.Configured[InvoiceSchema] =
    scalanotation.Configured.default.withDefaultValues
  given scalanotation.Reader[InvoiceSchema] = scalanotation.Reader.configured.derived

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
