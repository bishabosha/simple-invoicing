import java.time.LocalDate
import java.time.format.DateTimeParseException
import java.time.format.DateTimeFormatter

def fail(msg: String): Nothing =
  Logger.error(msg)
  sys.exit(1)

def requireValid(condition: Boolean, message: => String): Unit =
  if !condition then fail(message)

def parsePath(rawPath: String, label: String): os.Path =
  try os.Path(rawPath, os.pwd)
  catch
    case _: IllegalArgumentException =>
      fail(s"invalid ${label} path: ${rawPath}")

def parseInvoiceDate(dateText: String): LocalDate =
  try LocalDate.parse(dateText, DateTimeFormatter.ofPattern("yyyy/M/d"))
  catch
    case _: DateTimeParseException =>
      fail(
        s"invalid invoice.period.start '${dateText}', expected format yyyy/m/d"
      )

def validateConfig(conf: configs.InvoiceSchema): (issueDate: LocalDate) =
  requireValid(conf.client.id >= 0, "client.id must be non-negative")
  requireValid(conf.invoice.id >= 0, "invoice.id must be non-negative")
  requireValid(
    conf.invoice.period.days > 0,
    "invoice.period.days must be positive"
  )
  requireValid(
    conf.listings.taxRate >= 0 && conf.listings.taxRate <= 100,
    "listings.taxRate must be between 0 and 100"
  )
  requireValid(
    conf.listings.items.nonEmpty,
    "listings.items must not be empty"
  )
  for (item, idx) <- conf.listings.items.iterator.zipWithIndex do
    requireValid(item.qty > 0, s"listings.items[${idx}].qty must be positive")
    requireValid(item.price >= 0, s"listings.items[${idx}].price must be non-negative")
  (issueDate = parseInvoiceDate(conf.invoice.period.start))
