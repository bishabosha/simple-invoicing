//> using scala 3.nightly
//> using options -experimental
// Define the invoice details
import language.experimental.dedentedStringLiterals
(
  invoice = (
    id = 1, // ticker per client
    period = (
      start = '''
      2026/1/31
      ''', // year/month/day
      days = 30
    )
  ),
  client = (
    id = 3, // global client ID
    name = '''
      ACME corp.
      ''',
    address = '''
      Example street, London, United Kingdom
      ''',
    contactPerson = '''
      John Snow
      '''
  ),
  listings = (
    items = Vector(
      (
        desc = '''
          simplified description e.g. SKU, or reference to service in appendix
          ''',
        qty = 15, // raw numbers of the 'desc'
        price = 75_00 // in the smallest denomination (only decimal currencies supported)
      ),
      (
        desc = '''
          fractional orders are supported
          ''',
        qty = 7.5,
        price = 15
      )
    ),
    taxRate = 0, // if Value Added Tax equivalent is needed [0-100]
    useHours = true // changes label of quantity to hours
  ),
  business = (
    name = '''
      Lisa Simpson
      ''',
    address = '''
      60 Old Kent Road, London, SE1, United Kingdom
      ''',
    contact = '''
      example@example.com
      '''
  ),
  currency = (code = "EUR", symbol = "€", left = true),
  bank = (
    holder = '''
      President Business
      ''',
    name = '''
      UBS Switzerland AG
      ''',
    address = '''
      The bank's address
      ''',
    userAddress = null, // or "51 Main St, USA",
    account = '''
      CHXX 0000 0000 0000 0000 X
      ''', // IBAN
    swift = '''
      UBSXXXXX99X
      ''',
    intermediary = null, // or a swift code
    routing = null // or "000000000"
  ),
  twint = null, // Swiss personal payment service
  appendices = Vector(
    (
      title = '''
        Work Packages
        ''',
      description = '''
        Descriptions from the associated agreement with <Client>
        ''',
      sections = Vector(
        (
          title = '''
            Work Package X: ...
            ''',
          desc = "",
          itemsTitle = '''
            Tasks
            ''',
          items = (
            `X.1 ...` = '''
              This work package focuses on ...
              text split over multiple lines
                - indendation is respected
                - have fun!
              ''',
            `X.2 ...` = '''
              another work package
              '''
          )
        )
      )
    )
  )
)
