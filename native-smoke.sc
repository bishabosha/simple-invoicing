#! /usr/bin/env scala shebang
//> using scala 3.8.2
//> using platform scala-native
//> using nativeMode release-fast
//> using nativeLinking -L/opt/homebrew/lib -Wl,-rpath,/opt/homebrew/lib
//> using files Layout.scala CairoPdf.scala NativePdfRenderer.scala Logger.scala
//> using options -Wall -Werror

import Html.*
import Style.*
import FontFamily.*
import FontWeight.*
import TextAlign.*
import WhiteSpace.*

val outputPath = args.headOption.getOrElse("NativeInvoice.pdf")

val heading = Style(fontWeight = Bold, fontSize = 16, lineHeight = 20)
val label = Style(fontWeight = Bold)
val body = Style()

val doc =
  document(
    page()(
      p(heading.copy(marginBottom = 1.lh))("INVOICE"),
      p(body)(
        "My Company",
        "60 Old Kent Road, London, SE1, United Kingdom",
        "example@example.com"
      ),
      div(
        style = Style(width = Some(500.px), textAlign = Right, marginTop = 1.lh, gap = 1.lh)
      )(
        p(label)("Invoice No: INV30001"),
        p(body)("Issue Date: 31 Jan 2026", "Due Date: 02 Mar 2026")
      ),
      table(label, body, style = Style(marginTop = 1.lh, marginBottom = 1.lh))(
        th("Description", Style(width = Some(315.px), whiteSpace = Wrap)),
        th("Hours", Style(width = Some(50.px))),
        th("Unit Price", Style(width = Some(60.px))),
        th("Total", Style(width = Some(75.px)))
      )(
        tr(td("Cairo-backed Scala Native renderer"), td("15"), td("75,00"), td("1125,00")),
        tr(td("Fractional work order"), td("7,5"), td("0,15"), td("1,13"))
      ),
      row(Style(width = Some(500.px), marginTop = 1.lh))(
        span(label.copy(marginLeft = 320.px))("Total Amount Due:"),
        span(body.copy(marginLeft = 415.px))("€ 1126,13 (EUR)")
      ),
      hr(style = Style(marginTop = 1.lh, marginBottom = 1.lh)),
      p(body.copy(fontFamily = Monospace))(
        "IBAN: CHXX 0000 0000 0000 0000 X",
        "Message for payee: INV30001"
      )
    )
  )

NativePdfRenderer.render(outputPath, doc)
Logger.info(s"Native PDF written to ${outputPath}")
