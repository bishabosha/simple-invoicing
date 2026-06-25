#!/usr/bin/env -S scala shebang
//> using scala 3.8.3
//> using platform native
//> using nativeVersion 0.5.11
//> using toolkit 0.9.2
//> using dep io.github.bishabosha::scala-object-notation::0.4.1
//> using dep io.github.cquiroz::scala-java-time::2.6.0
//> using files ${.}/../Configs.scala ${.}/../Logger.scala ${.}/../IsValid.scala

val configPath =
  args.lift(0).getOrElse(throw new IllegalArgumentException("No config file provided"))
val experimental = args.lift(1).contains("--experimental")

val conf = configs.readConfig(os.Path(configPath, os.pwd), experimental)
val _ = validateConfig(conf)
Logger.info(s"Config loaded successfully from $configPath")
